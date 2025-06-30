import Foundation

enum PEGRule<AST>: CustomDebugStringConvertible {
    typealias r  = [PEGRule]
    typealias t = ([AST])->[AST]

    case sequence(r, t)
    case opt(r, t)
    case zeroMore(r, t)
    case onePlus(r, t)
    case choice(r, t)
    case lookahead(r, t)
    case forbid(r, t)

    case terminal(Regex<Substring>, (String)->AST)
    case ref(String)
    case transform(t)

    var debugDescription: String {
        switch self {
        case .sequence(let r, _): return r.map(\.debugDescription).joined(separator: " ")
        case .opt(let r, _): return "(\(r.map(\.debugDescription).joined(separator: " ")))?"
        case .zeroMore(let r, _): return "\(r)*"
        case .onePlus(let r, _): return "\(r)+"
        case .choice(let r, _): return r.map(\.debugDescription).joined(separator: "/")
        case .lookahead(let r, _): return "&\(r)"
        case .forbid(let r, _): return "!\(r)"

        case .terminal(let r, _): return "(terminal \(r))"
        case .ref(let r): return "[ref \(r)]"
        case .transform(_): return "[transform]"
        }
    }
}

struct PEGResult<AST> {
    var matched: Bool
    var ast: [AST]?
    var rest: Substring
    init(_ matched: Bool, _ ast: [AST]?, _ rest: Substring) {
        self.rest = rest
        self.matched = matched
        self.ast = ast
    }
}

class PEGParser<AST> {
    var lib: [String: PEGRule<AST>]
    struct MemoKey: Hashable {
        var rule: String
        var pos: String.Index
    }
    var memo: [MemoKey : PEGResult<AST>]

    init(_ lib: [String : PEGRule<AST>]) {
        self.lib = lib
        self.memo = [:]
    }

    func parse(_ s: String, _ r: String) -> PEGResult<AST> {
        memo = [:]
        return parseRule(s[...], lib[r]!)
    }

    func parseRule(_ s: Substring, _ rule: PEGRule<AST>) -> PEGResult<AST> {
        let failed = PEGResult<AST>(false, nil, s)
        let accepted = PEGResult<AST>(true, [], s)
        if case .ref(let n) = rule {
            if let r = lib[n] {
                let key = MemoKey(rule: n, pos: s.startIndex)
                if memo[key] == nil { memo[key] = parseRule(s, r) }
                return memo[key]!
            } else { return failed }
        }

        var src = s.trimmingPrefix(while: \.isWhitespace)
        var rs: [AST] = []

        switch rule {
        case .sequence(let xs, let c):
            for x in xs {
                if case .transform(let tx) = x { rs = tx(rs) }
                else {
                    let m = parseRule(src, x)
                    if m.matched { rs.append(contentsOf: m.ast!); src = m.rest } else { return failed }
                }
            }
            return PEGResult(true, c(rs), src)

        case .opt(let xs, let c):
            for x in xs {
                let m = parseRule(src, x)
                if m.matched { rs.append(contentsOf: m.ast!); src = m.rest } else { return accepted }
            }
            return PEGResult(true, c(rs), src)

        case .zeroMore(let xs, let c):
            while true {
                var src0 = src
                var rs0: [AST] = []
                for x in xs {
                    let m = parseRule(src0, x)
                    if m.matched { rs0.append(contentsOf: m.ast!); src0 = m.rest }
                    else { if rs.count > 0 { return PEGResult(true, c(rs), src) } else { return accepted } }
                }
                src = src0
                rs.append(contentsOf: rs0)
            }

        case .onePlus(let xs, let c):
            while true {
                var src0 = src
                var rs0: [AST] = []
                for x in xs {
                    let m = parseRule(src0, x)
                    if m.matched { rs0.append(contentsOf: m.ast!); src0 = m.rest }
                    else { if rs.count > 0 { return PEGResult(true, c(rs), src) } else { return failed } }
                }
                src = src0
                rs.append(contentsOf: rs0)
            }

        case .choice(let xs, let c):
            for x in xs {
                let m = parseRule(src, x)
                if m.matched { return PEGResult(true, c(m.ast!), m.rest) }
            }
            return failed

        case .lookahead(let xs, let c):
            for x in xs {
                let m = parseRule(src, x)
                if m.matched { rs.append(contentsOf: m.ast!); src = m.rest } else { return failed }
            }
            return PEGResult(true, c(rs), src)

        case .forbid(let xs, let c):
            for x in xs {
                let m = parseRule(src, x)
                if m.matched { rs.append(contentsOf: m.ast!); src = m.rest } else { return PEGResult(true, c(rs), src) }
            }
            return failed

        case .terminal(let x, let c):
            if let m = src.prefixMatch(of: x) { return PEGResult(true, [c(String(src[m.range]))], src[m.endIndex...]) }
            else { return failed }

        case .transform(_):  // unreachable
            fallthrough
        case .ref(_):    // unreachable
            return failed
        }
    }
}
