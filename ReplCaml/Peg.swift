import Foundation

enum PEGRule<AST>: CustomDebugStringConvertible {
    typealias R  = [PEGRule]
    typealias T = ([AST])->[AST]

    case sequence(R, T)
    case opt(R, T)
    case zeroMore(R, T)
    case onePlus(R, T)
    case choice(R, T)
    case lookahead(R)
    case forbid(R)

    case terminal(Regex<Substring>, (String)->AST)
    case ref(String)
    case transform(T)

    var debugDescription: String {
        switch self {
        case .sequence(let r, _): let s = r.map(\.debugDescription).joined(separator: " "); return r.count > 1 ? "(\(s))" : s
        case .opt(let r, _): let s = r.map(\.debugDescription).joined(separator: " "); return r.count > 1 ? "(\(s))?" : "\(s)?"
        case .zeroMore(let r, _): let s = r.map(\.debugDescription).joined(separator: " "); return r.count > 1 ? "(\(s))*" : "\(s)*"
        case .onePlus(let r, _): let s = r.map(\.debugDescription).joined(separator: " "); return r.count > 1 ? "(\(s))+" : "\(s)+"
        case .choice(let r, _): let s = r.map(\.debugDescription).joined(separator: " / "); return r.count > 1 ? "(\(s))" : s
        case .lookahead(let r): let s = r.map(\.debugDescription).joined(separator: " "); return r.count > 1 ? "&(\(s))" : "&\(s)"
        case .forbid(let r): let s = r.map(\.debugDescription).joined(separator: " "); return r.count > 1 ? "!(\(s))" : "!\(s)"

        case .terminal(let r, _): return "(terminal \(r))"
        case .ref(let r): return "[ref \(r)]"
        case .transform(_): return "[transform]"
        }
    }
}

struct PEGResult<AST>: Equatable, CustomDebugStringConvertible where AST: Equatable {
    static func == (l: PEGResult<AST>, r: PEGResult<AST>) -> Bool {
        l.matched == r.matched &&
        l.rest == r.rest &&
        l.errorMessage == r.errorMessage &&
        (l.ast == nil && r.ast == nil || l.ast!.elementsEqual(r.ast!))
    }

    var matched: Bool
    var ast: [AST]?
    var rest: Substring
    var errorMessage: String?

    init(_ matched: Bool, _ ast: [AST]?, _ rest: Substring, _ errorMessage: String? = nil) {
        self.rest = rest
        self.matched = matched
        self.ast = ast
        self.errorMessage = errorMessage
    }
    func with(message msg: String) -> PEGResult<AST> {
        return .init(matched, ast, rest, msg)
    }
    var debugDescription: String {
        return "\(matched), \(ast ?? []), \(rest)"
    }
}

class PEGParser<AST> where AST: Equatable {
    private var lib: [String: PEGRule<AST>]

    struct MemoKey: Hashable, CustomDebugStringConvertible {
        var rule: String
        var pos: String.Index
        var debugDescription: String { "\(pos):\(rule)" }
    }
    private var memo: [MemoKey : PEGResult<AST>]
#if DEBUG
    private var ruleStack: Set<MemoKey> = []
    private var level: Int = 0
#endif

    init(_ lib: [String : PEGRule<AST>]) {
        self.lib = lib
        self.memo = [:]
    }

    func parse(_ s: String, _ r: String) -> PEGResult<AST> {
#if DEBUG
        ruleStack = []
        level = 0
#endif
        memo = [:]
        let code = s[...]
        guard let rule = lib[r] else { return .init(false, nil, code, "undefined rule: \(r)") }
        let result = parseRule(code, rule)
        if result.matched { return result } else { return result.with(message: "Parse failed") }
    }

    private func parseRule(_ s: Substring, _ rule: PEGRule<AST>) -> PEGResult<AST> {
        let failed = PEGResult<AST>(false, nil, s)
        let accepted = PEGResult<AST>(true, [], s)

        if case .ref(let n) = rule {
            guard let r = lib[n] else { return failed.with(message: "undefined rule: \(n)") }
            let key = MemoKey(rule: n, pos: s.startIndex)
            if let cached = memo[key] { return cached }
#if DEBUG
            if ruleStack.contains(key) { return failed.with(message: "left recursion detected in rule: \(n)") }
            ruleStack.insert(key)
            level += 1
#endif
            let result = parseRule(s, r)
#if DEBUG
            level -= 1
            let filler = String(repeating: " ", count: level)
            print("\(filler) \(n): \(s.startIndex) \(result)")
            ruleStack.remove(key)
#endif
            memo[key] = result
            return result
        }

        var src = s.trimmingPrefix(while: \.isWhitespace)
        var rs: [AST] = []

        switch rule {
        case .sequence(let xs, let c):
            for x in xs {
                if case .transform(let tx) = x { rs = tx(rs) }
                else {
                    let m = parseRule(src, x)
                    if m.matched { rs.append(contentsOf: m.ast ?? []); src = m.rest } else { return failed }
                }
            }
            return PEGResult(true, c(rs), src)

        case .opt(let xs, let c):
            var matched = true
            var src0 = src
            var rs0: [AST] = []
            for x in xs {
                let m = parseRule(src0, x)
                if m.matched { rs0.append(contentsOf: m.ast ?? []); src0 = m.rest } else { matched = false; break }
            }
            if matched { src = src0; rs.append(contentsOf: rs0) }
            return PEGResult(true, c(rs), src)

        case .zeroMore(let xs, let c):
            var matched = true
            while matched {
                var src0 = src
                var rs0: [AST] = []
                for x in xs {
                    let m = parseRule(src0, x)
                    if m.matched { rs0.append(contentsOf: m.ast ?? []); src0 = m.rest } else { matched = false; break }
                }
                if matched { src = src0; rs.append(contentsOf: rs0) }
            }
            return PEGResult(true, c(rs), src)

        case .onePlus(let xs, let c):
            var matchedOnce = false
            var matched = true
            while matched {
                var src0 = src
                var rs0: [AST] = []
                for x in xs {
                    let m = parseRule(src0, x)
                    if m.matched { rs0.append(contentsOf: m.ast ?? []); src0 = m.rest } else { matched = false; break }
                }
                if matched { matchedOnce = true; src = src0; rs.append(contentsOf: rs0) }
            }
            if matchedOnce { return PEGResult(true, c(rs), src) } else { return failed }

        case .choice(let xs, let c):
            for x in xs {
                let m = parseRule(src, x)
                if m.matched { return PEGResult(true, c(m.ast ?? []), m.rest) }
            }
            return failed

        case .lookahead(let xs):
            var src0 = src
            for x in xs {
                let m = parseRule(src0, x)
                if m.matched { src0 = m.rest } else { return failed }
            }
            return accepted

        case .forbid(let xs):
            var src0 = src
            for x in xs {
                let m = parseRule(src0, x)
                if m.matched { src0 = m.rest } else { return accepted }
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
