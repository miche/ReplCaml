import Foundation
import Combine
import CxxStdlib

typealias Env<T> = [String: T]

struct Id: Equatable {
    typealias T = String
    nonisolated(unsafe) private static var counter = 0
    static func genid(_ s: String) -> T {
        counter += 1
        return "\(s).\(Id.counter)"
    }
    static func gentmp(_ t: Typ) -> T {
        counter += 1
        return ".\(id_of_typ(t))\(Id.counter)"
    }
    static func id_of_typ(_ t: Typ) -> String {
        switch t {
        case .UNIT: return "u"
        case .BOOL: return "b"
        case .INT: return "i"
        case .FLOAT: return "d"
        case .FUN: return "f"
        case .TUPLE: return "t"
        case .ARRAY: return "a"
        case .LIST: return "l"
        case .VAR: return "bug:Typ.rep(var)"
        }
    }
}

final class Opt: Equatable, CustomStringConvertible, Hashable {
    var v: Typ?
    init(_ v: Typ? = nil) {
        self.v = v
    }
    func update(_ v: Typ) {
        self.v = v
    }
    static func == (lhs: Opt, rhs: Opt) -> Bool {
        return lhs.v == rhs.v
    }
    var description: String { return v?.description ?? "#undef" }
    func hash(into hasher: inout Hasher) {
        hasher.combine(v ?? Typ.UNIT)
    }
}

struct TypS: CustomStringConvertible, Equatable { // type scheme
    var rs_ref: [Typ]
    var t: Typ

    init(_ rs_ref: [Typ], _ t: Typ) {
        self.rs_ref = rs_ref
        self.t = t
    }
//    func ch(_ c: Int) -> Character {
//        return Array("abcdefghijklmnopqrstuvwxyz")[c]
//    }
//    func new_var(_ r: Typ, _ counter: Int) -> String {
//        let s = "'\(ch(counter))"
//        env.append((r, s))
//        counter += 1
//        return s
//    }
    var description: String {
//        var env: [(Typ?, String)] = []
//        let a = UnicodeScalarType("a")
//        let c = Character(a)
        let xs = zip(rs_ref, Array("abcdefghijklmnopqrstuvwxyz")).map { t, c in (t, "'\(c)") }
//        env.append(contentsOf: xs)
        return "\(xs.map(\.1).joined(separator: ". "))\n  \(t)"
    }
}

indirect enum Typ: Equatable, CustomStringConvertible, Hashable {
    case UNIT
    case BOOL
    case INT
    case FLOAT
    case FUN([Typ], Typ, Typ, Typ)
    case TUPLE([Typ])
    case ARRAY(Typ)
    case LIST(Typ)
    case VAR(Opt)

    func map_var(_ f: (Typ?)->Typ, _ t: Typ) -> Typ {
        switch t {
        case .FUN(let t1s, let t2, let t3, let t4):
            return .FUN(t1s.map { map_var(f, $0) }, map_var(f, t2), map_var(f, t3), map_var(f, t4))
        case .TUPLE(let ts): return .TUPLE(ts.map { map_var(f, $0) })
        case .ARRAY(let t): return .ARRAY(map_var(f, t))
        case .LIST(let t): return .LIST(map_var(f, t))
        case .VAR(let v): if let t = v.v { return map_var(f, t) } else { return f(nil) }
        default: return t
        }
    }

    func subst(_ r: Typ, _ r2: Typ, _ t: Typ) -> Typ {
        self.map_var({ r0 in if r == r0 { return r2 } else { return .VAR(Opt(r0)) } }, t)
    }

    static func to_ts(_ t: Typ) -> TypS { TypS([], t) }

    static func gentyp() -> Typ { Typ.VAR(Opt(nil)) }

    var description: String {
        switch self {
        case .UNIT: return "unit"
        case .BOOL: return "bool"
        case .INT: return "int"
        case .FLOAT: return "float"
        case .FUN(let t1s, let t2, let t3, let t4):
            let fn = t1s.map(\.description).joined(separator: ", ")
            let s2 = t2.description, s3 = t3.description, s4 = t4.description
            if s2 == s4 { return "([\(fn)] -> \(s3)" }
            else { return "([\(fn)] / \(s2) -> \(s3) / \(s4)" }
        case .TUPLE(let ts):
            return "(\(ts.map(\.description).joined(separator: " * ")))"
        case .ARRAY(let t): return "\(t) array"
        case .LIST(let t): return "\(t) list"
        case .VAR(let r): if r.v != nil { return r.v!.description } else { return "'a"} //{ return new_var(r.v!) }
        }
    }
}

struct Ident: CustomStringConvertible, Equatable {
    var name: Id.T
    var typ: Typ
    var description: String { "(\(name) : \(typ))" }
    init(_ name: Id.T, _ typ: Typ) {
        self.name = name
        self.typ = typ
    }
    init(_ name: Id.T) {
        self.init(name, .gentyp())
    }
    init(_ name: Syntax) {
        self.init(name.to_s, .gentyp())
    }
    init() {
        self.init(Id.gentmp(.UNIT), .UNIT)
    }
}

struct IdentS: CustomStringConvertible, Equatable {
    var name: Id.T
    var typ: TypS
    init(_ name: Id.T, _ typ: TypS) {
        self.name = name
        self.typ = typ
    }
    init(_ name: Syntax) {
        self.init(name.to_s, TypS([], .UNIT))
    }
    var description: String { "(\(name) : \(typ))" }
}

indirect enum Syntax: CustomStringConvertible, Equatable {
    case UNIT
    case BOOL(Bool)
    case INT(Int)
    case FLOAT(Double)

    // case MOP(Id.T, Syntax)  // TODO: consider this approach
    case NOT(Syntax)
    case NEG(Syntax)
    case ADD(Syntax, Syntax)
    case SUB(Syntax, Syntax)
    case FNEG(Syntax)
    // case DOP(Id.T, Syntax, Syntax)  // TODO: consider this approach
    case FADD(Syntax, Syntax)
    case FSUB(Syntax, Syntax)
    case FMUL(Syntax, Syntax)
    case FDIV(Syntax, Syntax)
    case EQ(Syntax, Syntax)
    case LE(Syntax, Syntax)

    case IF(Syntax, Syntax, Syntax)
    case LET(Ident, Syntax, Syntax)
    case VAR(Id.T)
    case LETREC(Fundef, Syntax)
    case APP(Syntax, [Syntax])
    case TUPLE([Syntax])
    case LETTUPLE([Ident], Syntax, Syntax)
    case ARRAY(Syntax, Syntax)
    case GET(Syntax, Syntax)
    case PUT(Syntax, Syntax, Syntax)
    case EMPTY
    case CONS(Syntax, Syntax)
    case MATCH(Syntax, Syntax, Syntax)
    case SHIFT(IdentS, Syntax, Typ)
    case RESET(Syntax, Typ)

    // temporary containers
    case fundef(Fundef) // LETREC
    case pattern(Ident, Ident, Syntax) // MATCH
    case punct(String) // terminal
    case composite([Syntax]) // general use

    var description: String {
        switch self {
        case .UNIT: return "()"
        case .BOOL(let b): return b ? "true" : "false"
        case .INT(let i): return "\(i)"
        case .FLOAT(let f): return "\(f)"
        case .NOT(let e): return "not \(e)"
        case .NEG(let e), .FNEG(let e): return "-\(e)"
        case .ADD(let e1, let e2): return "(\(e1) + \(e2))"
        case .SUB(let e1, let e2): return "(\(e1) - \(e2))"
        case .FADD(let e1, let e2): return "(\(e1) +. \(e2))"
        case .FSUB(let e1, let e2): return "(\(e1) -. \(e2))"
        case .FMUL(let e1, let e2): return "(\(e1) *. \(e2))"
        case .FDIV(let e1, let e2): return "(\(e1) /. \(e2))"
        case .EQ(let e1, let e2): return "(\(e1) = \(e2))"
        case .LE(let e1, let e2): return "(\(e1) <= \(e2))"
        case .IF(let e1, let e2, let e3): return "if \(e1)\nthen \(e2) \nelse \(e3)"
        case .LET(let xt, let e1, let e2): return "let \(xt) = \(e1) in\n  \(e2)"
        case .VAR(let x): return x
        case .LETREC(let fd, let e2):
            let ats = fd.args.map(\.description).joined(separator: " ")
            return "let rec \(fd.name)\n  \(ats) =\n  \(fd.body) in\n  \(e2)"
        case .APP(let e, let es): return "(\(e) \(es.map(\.description).joined(separator: " ")))"
        case .TUPLE(let es): return "(\(es.map(\.description).joined(separator: ", ")))"
        case .LETTUPLE(let xts, let e1, let e2): return "let (\(xts.map(\.description).joined(separator: ", "))) = \(e1) in\n  \(e2)"
        case .ARRAY(let e1, let e2): return "Array.make \(e1) \(e2)"
        case .GET(let e1, let e2): return "\(e1).(\(e2))"
        case .PUT(let e1, let e2, let e3): return "\(e1).(\(e2)) <- \(e3)"
        case .EMPTY: return "[]"
        case .CONS(let e1, let e2): return "\(e1) :: \(e2)"  // TODO: loop [e1; e2] cons->list
        case .MATCH(let e1, let e2, let pat):
            return "match \(e1) with \n    [] -> \(e2)\n\(pat)"
        case .pattern(let xt, let yt, let e3): return "  | \(xt) :: \(yt) -> \(e3)"
        case .SHIFT(let xts, let e, _): return "shift (fun \(xts) -> \(e))"
        case .RESET(let e, _): return "reset (\(e))"

        case .punct(let s): return "*punct:\(s)*"
        case .composite(let xs): return xs.map(\.description).joined(separator: " ")
        case .fundef(_): return "*fundef*"
        }
    }

    var to_s: String { if case .punct(let s) = self { return s } else { fatalError() } }
    var to_a: [Syntax] { if case .composite(let xs) = self { return xs } else { fatalError() } }
    var to_fd: Fundef { if case .fundef(let fd) = self { return fd } else { fatalError() } }
    struct Fundef: Equatable {
        var name: IdentS
        var args: [Ident]
        var body: Syntax
        init(_ name: Syntax, _ args: Syntax, _ body: Syntax) {
            self.name = IdentS(name)
            self.args = args.to_a.map { Ident($0.to_s) }
            self.body = body
        }
    }
}

enum KNormal { case UNIT }

enum Closure { case UNIT }

enum Asm { case UNIT }

struct Pass {
    init() {}
    func typing(_ ast: Syntax) -> Syntax { return ast }
    func knormalize(_ ast: Syntax) -> KNormal { return .UNIT }
    func alphaConv(_ k: KNormal) -> KNormal { return k }
    func betaConv(_ k: KNormal) -> KNormal { return k }
    func reassoc(_ k: KNormal) -> KNormal { return k }
    func inlining(_ k: KNormal) -> KNormal { return k }
    func constFold(_ k: KNormal) -> KNormal { return k }
    func elimDeadCodes(_ k: KNormal) -> KNormal { return k }
    func closureConv(_ k: KNormal) -> Closure { return .UNIT }
    func virtualAsm(_ k: Closure) -> Asm { return .UNIT }
    func emit(_ k: Asm) -> String { return "emit" }
}

struct Parser {
    static func thru(_ ast: [Syntax]) -> [Syntax] { return ast }
    static func e1(_ ast: [Syntax]) -> [Syntax] { return [ast[0]] }
    static func e2(_ ast: [Syntax]) -> [Syntax] { return [ast[1]] }
    static func e3(_ ast: [Syntax]) -> [Syntax] { return [ast[2]] }
    static func e4(_ ast: [Syntax]) -> [Syntax] { return [ast[3]] }
    static func merge(_ a: [Syntax], _ b: [Syntax]) -> [Syntax] { return a + b }
    static func to_list(_ ast: [Syntax]) -> [Syntax] { var result = Syntax.EMPTY; for i in stride(from: ast.count - 1, through: 0, by: -2) { result = .CONS(ast[i], result) }; return [result]; }
    static func to_a(_ ast: [Syntax]) -> [Syntax] { [.composite(ast)] }
    static func to_a2(_ ast: [Syntax]) -> [Syntax] { var result = [ast.first!]; for i in stride(from: 2, to: ast.count, by: 2) { result.append(ast[i]) }; return result }
    static func addtyp(_ x: Syntax) -> Ident { Ident(x.to_s) }
    static func addtyp(_ xs: [Syntax]) -> [Ident] { xs.map(addtyp) }

    static func thruOr(_ f: @escaping ([Syntax])->Syntax) -> ([Syntax])->[Syntax] { return { ast in ast.count == 1 ? ast : [f(ast)]} }
    static func atop<A, B, C, Z>(_ g: @escaping (C)->Z, _ f: @escaping (A, B)->C) -> (A, B)->Z { return { a, w in g(f(a, w)) } }
    static func flip<A, B, Z>(_ f: @escaping (A, B)->Z) -> (B, A)->Z { return { a, w in f(w, a) } }
    static func jot<A, B, Z>(_ g: @escaping (B)->Z, _ f: @escaping (A)->B) -> (A)->Z { return { w in g(f(w)) } }
//    static func com<A, Z>(_ f: @escaping (A, A)->Z) -> (A)->Z { return { w in f(w, w) } }
//    static func over<A, B, Z>(_ g: @escaping (B, B)->Z, _ f: @escaping (A)->B) -> (A, A)->Z { return { a, w in g(f(a), f(w)) } }
//    static func before<A, B, C, Z>(_ g: @escaping (B, C)->Z, _ f: @escaping (A)->B) -> (A, C)->Z { return { a, w in g(f(a), w) } }
//    static func after<A, B, C, Z>(_ g: @escaping (A, C)->Z, _ f: @escaping (B)->C) -> (A, B)->Z { return { a, w in g(a, f(w)) } }
//    static func train<A, B, C, D, Z>(_ h: @escaping (D, C)->Z, _ g: @escaping (A, B)->D, _ f: @escaping (A, B)->C) -> (A, B)->Z { return { a, w in h(g(a, w), f(a, w)) } }
    static func train<A, B, Z>(_ h: @escaping (B, B)->Z, _ g: @escaping (A)->B, _ f: @escaping (A)->B) -> (A)->Z { return { w in h(g(w), f(w)) } }

    static private func opFold(_ ast: [Syntax], _ opMap: [String: (Syntax, Syntax) -> Syntax]) -> [Syntax] { var result = ast.last!; for i in stride(from: ast.count - 3, through: 0, by: -2) { result = opMap[ast[i + 1].to_s]!(ast[i], result) }; return [result]; }
    static private func letFold(_ ast: [Syntax]) -> Syntax { var result = ast.last!; for i in stride(from: ast.count - 3, through: 0, by: -2) { result = Syntax.LET(Ident(), ast[i], result) }; return result }

    static func term(_ r: Regex<Substring>) -> PEGRule<Syntax> { .terminal(r, { str in .punct(str) }) }
    static func seq(_ r: [PEGRule<Syntax>]) -> PEGRule<Syntax> { .sequence(r, { ast in ast }) }

    let lib: [String: PEGRule<Syntax>] = [
        "start": .ref("exp"),
        "exp": .ref("exp-let"),
        "exp-let": .choice([
            .sequence([term(/let\s+rec\b/), .ref("fundef"), term(/in\b/), .ref("exp")], { [.LETREC($0[1].to_fd, $0[3])] }),
            .sequence([term(/let\s*\(/), .ref("pat"), term(/\)\s*=/), .ref("exp"), term(/in\b/), .ref("exp")], { [.LETTUPLE(addtyp($0[1].to_a), $0[3], $0[5])] }),
            .sequence([term(/let\b/), .ref("ident"), term(/=/), .ref("exp"), term(/in\b/), .ref("exp")], { [.LET(addtyp($0[1]), $0[3], $0[5])] }),
            .sequence([term(/if\b/), .ref("exp-semicolon"), term(/then\b/), .ref("exp-semicolon"), term(/else\b/), .ref("exp-semicolon")], { [.IF($0[1], $0[3], $0[5])] }),
            .sequence([term(/shift\s*\(\s*fun\b/), .ref("ident"), term(/\-\>/), .ref("exp"), term(/\)/)], { [.SHIFT(IdentS($0[1]), $0[3], Typ.gentyp())] }),
            .sequence([term(/reset\s*\(\s*fun\s*\(\s*\)\s*\-\>/), .ref("exp"), term(/\)/)], { [.RESET($0[1], Typ.gentyp())] }),
            .sequence([term(/match\b/), .ref("exp"), term(/with\b/), .ref("match-clauses")], { [.MATCH($0[1], $0[3], $0[4])] }),
            .ref("exp-semicolon")], thru),
        "exp-semicolon": .sequence([.ref("exp-if"), .zeroMore([term(/\;/), .ref("exp-if")], thru)], thruOr(letFold)),
        "exp-if": .ref("exp-cons"),
        "exp-cons": .sequence([.ref("exp-assign"), .opt([term(/\:\:/), .ref("exp-cons")], thru)], thruOr({.CONS($0[0], $0[2])})),
        "exp-assign": .sequence([.ref("exp-comma"), .opt([term(/\<\-/), .ref("exp-comma")], thru)], thru),
        "exp-comma": .sequence([.ref("exp-equality"), .zeroMore([term(/\,/), .ref("exp-equality")], thru)], thruOr({.TUPLE(to_a2($0))})),
        "exp-equality": .sequence([.ref("exp-add"), .zeroMore([.choice([term(/\=\b/), .ref("exp-add"), term(/\<\>\b/), .ref("exp-add"), term(/\<\b/), .ref("exp-add"), term(/\>\b/), .ref("exp-add"), term(/\<\=\b/), .ref("exp-add"), term(/\>\=\b/), .ref("exp-add") ], thru)], thru)], { ast in opFold(ast, ["=": Syntax.EQ, "<>": atop(Syntax.NOT, Syntax.EQ), "<": atop(Syntax.NOT, flip(Syntax.LE)), ">": atop(Syntax.NOT, Syntax.LE), "<=": Syntax.LE, ">=": flip(Syntax.LE)]) }),
        "exp-add": .sequence([.ref("exp-mul"), .zeroMore([.choice([term(/\+/), .ref("exp-mul"), term(/\-/), .ref("exp-mul"), term(/\+\./), .ref("exp-mul"), term(/\-\./), .ref("exp-mul"), ], thru)], thru)], { ast in opFold(ast, ["+": Syntax.ADD, "-": Syntax.SUB, "+.": Syntax.FADD, "-.": Syntax.FSUB]) }),
        "exp-mul": .sequence([.ref("exp-unary"), .zeroMore([.choice([term(/\*\./), .ref("exp-unary"), term(/\/\./), .ref("exp-unary")], thru)], thru)], { ast in opFold(ast, ["*.": Syntax.FMUL, "/.": Syntax.FSUB]) }),
        "exp-unary": .choice([
            .sequence([term(/not\b/), .ref("exp-unary")], {[.NOT($0[1])]}),
            .sequence([term(/\-\b/), .ref("exp-unary")], {if case .FLOAT(let f) = $0[1] {return [.FLOAT(-f)]} else {return [.NEG($0[1])]}}),
            .sequence([term(/\-\.\b/), .ref("exp-unary")], {[.FNEG($0[1])]}),
            .ref("exp-app"),
        ], thru),
        "exp-app": .sequence([.ref("exp-dot"), .opt([.ref("actual_args")], thru)], thruOr {.APP($0[0], $0[1].to_a)}),
        "exp-dot": .sequence([.ref("simple_exp"), .zeroMore([.choice([
            .sequence([term(/\.\s*\(/), .ref("exp"), term(/\)\s*\<\-\b/), .ref("exp")], thru),
            .sequence([term(/\.\s*\(/), .ref("exp"), term(/\)/)], thru),
        ], thru)], thru)], thruOr({$0.count == 4 ? .GET($0[0], $0[2]) : .PUT($0[0], $0[2], $0[4]) })),
        "simple_exp": .choice([
            .sequence([term(/\(/), .ref("exp"), term(/\)/)], e2),
            .terminal(/\(\s*\)/, {_ in .UNIT }),
            .terminal(/\d+\.\d+/, { x in .FLOAT(Double(x)!) }),
            .terminal(/\d+/, { x in .INT(Int(x)!) }),
            .choice([.terminal(/true\b/, { _ in .BOOL(true) }), .terminal(/false\b/, { _ in .BOOL(false) })], thru),
            .sequence([term(/Array\s*.\s*create\b/), .ref("simple_exp"), .ref("simple_exp")], {[.ARRAY($0[1], $0[2])]}),
            .sequence([.ref("ident")], { [.VAR($0.first!.to_s)] }),
            .terminal(/\[\s*\]/, {_ in .EMPTY }),
            .sequence([term(/\[/), .ref("exp_list"), term(/]/)], e2),
        ], thru),
        "exp_list": .opt([.sequence([.ref("exp"), .zeroMore([term(/;/), .ref("exp")], thru)], thru)], to_list),
        "fundef": .sequence([.ref("ident"), .ref("formal_args"), term(/=/), .ref("exp")], { [.fundef(Syntax.Fundef($0[0], $0[1], $0[3]))] }),
        "formal_args": .onePlus([.ref("ident")], to_a),
        "actual_args": .onePlus([.ref("simple_exp")], to_a),
        "pat": .sequence([.ref("ident"), .zeroMore([term(/,/), .ref("ident")], thru)], jot(to_a, to_a2)),
        "match-clauses": .choice([
            .sequence([.ref("empty_clause"), term(/\|\b/), .ref("cons_clause")], train(merge, e1, e3)),
            .sequence([.ref("cons_clause"), term(/\|\b/), .ref("empty_clause")], train(merge, e3, e1)),
            .sequence([term(/\|\b/), .ref("empty_clause"), term(/\|\b/), .ref("cons_clause")], train(merge, e2, e4)),
            .sequence([term(/\|\b/), .ref("cons_clause"), term(/\|\b/), .ref("empty_clause")], train(merge, e4, e2)),
        ], thru),
        "empty_clause": .sequence([term(/\[\s*\]\s*\-\>\b/), .ref("exp")], e2),
        "cons_clause": .sequence([.ref("ident"), term(/::/), .ref("ident"), term(/->/), .ref("exp")], { [.pattern(Ident($0[0]), Ident($0[2]), $0[4])] }),
        "reserved": term(/(?:not|if|then|else|let|in|rec|match|with|fun|shift|reset|true|false)\b/),
        "ident": .sequence([.forbid([.ref("reserved")]), term(/[A-Za-z_][A-Za-z0-9_]*/)], thru),
    ]
    var parser: PEGParser<Syntax>
    init() { parser = PEGParser<Syntax>(lib) }
    func parse(_ input: String, _ top: String = "start") -> PEGResult<Syntax> { return parser.parse(input, top) }
}

@MainActor
class Compiler: ObservableObject {
    @Published private(set) var out: [ChatMsg] = []
    @Published var isProcessing: Bool = false
    private var pendingChats = CurrentValueSubject<[Chat], Never>([])
    private var cancellables = Set<AnyCancellable>()

    init() {
        pendingChats
            .debounce(for: .milliseconds(100), scheduler: RunLoop.main)
            .removeDuplicates()
            .sink { [weak self] chats in
                self?.out.append(contentsOf: chats.map {ChatMsg(id: UUID(), chat: $0)})
                self?.pendingChats.send([])
            }
            .store(in: &cancellables)
    }
    func append(_ chat: Chat) {
        pendingChats.send(pendingChats.value + [chat])
    }
    
    func spacer(_ n: Int) -> String {
        String(repeating: " ", count: n)
    }
    func compile(_ code: String) {
        isProcessing = true
        Task {
            defer { self.isProcessing = false }
            append(.request(text: code))
            let r = Parser().parse(code)
            guard let ast = r.ast?.first else { append(.error(tag: "Syntax error", text: "* no ast *")); return }
            append(.response(tag: "AST", text: ast.description))
            guard r.rest.isEmpty else { let taken = code[..<r.rest.startIndex]
                append(.error(tag: "Syntax error", text: "\(taken)\n\(spacer(taken.count))\(r.rest)")); return }

//            let r = Parser().parse(code, "exprs")
//            guard let asts = r.ast else { await append("Error", "syntax error"); return }
//            for ast in asts {
//                var a = ast;          await append("AST", a.description)
//                a = Typing.f(a);      await append("Typing", a.description)
//                var k = KNormal.f(a); await append("KNormal", k.description)
//                k = Alpha.f(k);       await append("Alpha", k.description)
//                for _ in 0..<1 {    // optimization
//                    k = Beta.f(k);        await append("Beta", k.description)
//                    k = Assoc.f(k);       await append("Assoc", k.description)
//                    k = Inline.f(k);      await append("Inline", k.description)
//                    k = ConstFold.f(k);   await append("ConstFold", k.description)
//                    k = Elim.f(k);        await append("Elim", k.description)
//                }
//                let (fdc, c) = Closure.f(k); for fd in fdc { await append("-", fd.description) }
//                await append("Closure", c.description)
//                let (fda, asm) = Virtual.f(fdc, c); for fd in fda { await append("-", fd.description) }
//                await append("Virtual", asm.description)
//                let x = Emit.f(fda, asm); await append("Emit", x)
//            }
        }
    }
}
