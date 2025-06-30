import Foundation

public indirect enum Ty: Equatable {
    case UNIT
    case BOOL
    case INT
    case FLOAT
    case VAR(Self?)
    case FUN([Self], Self)
    mutating func assign(vartype t: Ty) {
        if case .VAR(.none) = self {
            self = .VAR(t)
        }
    }
}
public class Ident: Equatable, CustomStringConvertible {
    var t: Ty
    var name: String
    init(_ name: String, _ t: Ty = .VAR(nil)) {
        self.t = t
        self.name = name
    }
    public func assign(type t: Ty) {
        self.t = t
    }
    public static func == (lhs: Ident, rhs: Ident) -> Bool {
        lhs.name == rhs.name
    }
    public var description: String { "\(name):\(t)" }
}
public struct Fundef: Equatable, CustomStringConvertible {
    var name: Ident
    var args: [Ident]
    var body: Syntax
    init(_ name: String, _ args: [Syntax], _ body: Syntax) {
        self.name = Ident(name)
        self.args = args.map(\.asIndent)
        self.body = body
    }
    public var description: String { "((\(name) \(args.map(\.description).joined(separator: " "))) \(body))" }
}

public indirect enum Syntax: Equatable, CustomStringConvertible {
    case UNIT
    case BOOL(Bool)
    case INT(Int)
    case FLOAT(Double)
    case VAR(String)
    
    case ADD(Self, Self)
    case SUB(Self, Self)
    case MUL(Self, Self)
    case DIV(Self, Self)
    case LET(Ident, Self, Self)
    case LETREC(Fundef, Self)
    case APP(Self, [Self])
    case COND(Self, Self, Self)
    case CMP(Self, Self, Self)
    case SEMICOLON

    case punct(String)
    case composite([Self])

    func eq(_ ch: String) -> Bool {
        if case .punct(let x) = self { return x == ch } else { return false }
    }
    var asString: String? {
        if case .punct(let x) = self { return x }
        if case .VAR(let x) = self { return x }
        return nil
    }
    var asIndent: Ident {
        if case .VAR(let s) = self { return Ident(s) } else { fatalError() }
    }
    var asArray: [Syntax]? {
        if case .composite(let xs) = self { return xs } else { return nil }
    }
    public var description: String {
        switch self {
        case .UNIT: return "()"
        case .BOOL(let x): return x ? "#t" : "#f"
        case .INT(let x): return String(x)
        case .FLOAT(let x): return String(x)
        case .VAR(let x): return x
        case .ADD(let x, let y): return "(+ \(x) \(y))"
        case .SUB(let x, let y): return "(- \(x) \(y))"
        case .MUL(let x, let y): return "(* \(x) \(y))"
        case .DIV(let x, let y): return "(/ \(x) \(y))"
        case .LET(let x, let y, let z): return "(let (\(x) \(y)) \(z))"
        case .LETREC(let x, let y): return "(let rec \(x) \(y))"
        case .APP(let x, let y): return "(\(x) \(y.map(\.description).joined(separator: " ")))"
        case .COND(let c, let t, let f): return "(cond \(c) \(t) \(f))"
        case .CMP(let c, let l, let r): return "(\(c.asString!) \(l) \(r))"
        case .SEMICOLON: return ";"

        case .punct(let x): return "(punct \(x))"
        case .composite(let xs): return "(\(xs.map(\.description).joined(separator: " ")))"
        }
    }
}

public class Parser {
    private static func thru(_ ast: [Syntax]) -> [Syntax] { return ast }
    private static func word(_ str: String) -> Syntax { return .punct(str) }
    let lib: [String: PEGRule<Syntax>] = [
        "exprs": .sequence([.ref("expr"), .zeroMore([.terminal(/;/, {_ in .SEMICOLON}), .ref("expr")], thru)],
                          { $0.filter { $0 != .SEMICOLON } }),
        "expr": .choice([.ref("letrec"), .ref("letvar"), .ref("cond"), .ref("add")], thru),
        "add": .sequence([.ref("mul"), .zeroMore([.terminal(/[\+\-]/, word), .ref("mul")], thru)],
                         { xs in var r = xs[0]
                             for i in stride(from: 1, to: xs.count, by: 2) {
                                 if xs[i].eq("+") { r = .ADD(r, xs[i + 1]) } else { r = .SUB(r, xs[i + 1]) }
                             }
                             return [r]
                         }),
        "mul": .sequence([.ref("app"), .zeroMore([.terminal(/[\*\/]/, word), .ref("app")], { $0 })],
                         { xs in var r = xs[0]
                             for i in stride(from: 1, to: xs.count, by: 2) {
                                 if xs[i].eq("*") { r = .MUL(r, xs[i + 1]) } else { r = .DIV(r, xs[i + 1]) }
                             }
                             return [r]
                         }),
        "letrec": .sequence([.terminal(/let\b/, word), .terminal(/rec\b/, word),
                             .ref("ident"), .onePlus([.ref("ident")], { [.composite($0)] }),
                             .terminal(/=/, word), .ref("expr"),
                             .terminal(/in\b/, word), .ref("expr")],
                            { xs in [.LETREC(Fundef(xs[2].asString!, xs[3].asArray!, xs[5]), xs[7])] }),
        "letvar": .sequence([.terminal(/let\b/, word), .ref("ident"),
                             .terminal(/=/, word), .ref("expr"),
                             .terminal(/in\b/, word), .ref("expr")],
                            { xs in [.LET(Ident(xs[1].asString!), xs[3], xs[5])] }),
        "cond": .sequence([.terminal(/if\b/, word), .ref("cmpexpr"),
                           .terminal(/then\b/, word), .ref("expr"),
                           .terminal(/else\b/, word), .ref("expr")],
                          { xs in return [.COND(xs[1], xs[3], xs[5])] }),
        "cmpexpr": .sequence([.ref("app"), .opt([.sequence([.terminal(/=|!=|<=|>=|<|>/, word), .ref("app")], thru)], thru)],
                             { xs in if xs.count == 1 { xs } else { [.CMP(xs[1], xs[0], xs[2])] } }),
        "app": .sequence([.ref("atom"), .zeroMore([.ref("atom")], thru)], { xs in
            if xs.count == 1 { xs } else { [.APP(xs[0], [Syntax](xs[1...]))] } }),
        "atom": .choice([.ref("float"), .ref("int"), .ref("unit"), .ref("paren"), .ref("bool"), .ref("ident")], thru),
        "paren": .sequence([.terminal(/\(/, word), .ref("expr"), .terminal(/\)/, word)], { [$0[1]] }),
        "unit": .terminal(/\(\)/, { _ in .UNIT }),
        "int": .terminal(/\d+/, { x in .INT(Int(x)!) }),
        "float": .terminal(/\d+\.\d+/, { x in .FLOAT(Double(x)!) }),
        "bool": .choice([.terminal(/true\b/, { _ in .BOOL(true) }),
                         .terminal(/false\b/, { _ in .BOOL(false) })], thru),
        "ident": .sequence([.forbid([.terminal(/(?:in|let|rec|if|then|else|true|false)\b/, word)], thru), .ref("ident0")], thru),
        "ident0": .terminal(/[A-Za-z_][A-Za-z0-9_]*/, { .VAR($0) }),
    ]
    var parser: PEGParser<Syntax>
    init() { parser = PEGParser<Syntax>(lib) }
    func parse(_ input: String, _ top: String) -> PEGResult<Syntax> {
        return parser.parse(input, top)
    }
}

struct Typing {

    // let rec g env e = (* 型推論ルーチン (caml2html: typing_g) *)
//    func g(_ env: inout [String: Ty], _ e: Syntax) -> Ty {
//        switch e {
//        case .UNIT: return .UNIT
//        case .BOOL(_): return .BOOL
//        case .INT(_): return .INT
//        case .FLOAT(_): return .FLOAT
//
//        case .ADD(let e1, let e2):
//            return .INT
//        case .LET(let id, let e1, let e2): // unify t (g env e1)
//            env[id.name] = id.t
//            return g(&env, e2)
//        }
//    }

    func occur(_ r1: Ty, in r2: Ty) -> Bool {
        switch r2 {
        case .FUN(let t2s, let t2): return occur(r1, in: t2) || t2s.contains { occur(r1, in: $0) }
        case .VAR(let name): if name != nil { return name == r1 } else { return false }
        default: return false
        }
    }
    func unify(_ t1: Ty, _ t2: Ty) {
        //    func unify(_ t1: Ty, _ t2: Ty) -> Bool {
        //        switch (t1, t2) {
        //        case (.UNIT, .UNIT): return true
        //        case (.BOOL, .BOOL): return true
        //        case (.INT, .INT): return true
        //        case (.FLOAT, .FLOAT): return true
        //        case (.FUN(let t1s, let t1), .FUN(let t2s, let t2)): return unify(t1, t2) && zip(t1s, t2s).allSatisfy { self.unify($0, $1) }
        //        case (.VAR(let t1), .VAR(nil)):
        //        case (.VAR(let t1), .VAR(let t2)): return t1 == t2
        //        default: return false
        //        }
        //    }
    }
    func infer(_ env: inout [String: Ty], _ e: Syntax) -> Ty {
        return .UNIT
    }
    func deref_typ(_ t: Ty) -> Ty {
        switch t {
        case .FUN(let t1s, let t2): return .FUN(t1s, t2)
        case .VAR(let name): if let t = name { return deref_typ(t) } else { return .INT }
        default: return t
        }
    }
    func deref_term(_ e: Syntax) -> Syntax {
        switch e {
        case .ADD(let e1, let e2): return .ADD(deref_term(e1), deref_term(e2))
        case .LET(let xt, let e1, let e2): return .LET(Ident(xt.name, deref_typ(xt.t)), deref_term(e1), deref_term(e2))
        default: return e
        }
    }
    func f(_ ast: Syntax) -> Syntax {
        var env: [String: Ty] = [:]
        unify(.UNIT, infer(&env, ast))
        //extenv = extenv.mapValues(deref_typ)
        return deref_term(ast)
    }
}
struct MinCaml {
    let ps = Parser()
    init() { }


    func handle(_ input: String) -> [String]? {
        let r = ps.parse(input, "exprs")
        if let ast = r.ast {
            return ast.map(\.description)
        } else { return nil }

        //let ast = ps.parser.parse("8+2*3-1", "expr")
        //let r = ps.parser.parseRule(" 8  - 2   * ( 5 +  3 ) *   4", .ref("add"))
        //let r = ps.parser.parseRule("let x = 123 in let y = 23 in x - y + 1", .ref("expr"))
        //let r = ps.parser.parseRule("let rec f x = x in f 100", .ref("letrec"))
    }
}

