import Foundation

public final class Opt: Equatable, CustomStringConvertible {
    typealias T = Typ
    private var v: T?
    init(_ v: T? = nil) {
        self.v = v
    }
    func update(_ v: T) {
        self.v = v
    }
    public static func == (lhs: Opt, rhs: Opt) -> Bool {
        return lhs.v == rhs.v
    }
    public var description: String { return v?.description ?? "#undef" }
}

public indirect enum Typ: Equatable, CustomStringConvertible {
    typealias T = Self
    case UNIT
    case BOOL
    case INT
    case FLOAT
    case VAR(Opt)
    case FUN([Self], Self)
    static func newvar() -> Self { Typ.VAR(Opt(nil)) }
    public var description: String {
        switch self {
        case .UNIT: return "()"
        case .BOOL: return "Bool"
        case .INT: return "Int"
        case .FLOAT: return "Float"
        case .VAR(let v): return v.description
        case .FUN(let a, let r): return "(\(a.map(\.description).joined(separator: ", ")))->\(r.description)"
        }
    }
}

public final class Ident: Equatable, CustomStringConvertible {
    var name: String
    var typ: Typ
    init(_ name: String) {
        self.name = name
        self.typ = Typ.newvar( )
    }
    public func update(_ t: Typ) {
        if case .VAR(let v) = self.typ {
            v.update(t)
        }
    }
    public static func == (lhs: Ident, rhs: Ident) -> Bool {
        lhs.name == rhs.name
    }
    public var description: String { "\(name):\(typ)" }
}

public indirect enum Syntax: Equatable, CustomStringConvertible {
    public typealias T = Self
    case UNIT
    case BOOL(Bool)
    case INT(Int)
    case FLOAT(Double)
    case VAR(String)

    case LET(name: Ident, value: T, in: T)
    case LETREC(name: Ident, args: [Ident], body: T, in: T)
    case APP(fn: T, args: [T])
    case COND(pred: T, ifthen: T, ifelse: T)
    case SEMICOLON

    case ADD(lhs: T, rhs: T)
    case SUB(lhs: T, rhs: T)
    case MUL(lhs: T, rhs: T)
    case DIV(lhs: T, rhs: T)
    case CMP(pred: String, lhs: T, rhs: T)
    //    case MOP(op: T, rhs: T) // regular monadic operator
    //    case DOP(op: T, lhs: T, rhs: T) // regular dyadic operator
    //    case MFN(fn: T, rhs: T) // regular monadic function
    //    case DFN(fn: T, lhs: T, rhs: T) // regular dyadic function

    case punct(String)
    case composite([T])

    static func u() -> T { .UNIT }
    static func b(_ n: Bool) -> T { .BOOL(n) }
    static func i(_ n: Int) -> T { .INT(n) }
    static func v(_ s: String) -> T { .VAR(s) }
    static func d(_ n: String, _ v: T, in e: T) -> T  { .LET(name: Ident(n), value: v, in: e) }
    static func f(_ n: String, _ a: [String], _ b: T, in e: T) -> T  { .LETREC(name: Ident(n), args: a.map {Ident($0)}, body: b, in: e) }
    static func c(_ p: T, _ t: T, _ f: T) -> T { .COND(pred: p, ifthen: t, ifelse: f) }

    static func p(_ p: String, _ l: T, _ r: T) -> T { .CMP(pred: p, lhs: l, rhs: r) }
    static func a(_ l: T, _ r: T) -> T { .ADD(lhs: l, rhs: r) }
    static func x(_ f: T, _ a: [T]) -> T { .APP(fn: f, args: a) }

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
        case .LETREC(let n, let a, let b, let e): return "(let rec \(n) \(a.map(\.name).joined(separator: " ")) \(b) \(e))"
        case .APP(let x, let y): return "(\(x) \(y.map(\.description).joined(separator: " ")))"
        case .COND(let c, let t, let f): return "(cond \(c) \(t) \(f))"
        case .CMP(let c, let l, let r): return "(\(c) \(l) \(r))"
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
                                 if xs[i].eq("+") { r = .ADD(lhs: r, rhs: xs[i + 1]) } else { r = .SUB(lhs: r, rhs: xs[i + 1]) }
                             }
                             return [r]
                         }),
        "mul": .sequence([.ref("app"), .zeroMore([.terminal(/[\*\/]/, word), .ref("app")], { $0 })],
                         { xs in var r = xs[0]
                             for i in stride(from: 1, to: xs.count, by: 2) {
                                 if xs[i].eq("*") { r = .MUL(lhs: r, rhs: xs[i + 1]) } else { r = .DIV(lhs: r, rhs: xs[i + 1]) }
                             }
                             return [r]
                         }),
        "letrec": .sequence([.terminal(/let\b/, word), .terminal(/rec\b/, word),
                             .ref("ident"), .onePlus([.ref("ident")], { [.composite($0)] }),
                             .terminal(/=/, word), .ref("expr"),
                             .terminal(/in\b/, word), .ref("expr")],
                            { xs in [.LETREC(name: Ident(xs[2].asString!), args: xs[3].asArray!.map{Ident($0.asString!)}, body: xs[5], in: xs[7])] }),
        "letvar": .sequence([.terminal(/let\b/, word), .ref("ident"),
                             .terminal(/=/, word), .ref("expr"),
                             .terminal(/in\b/, word), .ref("expr")],
                            { xs in [.LET(name: Ident(xs[1].asString!), value: xs[3], in: xs[5])] }),
        "cond": .sequence([.terminal(/if\b/, word), .ref("cmpexpr"),
                           .terminal(/then\b/, word), .ref("expr"),
                           .terminal(/else\b/, word), .ref("expr")],
                          { xs in return [.COND(pred: xs[1], ifthen: xs[3], ifelse: xs[5])] }),
        "cmpexpr": .sequence([.ref("app"), .opt([.sequence([.terminal(/=|!=|<=|>=|<|>/, word), .ref("app")], thru)], thru)],
                             { xs in if xs.count == 1 { xs } else { [.CMP(pred: xs[1].asString!, lhs: xs[0], rhs: xs[2])] } }),
        "app": .sequence([.ref("atom"), .zeroMore([.ref("atom")], thru)], { xs in
            if xs.count == 1 { xs } else { [.APP(fn: xs[0], args: [Syntax](xs[1...]))] } }),
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
    var env: [String: Typ] = [:]
    init(_ ast: Syntax) {
        var env: [String: Typ] = [:]
        unify(.UNIT, infer(&env, ast))
        //extenv = extenv.mapValues(deref_typ)
        //return deref_term(ast)
        self.env = env
    }
    func occur(_ r1: Typ, in r2: Typ) -> Bool {
        switch r2 {
        case .FUN(let t2s, let t2): return occur(r1, in: t2) || t2s.contains { occur(r1, in: $0) }
//        case .VAR(let name): if name != nil { return name == r1 } else { return false }
        default: return false
        }
    }
    func unify(_ a: Typ, _ b: Typ) {
        switch (a, b) {
        case (.VAR(_), .VAR(_)): break
        case (.VAR(let l), _): l.update(b)
        case (_, .VAR(let r)): r.update(a)
        default: break
        }
    }
    func infer(_ env: inout [String: Typ], _ s: Syntax) -> Typ {
        switch s {
        case .UNIT: return .UNIT
        case .BOOL: return .BOOL
        case .INT: return .INT
        case .VAR(let x): return env[x] ?? Typ.newvar()
        case .LET(let n, let v, let e):
            let t = infer(&env, v)
            env[n.name] = t
            n.update(t)
            return infer(&env, e)
        case .LETREC(let n, let a, let b, let e):
            var cl = env
            let at = a.map { let t = Typ.newvar(); cl[$0.name] = t; return t }
            let bt = infer(&cl, b)
            let t = Typ.FUN(at, bt)
            cl[n.name] = t
            env[n.name] = t
            n.update(t)
            return infer(&cl, e)
        case .COND(_, let t, let e): let tt = infer(&env, t); let te = infer(&env, e); return infer(&env, t)
        case .CMP: return .BOOL
        case .ADD(let l, let r): unify(.INT, infer(&env, l)); unify(.INT, infer(&env, r)); return .INT
        case .APP(let f, let a): _ = a.map {infer(&env, $0)}; return infer(&env, f)

        case .FLOAT: return .FLOAT
        case .SEMICOLON: return .UNIT
        case .SUB(let l, let r): unify(.INT, infer(&env, l)); unify(.INT, infer(&env, r)); return .INT
        case .MUL(let l, let r): unify(.INT, infer(&env, l)); unify(.INT, infer(&env, r)); return .INT
        case .DIV(let l, let r): unify(.INT, infer(&env, l)); unify(.INT, infer(&env, r)); return .INT
        case .punct: return .UNIT
        case .composite: return .UNIT
        }
    }
}
struct MinCaml {
    let ps = Parser()
    init() { }

    func handle(_ input: String) -> [String]? {
        let r = ps.parse(input, "exprs")
        if let ast = r.ast {
            let x = Typing(ast.first!)
            var out = ast.map(\.description)
            out.append(x.env.description)
            return out
        } else { return nil }

        //let ast = ps.parser.parse("8+2*3-1", "expr")
        //let r = ps.parser.parseRule(" 8  - 2   * ( 5 +  3 ) *   4", .ref("add"))
        //let r = ps.parser.parseRule("let x = 123 in let y = 23 in x - y + 1", .ref("expr"))
        //let r = ps.parser.parseRule("let rec f x = x + 1 in f 100", .ref("letrec"))
    }
}

