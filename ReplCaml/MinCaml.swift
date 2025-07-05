import Foundation

public final class Id {
    public typealias T = String
    public typealias L = String
    private static var counter = 0

    init() {}
    static func genid(_ s: String) -> T {
        counter += 1
        return "\(s).\(Id.counter)"
    }
    static func gentmp(_ t: Typ) -> T {
        counter += 1
        return "$\(t.rep)\(Id.counter)"
    }
}

public final class Opt: Equatable, CustomStringConvertible {
    typealias T = Typ

    var v: T?
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
    public typealias T = Typ

    case UNIT
    case BOOL
    case INT
    case FLOAT
    case VAR(Opt)
    case FUN([Self], Self)
    static func gentyp() -> T { T.VAR(Opt(nil)) }
    public var description: String {
        switch self {
        case .UNIT: return "()"
        case .BOOL: return "Bool"
        case .INT: return "Int"
        case .FLOAT: return "Float"
        case .VAR(let v): return "Var(\(v))"
        case .FUN(let a, let r): return "(\(a.map(\.description).joined(separator: " ")))->\(r.description)"
        }
    }
    var rep: String {
        switch self {
        case .UNIT: return "u"
        case .BOOL: return "b"
        case .INT: return "i"
        case .FLOAT: return "d"
        case .FUN: return "f"
        case .VAR: return "bug:Typ.rep(var)"
        }
    }
}

public class Ident: Equatable, CustomStringConvertible {
    var name: String
    var typ: Typ
    init(_ name: String) {
        self.name = name
        self.typ = Typ.gentyp()
    }
    init(_ name: String, _ typ: Typ) {
        self.name = name
        self.typ = typ
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
    static func m(_ l: T, _ r: T) -> T { .MUL(lhs: l, rhs: r) }
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
        case .LET(let x, let y, let z): return "(let \(x)=\(y) in \(z))"
        case .LETREC(let n, let a, let b, let e): return "(let rec \(n) \(a.map(\.description).joined(separator: " "))=\(b) in \(e))"
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
    public typealias T = Syntax
    private static func thru(_ ast: [T]) -> [T] { return ast }
    private static func word(_ str: String) -> T { return .punct(str) }
    let lib: [String: PEGRule<T>] = [
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
                             .ref("ident"), .onePlus([.ref("ident")], { [.composite($0)] }), // TODO: support ()
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
            if xs.count == 1 { xs } else { [.APP(fn: xs[0], args: [T](xs[1...]))] } }),
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
    var parser: PEGParser<T>
    init() { parser = PEGParser<T>(lib) }
    func parse(_ input: String, _ top: String) -> PEGResult<T> { return parser.parse(input, top) }
}

public struct Typing {
    public typealias T = Syntax
    public typealias E = [String: Typ]

    public var env: E = [:]
    public var s: T = .UNIT
    init(_ ast: T) {
        var env: E = [:]
        unify(.UNIT, infer(&env, ast))
        //extenv = extenv.mapValues(deref_typ)
        self.s = deref_term(ast)
        self.env = env
    }
    func deref_typ(_ t: Typ) -> Typ {
        switch t {
        case .FUN(let a, let e): return .FUN(a.map(deref_typ), deref_typ(e))
        case .VAR(let x): if let t = x.v { let t2 = deref_typ(t); x.v = t2; return t2 }
            else { print("deref_typ failed"); x.v = .INT; return .INT }
        default: return t
        }
    }
    func deref_id_typ(_ xt: Ident) -> Ident { return Ident(xt.name, deref_typ(xt.typ)) }
    func deref_term(_ t: T) -> T {
        switch t {
        case .ADD(let l, let r): return .ADD(lhs: deref_term(l), rhs: deref_term(r))
        case .MUL(let l, let r): return .MUL(lhs: deref_term(l), rhs: deref_term(r))
        case .LET(let xt, let v, let b): return .LET(name: deref_id_typ(xt), value: deref_term(v), in: deref_term(b))
        case .LETREC(let xt, let a, let b, in: let e):
            return .LETREC(name: deref_id_typ(xt), args: a.map(deref_id_typ), body: deref_term(b), in: deref_term(e))
        case .APP(let f, let a): return .APP(fn: deref_term(f), args: a.map(deref_term))
        default: return t
        }
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
        case (.FUN(let a1, let r1), .FUN(let a2, let r2)): zip(a1, a2).forEach { unify($0, $1) }; unify(r1, r2)
        case (.VAR(let l), .VAR(let r)):
            if l == r { break } else if l.v == nil { l.v = r.v } else { r.v = l.v }
        case (.VAR(let l), _): if l.v != nil { unify(l.v!, b) } else { l.update(b) }
        case (_, .VAR(let r)): if r.v != nil { unify(a, r.v!) } else { r.update(a) }
        default: break
        }
    }
    func infer(_ env: inout [String: Typ], _ s: T) -> Typ {
        switch s {
        case .UNIT: return .UNIT
        case .BOOL: return .BOOL
        case .INT: return .INT
        case .FLOAT: return .FLOAT

        case .ADD(let l, let r): unify(.INT, infer(&env, l)); unify(.INT, infer(&env, r)); return .INT
        case .SUB(let l, let r): unify(.INT, infer(&env, l)); unify(.INT, infer(&env, r)); return .INT
        case .MUL(let l, let r): unify(.INT, infer(&env, l)); unify(.INT, infer(&env, r)); return .INT
        case .DIV(let l, let r): unify(.INT, infer(&env, l)); unify(.INT, infer(&env, r)); return .INT

        case .LET(let xt, let v, let e):
            unify(xt.typ, infer(&env, v))
            env[xt.name] = xt.typ
            return infer(&env, e)
        case .VAR(let x): if let t = env[x] { return t } else { print("extvar"); return Typ.gentyp() }
        case .LETREC(let xt, let a, let b, let e):
            env[xt.name] = xt.typ
            var cl = env
            a.forEach { cl[$0.name] = $0.typ }
            unify(xt.typ, .FUN(a.map(\.typ), infer(&cl, b)))
            return infer(&cl, e)

        case .APP(let f, let a):
            let t = Typ.gentyp()
            unify(infer(&env, f), .FUN(a.map { infer(&env, $0) }, t))
            return t

//        case .COND(_, let t, let e): let tt = infer(&env, t); let te = infer(&env, e); return infer(&env, t)
//        case .CMP: return .BOOL
//        case .SEMICOLON: return .UNIT

        default: return .UNIT
        }
    }
}

public struct IdentX: Equatable, CustomStringConvertible {
    var name: Id.T
    var typ: Typ
    init(_ name: Id.T, _ typ: Typ) {
        self.name = name
        self.typ = typ
    }
    init(_ xt: Ident) {
        self.name = xt.name
        self.typ = xt.typ
    }
    public var description: String { name } // { "\(name):\(typ)" }
}

public struct IdentL: Equatable, CustomStringConvertible {
    var name: Id.L
    var typ: Typ
    init(_ name: Id.L, _ typ: Typ) {
        self.name = name
        self.typ = typ
    }
    init(_ xt: IdentX) {
        self.name = xt.name
        self.typ = xt.typ
    }

    public var description: String { "\(name):\(typ)" }
}

public indirect enum KNormalT: Equatable, CustomStringConvertible {
    public typealias T = KNormalT
    public typealias I = Id.T
    
    case UNIT
    case INT(Int)
    case FLOAT(Double)
//    case MOP(String, I)
//    case DOP(String, I, I)
    case ADD(I, I)
    case MUL(I, I)
//    case COND(String, I, I, T, T)
    case IFEQ(I, I, T, T)
    case LET(IdentX, T, T)
    case VAR(I)
    case LETREC(IdentX, [IdentX], T, T)
    case APP(I, [I])

    public var to_i: Int? { if case .INT(let i) = self {return i} else {return nil}}
    public var description: String {
        switch self {
        case .UNIT: return "()"
        case .INT(let x): return "\(x)"
        case .FLOAT(let x): return "\(x)f"
        case .ADD(let l, let r): return "(+ \(l) \(r))"
        case .MUL(let l, let r): return "(* \(l) \(r))"
        case .IFEQ(let l, let r, let a, let b): return "(ifeq \(l) \(r) \(a) \(b))"
        case .LET(let n, let v, let e): return "(let \(n)=\(v) in \(e))"
        case .VAR(let n): return "(var \(n))"
        case .LETREC(let n, let a, let b, let e): return "(let rec \(n) \(a.map(\.description).joined(separator: " "))=\(b) in \(e))"
        case .APP(let f, let a): return "(\(f) \(a.map(\.description).joined(separator: " ")))"
        }
    }

    public static func u() -> T { .UNIT }
    public static func i(_ n: Int) -> T { .INT(n) }
    public static func v(_ s: I) -> T { .VAR(s) }
    public static func d(_ n: I, _ v: T, in e: T) -> T  { .LET(IdentX(n, .INT), v, e) }
    public static func f(_ n: I, _ a: [I], _ b: T, in e: T) -> T  { .LETREC(IdentX(n, .INT), a.map {IdentX($0, .INT)}, b, e) }

    public static func a(_ l: I, _ r: I) -> T { .ADD(l, r) }
    public static func m(_ l: I, _ r: I) -> T { .MUL(l, r) }
    public static func x(_ f: I, _ a: [I]) -> T { .APP(f, a) }

    public var fv: Set<String> {
        switch self {
        case .ADD(let l, let r), .MUL(let l, let r): return [l, r]
        case .VAR(let x): return [x]
        case .LET(let xt, let v, let e): return v.fv.union(e.fv.subtracting([xt.name]))
        case .LETREC(let xt, let a, let b, let e):
            return b.fv.subtracting(a.map(\.name)).union(e.fv).subtracting([xt.name])
        case .APP(let f, let a): return Set<String>(a).union([f])
        default: return []
        }
    }
}

public struct KNormal {
    public typealias T = KNormalT
    public typealias E = [String: Typ]

    public var env: E = [:]
    public var k = T.UNIT
    func insert_let(_ et: (e: T, t: Typ), _ k: (String)->(T, Typ)) -> (T, Typ) {
        if case .VAR(let x) = et.e { return k(x) }
        else {
            let x = Id.gentmp(et.t)
            let (e, t) = k(x)
            return (.LET(IdentX(x, et.t), et.e, e), t)
        }
    }
    func g(_ env: inout E, _ s: Syntax) -> (T, Typ) {
        switch s {
        case .UNIT: return (.UNIT, .UNIT)
        case .BOOL(let b): return (.INT(b ? 1 : 0), .INT)
        case .INT(let i): return (.INT(i), .INT)
        case .FLOAT(let d): return (.FLOAT(d), .FLOAT)
// .NOT
        case .ADD(lhs: let lhs, rhs: let rhs):
            return insert_let(g(&env, lhs)) { x in insert_let(g(&env, rhs)) { y in return (.ADD(x, y), .INT) } }
//        case .SUB(lhs: let lhs, rhs: let rhs):
//            insert_let(g(&env, lhs)) { x in insert_let(g(&env, rhs)) { y in return (.ADD(x, y), .INT) } }
        case .MUL(lhs: let lhs, rhs: let rhs):
            return insert_let(g(&env, lhs)) { x in insert_let(g(&env, rhs)) { y in return (.MUL(x, y), .INT) } }
//        case .DIV(lhs: let lhs, rhs: let rhs):
//            insert_let(g(&env, lhs)) { x in insert_let(g(&env, rhs)) { y in return (.ADD(x, y), .INT) } }
        case .VAR(let x): return (.VAR(x), env[x] ?? .INT) // forcing to int
        case .LET(name: let xt, value: let v, in: let e):
            let (e1, t1) = g(&env, v)
            env[xt.name] = xt.typ
            let (e2, t2) = g(&env, e)   // BUG: need sub env, may be or not, seems ok by spec because of 'in'?
            return (.LET(IdentX(xt), e1, e2), t2)
        case .LETREC(name: let xt, args: let a, body: let b, in: let e):
            var cl = env
            cl[xt.name] = xt.typ
            env[xt.name] = xt.typ
            let (e2, t2) = g(&cl, e)
            let ax = a.map { cl[$0.name] = $0.typ; return IdentX($0) }
            let (e1, t1) = g(&cl, b)
            return (.LETREC(IdentX(xt), ax, e1, e2), t2)
        case .APP(fn: let fn, args: let args):
            let g_e1 = g(&env, fn)
            if case .FUN(_, let t) = g_e1.1 {
                return insert_let(g_e1) { f in
                    func bind(_ xs: [String], _ rs: [Syntax]) -> (T, Typ) {
                        if rs.isEmpty { return (.APP(f, xs), t) }
                        return insert_let(g(&env, rs.first!)) { x in
                            return bind(xs + [x], [Syntax](rs.dropFirst()))
                        }
                    }
                    return bind([], args)
                }
            } else { return (.UNIT, .UNIT) }

//        case .COND(pred: let pred, ifthen: let ifthen, ifelse: let ifelse):
//        case .CMP(pred: let pred, lhs: let lhs, rhs: let rhs):

        default: return (.UNIT, .UNIT)
        }
    }
    init(_ e: Syntax) {
        var env: E = [:]
        let kt = g(&env, e)
        self.env = env
        self.k = kt.0
    }
}

public struct Alpha {
    public typealias T = KNormalT
    public typealias E = [String: String]

    public var env: E = [:]
    public var k = T.UNIT
    init(_ k: T, _ env0: E = [:]) {
        var env = env0
        self.k = g(&env, k)
        self.env = env
    }
    func find(_ x: String, _ env: E) -> String {
        return env[x] ?? x
    }
    func g(_ env: inout E, _ k: T) -> T {
        switch k {
        case .ADD(let lhs, let rhs): return .ADD(find(lhs, env), find(rhs, env))
        case .MUL(let lhs, let rhs): return .MUL(find(lhs, env), find(rhs, env))
        case .VAR(let x): return .VAR(env[x]!)
        case .LET(let xt, let v, let e):
            let x = Id.genid(xt.name)
            env[xt.name] = x
            return .LET(IdentX(x, xt.typ), g(&env, v), g(&env, e))
        case .LETREC(let xt, let a, let b, let e):
            let x = Id.genid(xt.name)
            env[xt.name] = x
            var cl = env
            var cl2 = env
            let ys = a.map { let y = Id.genid($0.name); cl2[$0.name] = y; return IdentX(y, $0.typ) }
            return .LETREC(IdentX(x, xt.typ), ys, g(&cl2, b), g(&cl, e))
        case .APP(let f, let a): return .APP(env[f]!, a.map { env[$0]! })

        default: return k
        }
    }
}

public struct Beta {
    public typealias T = KNormalT
    public typealias E = [String: String]

    public var env: E = [:]
    public var k = T.UNIT
    init(_ k: T) {
        var env: E = [:]
        self.k = g(&env, k)
        self.env = env
    }
    func find(_ x: String, _ env: E) -> String { env[x] ?? x }

    func g(_ env: inout E, _ k: T) -> T {
        switch k {
        case .ADD(let l, let r): return .ADD(find(l, env), find(r, env))
        case .MUL(let l, let r): return .MUL(find(l, env), find(r, env))
        case .VAR(let x): return .VAR(find(x, env))
        case .LET(let xt, let v, let e):
            let e1 = g(&env, v)
            if case .VAR(let y) = e1 {
                env[xt.name] = y
                return g(&env, e)
            } else {
                let e2 = g(&env, e)
                return .LET(xt, e1, e2)
            }
        case .LETREC(let xt, let a, let b, let e): return .LETREC(xt, a, g(&env, b), g(&env, e))
        case .APP(let f, let a): return .APP(find(f, env), a.map { find($0, env) })
        default: return k
        }
    }
}

public struct Assoc {  // re-association actually
    public typealias T = KNormalT
    public var k = T.UNIT

    init(_ k: T) {
        self.k = f(k)
    }
    func f(_ k: T) -> T {
        switch k {
        case .LET(let xt, let v, let e2):
            func insert(_ e: T) -> T {
                switch e {
                case .LET(let yt, let e3, let e4): return .LET(yt, e3, insert(e4))
                case .LETREC(let yt, let a, let b, let e): return .LETREC(yt, a, b, insert(e))
                default: return .LET(xt, e, f(e2))
                }
            }
            return insert(f(v))
        case .LETREC(let xt, let a, let b, let e): return .LETREC(xt, a, f(b), f(e))
        default: return k
        }
    }
}

public struct Inline {
    public typealias T = KNormalT
    public typealias E = [String: ([String], KNormalT)]  // args, body

    public var env: E = [:]
    public var k = T.UNIT
    var threshold: Int

    init(_ k: T, _ threshold: Int = 0) {
        var env: E = [:]
        self.threshold = threshold
        self.k = g(&env, k)
        self.env = env
    }
    func size(of k:T) -> Int {
        switch k {
        case .LET(_, let v, let e): return 1 + size(of: e) + size(of: v)
        case .LETREC(_, _, let b, let e): return 1 + size(of: b) + size(of: e)
        default: return 1
        }
    }
    func g(_ env: inout E, _ k: T) -> T {
        switch k {
        case .LET(let xt, let v, let e): return .LET(xt, g(&env, v), g(&env, e))
        case .LETREC(let xt, let a, let b, let e):
            if size(of: b) < self.threshold { env[xt.name] = (a.map(\.name), b) }
            var cl = env
            return .LETREC(xt, a, g(&cl, b), g(&cl, e))
        case .APP(let f, let a):
            guard let (zs, e) = env[f] else { return k }
            var cl: Alpha.E = [:]
            zip(zs, a).forEach { cl[$0] = $1 }
            let k = Alpha(e, cl)
            return k.k
        default: return k
        }
    }
}

struct ConstFold {
    public typealias T = KNormalT
    public typealias E = [String: T]
    public var env: E = [:]
    public var k = KNormalT.UNIT
    init(_ k: KNormalT) {
        var env: E = [:]
        self.k = g(&env, k)
        self.env = env
    }
    func findi(_ x: String, _ env: E) -> Int? {
        guard let t = env[x] else { return nil }
        if case .INT(let i) = t { return i } else { return nil }
    }
    func g(_ env: inout E, _ k: T) -> T {
        switch k {
        case .VAR(let x): if let i = findi(x, env) { return .INT(i) } else { return k }
        case .ADD(let lhs, let rhs): let l = findi(lhs, env), r = findi(rhs, env)
            if l != nil && r != nil { return .INT(l! + r!) } else { return k }
        case .MUL(let lhs, let rhs): let l = findi(lhs, env), r = findi(rhs, env)
            if l != nil && r != nil { return .INT(l! * r!) } else { return k }
        case .LET(let xt, let v, let e):
            let e1 = g(&env, v)
            env[xt.name] = e1
            let e2 = g(&env, e)
            return .LET(xt, e1, e2)
        case .LETREC(let xt, let a, let b, let e):
            return .LETREC(xt, a, g(&env, b), g(&env, e))
        default: return k
        }
    }
}

public struct Elim {
    public typealias T = KNormalT

    public var k = T.UNIT
    init(_ k: T) {
        self.k = f(k)
    }
    func hasSideEffect(_ k: T) -> Bool {
        switch k {
        case .LET(_, let v, let e): return hasSideEffect(v) || hasSideEffect(e)
        case .LETREC(_, _, _, let e): return hasSideEffect(e)
        case .APP: return true
        default: return false
        }
    }
    func f(_ k: T) -> T {
        switch k {
        case .LET(let xt, let v, let e):
            let e1 = f(v)
            let e2 = f(e)
            if hasSideEffect(e1) || e2.fv.contains(xt.name) { return .LET(xt, e1, e2) }
            else { return e2 }
        case .LETREC(let xt, let a, let b, let e):
            let e2 = f(e)
            if e2.fv.contains(xt.name) { return .LETREC(xt, a, f(b), e2) }
            else { return e2 }
        default: return k
        }
    }
}

struct ClosureX {
    public typealias T = Self

    var entry: Id.L
    var actual_fv: [Id.T]
}

public indirect enum ClosureT: CustomStringConvertible {
    public typealias T = Self
    public typealias I = Id.T   // variable
    public typealias L = Id.L   // function

    struct Fundef: CustomStringConvertible {
        var name: IdentL
        var args: [IdentX]
        var formal_fv: [IdentX]
        var body: ClosureT
        init(_ name: IdentL, _ args: [IdentX], _ formal_fv: [IdentX], _ body: ClosureT) {
            self.name = name
            self.args = args
            self.formal_fv = formal_fv
            self.body = body
        }
        var description: String {
            return "\(name) \(args.map(\.description).joined(separator: " "))/\(formal_fv.map(\.description).joined(separator: " "))=\(body)"
        }
    }

    case UNIT
    case INT(Int)

    case ADD(I, I)
    case MUL(I, I)

    case LET(IdentX, T, T)
    case VAR(I)
//    case LETREC(IdentX, [IdentX], T, T)
//    case MakeCls(IdentX, ClosureX(Id.l, [Id.t]), T)
    case MakeCls(IdentX, L, [I], T)
//    case APP(I, [I])
    case AppCls(I, [I]) // closure
    case AppDir(L, [I]) // direct call(top level)

    public var fv: Set<String> {
        switch self {
        case .ADD(let l, let r), .MUL(let l, let r): return [l, r]
        case .VAR(let x): return [x]
        case .LET(let xt, let v, let e): return v.fv.union(e.fv.subtracting([xt.name]))
        case .MakeCls(let xt, let l, let v, let e):
            return Set<String>(v).union(e.fv).subtracting([xt.name])
        case .AppCls(let f, let a): return Set<String>(a).union([f])
        case .AppDir(_, let a): return Set<String>(a)
        default: return []
        }
    }
    public var description: String {
        switch self {
        case .UNIT: return "()"
        case .INT(let x): return "\(x)"
        case .ADD(let l, let r): return "(+ \(l) \(r))"
        case .MUL(let l, let r): return "(* \(l) \(r))"
        case .LET(let n, let v, let e): return "(let \(n)=\(v) in \(e))"
        case .VAR(let n): return "(var \(n))"
        case .MakeCls(let n, let a, let fv, let e): return "(\(n.name):\(n.typ) \(a)/\(fv.map(\.description).joined(separator: " "))=\(e))"
        case .AppCls(let f, let a): return "(\(f) \(a.map(\.description).joined(separator: " ")))"
        case .AppDir(let f, let a): return "(\(f) \(a.map(\.description).joined(separator: " ")))"
        }
    }
}

struct Closure {
    typealias T = ClosureT
    typealias E = [String: Typ]
    typealias K = ClosureT

    public var toplevel: [T.Fundef] = []
    public var env: E = [:]
    public var e = T.UNIT

    init(_ k: KNormalT) {
        var toplevel: [T.Fundef] = []
        var env: E = [:]
        var known: Set<String> = []
        self.e = g(&env, &known, &toplevel, k)
        self.env = env
        self.toplevel = toplevel
        // return Prog(List.rev !toplevel, e')
    }
    // let rec quad x = let rec dbl x = x + x in dbl (dbl x) in quad 123
    // let rec make_adder x = let rec adder y = x + y in adder in (make_adder 3) 7

    func g(_ env: inout E, _ known: inout Set<String>, _ toplevel: inout [T.Fundef], _ k: KNormalT) -> T {
        switch k {
        case .UNIT: return .UNIT
        case .INT(let i): return .INT(i)
        case .ADD(let l, let r): return .ADD(l, r)
        case .MUL(let l, let r): return .MUL(l, r)
        case .VAR(let x): return .VAR(x)
        case .LET(let xt, let v, let e):
            env[xt.name] = xt.typ
            return .LET(IdentX(xt.name, xt.typ), g(&env, &known, &toplevel, v), g(&env, &known, &toplevel, e))
        case .LETREC(let xt, let a, let b, let e):
            env[xt.name] = xt.typ
            var cl = env
            known.insert(xt.name)
            var knownx = known
            a.forEach { cl[$0.name] = $0.typ }
            var toplevel_trial = toplevel
            var e1 = g(&cl, &knownx, &toplevel_trial, b)
            let zs = e1.fv.subtracting(a.map(\.name))
            if zs.isEmpty {
                toplevel = toplevel_trial
            } else {
                a.forEach { cl[$0.name] = $0.typ } // ?
                e1 = g(&cl, &known, &toplevel, b)
                knownx = known
            }
            let zz = e1.fv.subtracting(Set<String>([xt.name]).union(a.map(\.name)))
            let zts = zz.map { IdentX($0, cl[$0]!) }
            let fn = T.Fundef(IdentL(xt), a, zts, e1)
            toplevel.append(fn)
            let e2 = g(&env, &known, &toplevel, e)
            if e2.fv.contains(xt.name) { return .MakeCls(xt, xt.name, Array(zz), e2) }
            else { return e2  }

        case .APP(let f, let a):
            if known.contains(f) { return .AppDir(f, a) } else { return .AppCls(f, a) }

        default: return .UNIT // TODO
        }
    }
}

indirect enum Asm: CustomStringConvertible {
    enum Exp: CustomStringConvertible {
        case NOP
        case SET(Int)
        case SETL(Id.L)
        case MOV(Id.T)
        case FMOVD(Id.T)
        case ADD(Id.T, Id.T)
        case ADDi(Id.T, Int)
//        case SLL(Id.T, Id.T)
//        case SLLi(Id.T, Int)
//        case LD(Id.T, Id.T)
        case LDi(Id.T, Int)
//        case ST(Id.T, Id.T, Id.T)
        case STi(Id.T, Id.T, Int)
        case MUL(Id.T, Id.T)
//        case LDDF(Id.T, Id.T)
        case LDDFi(Id.T, Int)
//        case STDF(Id.T, Id.T, Id.T)
        case STDFi(Id.T, Id.T, Int)
        case CALLCLS(Id.T, [Id.T], [Id.T])
        case CALLDIR(Id.T, [Id.T], [Id.T])
//        case SAVE(Id.T, Id.T)
//        case RESTORE(Id.T)

        var description: String {
            switch self {
            case .NOP: return "NOP;"
            case .SET(let i): return "SET \(i);" // TODO: -> store
            case .SETL(let i): return "SETL \(i);" // TODO: -> store
            case .MOV(let i): return "MOV \(i);" // TODO: -> alloca;store
            case .FMOVD(let i): return "FMOVD \(i);"
            case .ADD(let i, let j): return "ADD \(i), \(j);"
            case .ADDi(let i, let j): return "ADDi \(i), \(j);"
//            case .SLL(let i, let j): return "SLL \(i), \(j);"   // Shift Left Logical
//            case .SLLi(let i, let j): return "SLLi \(i), \(j);"
//            case .LD(let i, let j): return "LD \(i), \(j);"
            case .LDi(let i, let j): return "LDi \(i), \(j);"
//            case .ST(let i, let j, let k): return "ST \(i), \(j), \(k);" // store ptr
            case .STi(let i, let j, let k): return "STi \(i), \(j), \(k);" // store
            case .MUL(let i, let j): return "MUL \(i), \(j);"
//            case .LDDF(let i, let j): return "LDDF \(i), \(j);"
            case .LDDFi(let i, let j): return "LDDFi \(i), \(j);"
//            case .STDF(let i, let j, let k): return "STDF \(i), \(j), \(k);"    // Store Double Floating-Point register
            case .STDFi(let i, let j, let k): return "STDFi \(i), \(j), \(k);"
            case .CALLCLS(let i, let j, let k): return "CALLCLS \(i) \(j.joined(separator: ", "))/\(k.joined(separator: ", "));"
            case .CALLDIR(let i, let j, let k): return "CALLDIR \(i) \(j.joined(separator: ", "))/\(k.joined(separator: ", "));"
//            case .SAVE(let i, let j): return "SAVE \(i), \(j);"
//            case .RESTORE(let i): return "RESTORE \(i);"
            }
        }
    }
    case ANS(Exp) // TODO: -> %tmp = ext; ret type %tmp
    case LET(IdentX, Exp, Asm)

    struct Fundef: CustomStringConvertible {
        let name: Id.L
        let args: [String]
        let fargs: [String]
        let body: Asm
        let ret: Typ
        init(_ name: Id.L, _ args: [String], _ fargs: [String], _ body: Asm, _ ret: Typ) {
            self.name = name
            self.args = args
            self.fargs = fargs
            self.body = body
            self.ret = ret
        }
        var description: String {
            return "(\(name) \(args.joined(separator: " "))/\(fargs.joined(separator: " ")) \(body))"
        }
    }
    var description: String {
        switch self {
            case .ANS(let e): return "ANS \(e);"
            case .LET(let x, let e, let a):
            return "(LET \(x) \(e) in \(a))"
        }
    }
    static let reg_cl: String = "closure_ptr"
    static let reg_hp: String = "heap_ptr"
}

struct Virtual {
    typealias T = Asm
    typealias E = [String: Typ]

    func align(_ i: Int) -> Int { if i % 8 == 0 { return i } else { return i + 4 } }

    func classify<ACC>(_ xts: [IdentX], _ ini: ACC,
                  _ addf: (ACC, Id.T)->ACC,
                  _ addi: (ACC, Id.T, Typ)->ACC) -> ACC {
        return xts.reduce(ini) { acc, xt in
            switch xt.typ {
            case .UNIT: return acc
            case .FLOAT: return addf(acc, xt.name)
            default: return addi(acc, xt.name, xt.typ)
            }
        }
    }
    func separate(_ xts: [IdentX]) -> ([Id.T], [Id.T]) {
        classify(xts, ([], []),
                 { t, x in return (t.0, t.1 + [x]) },
                 { t, x, _ in return (t.0 + [x], t.1) })
    }

    func expand(_ xts: [IdentX], _ ini: (Int, T),
                _ addf: (Id.T, Int, Asm)->T,
                _ addi: (Id.T, Typ, Int, Asm)->T) -> (Int, T) {
        return classify(xts, ini,
                        { oa, x in return (oa.0 + 8, addf(x, oa.0, oa.1)) },
                        { oa, x, t in return (oa.0 + 4, addi(x, t, oa.0, oa.1))} )
    }

    func concat(_ e1: T, _ xt: IdentX, _ e2: T) -> T {
        switch e1 {
        case .ANS(let exp): return .LET(xt, exp, e2)
        case .LET(let yt, let exp, let e1_): return .LET(yt, exp, concat(e1_, xt, e2))
        }
    }

    func g(_ env: inout E, _ e: ClosureT) -> T {
        switch e {
        case .UNIT: return .ANS(.NOP)
        case .INT(let i): return .ANS(.SET(i))
        case .ADD(let l, let r): return .ANS(.ADD(l, r))
        case .MUL(let l, let r): return .ANS(.MUL(l, r))
        case .LET(let xt, let v, let b):
            let e1 = g(&env, v)
            env[xt.name] = xt.typ
            let e2 = g(&env, b)
            return concat(e1, xt, e2)
        case .VAR(let x):
            switch env[x]! {
            case .UNIT: return .ANS(.NOP)
            case .FLOAT: return .ANS(.FMOVD(x))
            default: return .ANS(.MOV(x))
            }
        case .MakeCls(let xt, let l, let fv, let b):
            env[xt.name] = xt.typ
            let e2 = g(&env, b)
            let (offset, store_fv) = expand(fv.map {IdentX($0, env[$0]!)}, (4, e2),
                                            {y, offset, store_fv in seq(.STDFi(y, xt.name, offset), store_fv) },
                                            {y, _, offset, store_fv in seq(.STi(y, xt.name, offset), store_fv) })
            let z = Id.genid("l")
            return .LET(xt, .MOV(Asm.reg_hp),
                        .LET(IdentX(Asm.reg_hp, .INT), .ADDi(Asm.reg_hp, offset),
                             .LET(IdentX(z, .INT), .SETL(l),
                                  seq(.STi(z, xt.name, 0), store_fv))))
        case .AppCls(let x, let ys):
            let (i, f) = separate(ys.map { IdentX($0, env[$0]!) })
            return .ANS(.CALLCLS(x, i, f))
        case .AppDir(let x, let ys):
            let (i, f) = separate(ys.map { IdentX($0, env[$0]!) })
            return .ANS(.CALLDIR(x, i, f))
        }
    }

    func fletd(_ x: Id.T, _ e1: Asm.Exp, _ e2: Asm) -> T {
        return .LET(IdentX(x, .FLOAT), e1, e2)
    }

    func seq(_ e1: Asm.Exp, _ e2: Asm) -> T {
        return .LET(IdentX(Id.gentmp(.UNIT), .UNIT), e1, e2)
    }

    func h(_ f: ClosureT.Fundef) -> Asm.Fundef {
        let (it, ft) = separate(f.args)
        var env: E = [:]
        f.formal_fv.forEach { env[$0.name] = $0.typ }
        f.args.forEach { env[$0.name] = $0.typ }
        env[f.name.name] = f.name.typ
        let (offset, load) = expand(f.formal_fv, (4, g(&env, f.body)),
                                    {z, offset, load in fletd(z, .LDDFi(Asm.reg_cl, offset), load) },
                                    {z, t, offset, load in return .LET(IdentX(z, t), .LDi(Asm.reg_cl, offset), load) })
        if case .FUN(_, let t2) = f.name.typ {
            return Asm.Fundef(f.name.name, it, ft, load, t2)
        } else { fatalError() }
    }

    var fundefs: [Asm.Fundef] = []
    var e: T = .ANS(.NOP)
    init(_ fundefs: [ClosureT.Fundef], _ e: ClosureT) {
        self.fundefs = fundefs.map(h)
        var env: E = [:]
        self.e = g(&env, e)
    }
}

struct Emit {
    typealias T = Asm

    public var ir: String = "emit"

    func h(_ fd: Asm.Fundef) {
        //declare void @llvm.init.trampoline(ptr <tramp>, ptr <func>, ptr <nval>)

    }
    func g(_ e: T) -> T {
        return e
    }

    init(_ fundefs: [Asm.Fundef], _ e: T) {
        fundefs.forEach { h($0) }
        _ = g(e)
        ir = "ok"
    }
}

struct MinCaml {
    let ps = Parser()
    init() { }

    func handle(_ input: String) -> [(String, String)]? {
        let r = ps.parse(input, "exprs")
        guard let asts = r.ast else { return nil }
        var out: [(String, String)] = []
        asts.forEach { ast in
            out.append((ast.description, "AST"))
            let x1 = Typing(ast)
            out.append((x1.s.description, "Typing"))
            let x2 = KNormal(x1.s)
            out.append((x2.k.description, "KNormal"))
            let x3 = Alpha(x2.k)
            out.append((x3.k.description, "Alpha"))

            //optimization begin
            let x4 = Beta(x3.k)
            out.append((x4.k.description, "Beta"))
            let x5 = Assoc(x4.k)
            out.append((x5.k.description, "Assoc"))
            let x6 = Inline(x5.k)
            out.append((x6.k.description, "Inline"))
            let x7 = ConstFold(x6.k)
            out.append((x7.k.description, "ConstFold"))
            let x8 = Elim(x7.k)
            out.append((x8.k.description, "Elim"))
            //optimization end

            let x9 = Closure(x8.k)
            x9.toplevel.forEach { out.append(($0.description, "-")) }
            out.append((x9.e.description, "Closure"))
            let x10 = Virtual(x9.toplevel, x9.e) // do nothing
            x10.fundefs.forEach { out.append(($0.description, "-")) }
            out.append((x10.e.description, "Virtual"))
            let x11 = Emit(x10.fundefs, x10.e)
            out.append((x11.ir, "Emit"))
        }
        return out
    }
}
