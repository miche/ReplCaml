import Foundation

//let a = Optional<Int>.some(1)

class Op: Equatable, CustomStringConvertible {
    typealias T = Tx
    private var v: T?
    init(v: T? = nil) {
        self.v = v
    }
    func update(_ v: T) {
        self.v = v
    }
    public static func == (lhs: Op, rhs: Op) -> Bool {
        return lhs.v == rhs.v
    }
    var description: String { return v?.description ?? "#undef" }
}
indirect enum Tx: Equatable, CustomStringConvertible {
    case UNIT
    case BOOL
    case INT
    case FLOAT
    case VAR(Op)
    case FUN([Self], Self)
    static func newvar() -> Self { Tx.VAR(Op(v: nil)) }
    var description: String {
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

class Id: CustomStringConvertible {
    var name:String
    var tx:Tx
    var description: String { return "\(name):\(tx.description)" }
    init(_ name:String) {
        self.name = name
        self.tx = Tx.newvar()
    }
    func update(_ t: Tx) {
        if case .VAR(let v) = self.tx {
            v.update(t)
        }
    }
}

indirect enum Sy: CustomStringConvertible {
    typealias T = Self
    case UNIT
    case BOOL(Bool)
    case INT(Int)
    case VAR(String)
    case LET(name: Id, value: T, in: T)
    case LETREC(name: Id, args: [Id], body: T, in: T)
    case COND(pred: T, ifthen: T, ifelse: T) // pred:BOOL, then.type == else.type

    case CMP(pred: String, lhs: T, rhs: T) // comparison dyadic operator, < = > <= != =>
//    case MOP(pred: T, rhs: T) // regular monadic operator
//    case DOP(pred: T, lhs: T, rhs: T) // regular dyadic operator
    case NEG(rhs: T) // monadic function sample: APP(func:-, args:[rhs])
    case ADD(lhs: T, rhs: T) // dyadic function sample: APP(func:+, args:[lhs, rhs])
    case APP(fn: T, args: [T]) // regular function
//    case MFN(fn: T, rhs: T) // regular monadic function
//    case DFN(fn: T, lhs: T, rhs: T) // regular dyadic function

    var description: String {
        switch self {
        case .UNIT: return "()"
        case .BOOL(let x): return x ? "true" : "false"
        case .INT(let x): return "\(x)"
        case .VAR(let x): return x
        case .LET(let n, let v, let e): return "let \(n) = \(v) in \(e)"
        case .LETREC(let n, let a, let b, let e): return "let rec \(n) (\(a.map(\.name).joined(separator: " "))) = \(b) in \(e)"
        case .COND(let p, let t, let e): return "if \(p) then \(t) else \(e)"
        case .CMP(let p, let l, let r): return "\(l) \(p) \(r)"
        case .NEG(let r): return "-\(r)"
        case .ADD(let l, let r): return "\(l) + \(r)"
        case .APP(let f, let a): return "\(f) \(a.map(\.description).joined(separator: " "))"
        }
    }

    static func u() -> T { .UNIT }
    static func b(_ n: Bool) -> T { .BOOL(n) }
    static func i(_ n: Int) -> T { .INT(n) }
    static func v(_ s: String) -> T { .VAR(s) }
    static func d(_ n: String, _ v: Sy, in e: Sy) -> T  { .LET(name: Id(n), value: v, in: e) }
    static func f(_ n: String, _ a: [String], _ b: T, in e: T) -> T  { .LETREC(name: Id(n), args: a.map {Id($0)}, body: b, in: e) }
    static func c(_ p: T, _ t: T, _ f: T) -> T { .COND(pred: p, ifthen: t, ifelse: f) }

    static func p(_ p: String, _ l: T, _ r: T) -> T { .CMP(pred: p, lhs: l, rhs: r) }
    static func m(_ r: T) -> T { .NEG(rhs: r) }
    static func a(_ l: T, _ r: T) -> T { .ADD(lhs: l, rhs: r) }
    static func x(_ f: T, _ a: [T]) -> T { .APP(fn: f, args: a) }
}
func uni(_ a: Tx, _ b: Tx) {
    switch (a, b) {
    case (.VAR(let l), .VAR(let r)): break
    case (.VAR(let l), _): l.update(b)
    case (_, .VAR(let r)): r.update(a)
    default: break
    }
}
func inf(_ env: inout [String: Tx], _ s: Sy) -> Tx {
    switch s {
    case .UNIT: return .UNIT
    case .BOOL: return .BOOL
    case .INT: return .INT
    case .VAR(let x): return env[x] ?? Tx.newvar()
    case .LET(let n, let v, let e):
        let t = inf(&env, v)
        env[n.name] = t
        n.update(t)
        return inf(&env, e)
    case .LETREC(let n, let a, let b, let e):
        var cl = env
        let at = a.map { let t = Tx.newvar(); cl[$0.name] = t; return t }
        let bt = inf(&cl, b)
        let t = Tx.FUN(at, bt)
        cl[n.name] = t
        env[n.name] = t
        n.update(t)
        return inf(&cl, e)
    case .COND(let p, let t, let e): return inf(&env, t)
    case .CMP: return .BOOL
    case .NEG: return .INT
    case .ADD(let l, let r): uni(.INT, inf(&env, l)); uni(.INT, inf(&env, r)); return .INT
    case .APP(let f, let a): return inf(&env, f)
    }
}
//var ast = Sy.LET(.VAR("x"), .INT(1), .VAR("x"))
var ast = Sy.f("f", ["x", "y"], .a(.v("x"), .i(1)),
               in: .d("a", .i(1),
                      in: .c(.p("=", .x(.v("f"), [.v("a")]), .i(1)),
                             .i(10), .i(20))
                    ))
print(ast)
var e: [String: Tx] = [:]
let t = inf(&e, ast)
print(t)
print(ast)
e.forEach { (k, v) in
    print(k, ":", v)
}

func unify(_ lhs: inout Ty, _ rhs: inout Ty) -> Ty? {
    print("unify(\(lhs), \(rhs))")
    switch (lhs, rhs) {
    case (.UNIT, .UNIT), (.BOOL, .BOOL), (.INT, .INT), (.FLOAT, .FLOAT): return lhs
    case (.FUN(let t1s, let t1), .FUN(let t2s, let t2)): return lhs
    case (.VAR(nil), .VAR(nil)): return nil
    case (.VAR(let t1), .VAR(nil)): return t1
    case (.VAR(nil), .VAR(let t2)): return t2
    case (.VAR(let t1), .VAR(let t2)): return t1
    case (.VAR(nil), _): lhs = .VAR(rhs); return rhs
    case (_, .VAR(nil)): rhs = .VAR(lhs); return lhs
    default: return nil
    }
}

func infer(_ env: inout [String: Ty], _ x: Syntax) -> Ty {
    switch x {
    case .UNIT: return .UNIT
    case .BOOL: return .BOOL
    case .INT: return .INT
    case .FLOAT: return .FLOAT
    case .ADD(let l, let r): let tl = infer(&env, l), tr = infer(&env, r); return tr
        //if tl == tr { return tl } else { return .UNIT }
    case .SUB: return .INT
    case .VAR(let n): return env[n] ?? .UNIT
    case .LET(let n, let v, let e):
        var t = Ty.VAR(infer(&env, v)) 
        n.assign(type: t)
        env[n.name] = t
        return infer(&env, e)
    case .LETREC(let xts, let e):
        var cl = env
        cl[xts.name.name] = xts.name.t
        let t = Ty.FUN(xts.args.map(\.t), infer(&cl, xts.body))
        env[xts.name.name] = t
        return infer(&cl, e)
    case .APP(let e, let es):
        let t = infer(&env, e)
        //unify(t, .FUN(es.map { infer(&env, $0) }, .UNIT)) ?? fatalError("type mismatch")
        return t

    case .CMP(_, let l, let r): let tl = infer(&env, l), tr = infer(&env, r); return .BOOL
        //if tl == tr { return tl } else { return .UNIT }
    case .COND(let pred, let ifThen, let ifElse):
        let tp = infer(&env, pred), te1 = infer(&env, ifThen), te2 = infer(&env, ifElse)
        //if tp != .BOOL { return .UNIT }
        //if te1 == te2 { return te1 } else { return .UNIT }
        return te2
    default : return .UNIT
    }
}
//let a = Syntax.INT(10)
//let b = Syntax.FLOAT(3.14)
//var env: [String: Ty] = [:]
//infer(&env, .UNIT)
//infer(&env, a)
//infer(&env, b)
//infer(&env, .ADD(a, b))
//infer(&env, .SUB(a, b))
//infer(&env, .LET(Ident.newvar("a"), .FLOAT(1),
//                 .LET(Ident.newvar("b"), .FLOAT(2),
//                      .ADD(.VAR("a"), .VAR("b")))))
//var s = Syntax.LETREC(Fundef("f", [.VAR("x")], .ADD(.VAR("x"), .INT(1))), .APP(.VAR("f"), [.INT(10)]))
//infer(&env, s)
//var l = Syntax.LET(Ident("x"), .FLOAT(1), .VAR("x"))
//infer(&env, l)
//l
//env


func eval(_ x: Syntax, _ env: inout [String: Syntax]) -> Int {
    switch x {
    case .punct(let s): fatalError(s)

    case .UNIT: return 0
    case .BOOL(let b): return b ? 1 : 0
    case .INT(let n): return n
    case .ADD(let l, let r): return eval(l, &env) + eval(r, &env)
    case .SUB(let l, let r): return eval(l, &env) - eval(r, &env)
    case .MUL(let l, let r): return eval(l, &env) * eval(r, &env)
//    case .DIV(let l, let r): let el = eval(l), er = eval(r); if el % er == 0 { return el / er } else { .FLOAT(el / er)}
    case .VAR(let n): if let v = env[n] { return eval(v, &env) } else { fatalError("undefined variable \(n)") }
    case .LET(let n, let v, let x): env[n.name] = .INT(eval(v, &env)); return eval(x, &env)
    default: fatalError("unhandled")
    }
}

let ps = Parser()
//let ast = ps.parser.parse("8+2*3-1", "expr")
//let r = ps.parser.parseRule(" 8  - 2   * ( 5 +  3 ) *   4", .ref("add"))
//let r = ps.parser.parseRule("let x = 123 in let y = 23 in x - y + 1", .ref("expr"))
let r = ps.parser.parseRule("let rec f x = x in f 100", .ref("letrec"))
//let r = ps.parser.parseRule("f 100 2", .ref("expr"))
