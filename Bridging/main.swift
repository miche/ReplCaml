import Foundation

var mlir = MLIRKit()
mlir.generateIR()

struct Parser {
    typealias AST = ParsedExpr
    /*
     Expr    <- OpExpr
     OpExpr  <- Term Rest?
     Rest    <- Op OpExpr
     Term    <- Var / Lit
     Op      <- '+' / '-' / '*' / '/'
     Lit     <- [0-9]+
     */
    /*
     Expr          <- PrefixOpExpr
     PrefixOpExpr  <- Prefix* Primary Suffix?
     Prefix        <- Op
     Suffix        <- Op PrefixOpExpr
     Primary       <- '(' Expr ')' / Var / Lit
     Op            <- '+' / '-' / '*' / '/'
     Var           <- [a-zA-Z_][a-zA-Z0-9_]*
     Lit           <- [0-9]+

     H             <- '⌿'
     Dop           <- '¨'
     Mop           <- '⍤'
     Jot           <- '∘'

     Exprs         <- Expr ('⋄' Expr)*
     OpOrDfn       <- Op / Dfn
     Dfn           <- '{' DfnExprs '}'
     PrimaryOrARef <- Primary ('[' Expr ']')*
     Primary2      <- Primary / 'string'

     S ← E ('⋄' E)*
     E ← A D A / M A / A
     D ← DF DOP / DF '.' DF / DF
     M ← MF MOP / A '∘' D / D ∘ A / MF
     DOP ← [¨⍨&] / '{' '⍺⍺' '⍵⍵' '}'
     MOP ← [⍣⍠⍤⌸] / '{' '⍵⍵' '}'
     DF ← '{' '⍺' '⍵' !('⍺⍺' '⍵⍵') '}' / F
     MF ← '{' '⍵' !('⍺' '⍺⍺' '⍵⍵') '}' / F
     F ← [+-×÷...] / '⎕'[a-zA-Z0-9]+ / [a-zA-Z0-9]+
     A ← (n / s / v)+
     v ← '⍺' / '⍵' / [a-zA-Z_][a-zA-Z0-9_]*
     s ← ''' [^']* '''
     n ← [0-9]+(\.[0-9])?

     */
    let lib: [String: PEGRule<AST>] = [
        "start": .ref("expr"),
        "expr": .choice([.ref("expr-op")], thru),
        "expr-op": .sequence([.zeroMore([.ref("op")], cmp), .ref("expr-term"), .opt([.ref("expr-suffix")], cmp)], op),
        "expr-suffix": .sequence([.ref("op"), .ref("expr-op")], thru),
        "expr-term": .choice([.ref("number"), .ref("variable")], thru),

        "paren": .sequence([term(/\(/), .ref("expr"), term(/\)/)], fst),
        "number": .terminal(/\d+(?:\.\d+)?(?:J\d+(?:\.\d+)?)?\b/, {.number($0)}),
        "variable": .terminal(/[a-zA-Z_][a-zA-Z0-9_]*\b/, {.variable($0)}),
        "op": .terminal(/[^a-zA-Z0-9_]/, {.punct($0)}),
    ]
    static private func op(_ ast: [AST]) -> [AST] {
        let prefix = ast[0].to_a, term = ast[1], suff = ast[2].to_a
        if suff.isEmpty {
            if prefix.isEmpty { return [term] }
            else { return [prefix.reversed().reduce(term) {acc, t in .unary(t.to_s, acc) }] }
        } else { return [prefix.reversed().reduce(.binary(suff[0].to_s, term, suff[1])) {acc, t in .unary(t.to_s, acc) }] }
    }
    static func thru(_ ast: [AST]) -> [AST] { return ast }
    static func fst(_ ast: [AST]) -> [AST] { return [ast[0]] }
    static func cmp(_ ast: [AST]) -> [AST] { return [.composite(ast)] }
    static func thruOr(_ f: @escaping ([AST])->AST) -> ([AST])->[AST] { return { ast in ast.count == 1 ? ast : [f(ast)]} }
    static func term(_ r: Regex<Substring>) -> PEGRule<AST> { .terminal(r, { str in .punct(str) }) }
    static func seq(_ r: [PEGRule<AST>]) -> PEGRule<AST> { .sequence(r, { ast in ast }) }

    let parser: PEGParser<AST>
    init() { parser = PEGParser<AST>(lib) }
    func parse(_ input: String, _ top: String = "start") -> PEGResult<AST> { return parser.parse(input, top) }
}

indirect enum ParsedExpr: Equatable, CustomStringConvertible {
    case number(String)                              // "42" or "3.14"
    case variable(String)                            // x
    case unary(String, ParsedExpr)                      // -e
    case binary(String, ParsedExpr, ParsedExpr) // e1 + e2, etc.
    case lambda(String, ParsedExpr)     // λx. e
    case application(ParsedExpr, ParsedExpr) // e1 e2

    case punct(String)
    case composite([ParsedExpr])
    var to_s: String { if case .punct(let s) = self { return s } else { fatalError() } }
    var to_a: [ParsedExpr] { if case .composite(let xs) = self { return xs } else { fatalError() } }
    var description: String {
        switch self {
        case .number(let s): return s
        case .variable(let s): return s
        case .unary(let op, let e): return "(\(op) \(e))"
        case .binary(let op, let lhs, let rhs): return "(\(lhs) \(op) \(rhs))"

        case .lambda(let varName, let e): return "(\(varName) -> \(e))"
        case .application(let lhs, let rhs): return "(\(lhs) \(rhs))"
        case .punct(let s): return s
        case .composite(let xs): return "[\(xs.map(\.description).joined(separator: " "))]"
        }
    }
}

func test() {
    let codes: [String] = [
        //    "1", "-1",
        "1+2", "1+*2", "--(1-!2)+#$3.13", "÷×1 * !*3J2 + *4"]
    for code in codes {
        print(code)
        print(Parser().parse(code).ast?.first ?? "syntax error")
    }
}
//print(Parser().parse("1+-1").ast?.first ?? "syntax error")
//
//indirect enum Typ: Equatable, CustomStringConvertible {
//    case int
//    case float
//    case arrow(Typ, Typ)
//    case tvar(String)
//
//    class TypeVarGenerator {
//        nonisolated(unsafe) static var count: Int = 0
//        static func next() -> String {
//            var num = count
//            let alphabet = Array("abcdefghijklmnopqrstuvwxyz")
//            let base = alphabet.count
//            var result = ""
//
//            repeat {
//                let remainder = num % base
//                result = String(alphabet[remainder]) + result
//                num = num / base
//            } while num > 0
//
//            count += 1
//            return result
//        }
//    }
//    static func freshTypeVariable() -> Typ {
//        return .tvar(TypeVarGenerator.next())
//    }
//    func substitute(_ mapping: [String: Typ]) -> Typ {
//        switch self {
//        case .arrow(let t1, let t2): return .arrow(t1.substitute(mapping), t2.substitute(mapping))
//        case .tvar(let n): return mapping[n] ?? self
//        default: return self
//        }
//    }
//    var description: String {
//        switch self {
//        case .int: return "Int"
//        case .float: return "Float"
//        case .arrow(let t1, let t2): return "\(t1)→\(t2)"
//        case .tvar(let n): return "'\(n)"
//        }
//    }
//}
//
//indirect enum TypedExpr: CustomStringConvertible {
//    case intLiteral(Int)
//    case floatLiteral(Double)
//    case variable(String, Typ)
//    case unary(op: UnaryOp, operand: TypedExpr, type: Typ)
//    case binary(op: BinaryOp, lhs: TypedExpr, rhs: TypedExpr, type: Typ)
//    case lambda(param: String, paramType: Typ, body: TypedExpr, type: Typ)
//    case application(funcExpr: TypedExpr, argExpr: TypedExpr, type: Typ)
//
//    var type: Typ {
//        switch self {
//        case .intLiteral: return .int
//        case .floatLiteral: return .float
//        case .variable(_, let t): return t
//        case .unary(_, _, let t): return t
//        case .binary(_, _, _, let t): return t
//        case .lambda(_, _, _, let t): return t
//        case .application(_, _, let t): return t
//        }
//    }
//
//    var description: String {
//        switch self {
//        case .intLiteral(let i): return "\(i)"
//        case .floatLiteral(let f): return "\(f)"
//        case .variable(let name, let t): return "\(name): \(t)"
//        case .unary(op: let op, operand: let o, type: let t): return "(\(op))\(o): \(t)"
//        case .binary(op: let op, lhs: let l, rhs: let r, type: let t): return "(\(l): \(t))\(op)(\(r): \(t)): \(t)"
//        case .lambda(param: let p, paramType: let pt, body: let b, type: let t): return "λ\(p): \(pt) -> \(b): \(t)"
//        case .application(funcExpr: let f, argExpr: let a, type: let t): return "(\(f): \(t))\(a): \(t)"
//        }
//    }
//}
//
//enum UnaryOp {
//    case conjugate, negate, direction, reciprocal, exponential, nlog, pitimes, factorial
//    init (_ op: String) throws {
//        let mapping: [String: UnaryOp] = [
//            "+": .conjugate, "-": .negate, "×": .direction,
//            "÷": .reciprocal, "*": .exponential, "⍟": .nlog,
//            "○": .pitimes, "!": .factorial]
//        if let t = mapping[op] { self = t } else { throw TypeError.unknownOperator(op) }
//    }
//    var to_s: String {
//        let mapping: [UnaryOp: String] = [
//            .conjugate: "conjugate", .negate: "negate", .direction: "direction",
//            .reciprocal: "reciprocal", .exponential: "exponential", .nlog: "nlog",
//            .pitimes: "pitimes", .factorial: "factorial"]
//        return mapping[self]!
//    }
//}
//enum BinaryOp {
//    case plus, minus, times, divide, power, log, circle, binomial
//    init (_ op: String) throws {
//        let mapping: [String: BinaryOp] = [
//            "+": .plus, "-": .minus, "×": .times,
//            "÷": .divide, "*": .power, "⍟": .log,
//            "○": .circle, "!": .binomial]
//        if let t = mapping[op] { self = t } else { throw TypeError.unknownOperator(op) }
//    }
//    var to_s: String {
//        let mapping: [BinaryOp: String] = [
//            .plus: "plus", .minus: "minus", .times: "times",
//            .divide: "divide", .power: "power", .log: "log",
//            .circle: "circle", .binomial: "binomial"]
//        return mapping[self]!
//    }
//}
//
//struct TypeScheme {
//    let typeVariables: [String]
//    let baseType: Typ
//    // ∀α. α → α : TypeScheme(typeVariables: ["a"], baseType: .arrow(.tvar("a"), .tvar("a")))
//}
//typealias TypeEnv = [String: TypeScheme]
//
//func instantiate(_ scheme: TypeScheme) -> Typ {
//    var subst: [String: Typ] = [:]
//    for name in scheme.typeVariables {
//        subst[name] = .freshTypeVariable()
//    }
//    return scheme.baseType.substitute(subst)
//}
//
//func generalize(_ env: TypeEnv, _ ty: Typ) -> TypeScheme {
//    let envFreeVars = env.values.flatMap { $0.freeTypeVariables() }.uniqued()
//    let tyFreeVars = ty.freeTypeVariables()
//    let generalized = tyFreeVars.filter { !envFreeVars.contains($0) }
//    return TypeScheme(typeVariables: generalized, baseType: ty)
//}
//
//enum TypeError: Error {
//    case invalidLiteral(String)
//    case unboundVariable(String)
//    case unknownOperator(String)
//    case typeMismatch(expected: Typ, actual: Typ)
//    case expectedFunction(Typ)
//}
//
//func inferType(_ expr: ParsedExpr, env: TypeEnv) throws -> TypedExpr {
//    switch expr {
//    case .number(let s):
//        if let i = Int(s) {
//            return .intLiteral(i)
//        } else if let f = Double(s) {
//            return .floatLiteral(f)
//        } else {
//            throw TypeError.invalidLiteral(s)
//        }
//
//    case .variable(let name): return .variable(name, try env.lookup(name))
//
//    case .unary(let op, let e):
//        let te = try inferType(e, env: env)
//        return .unary(op: try UnaryOp(op), operand: te, type: te.type)
//
//    case .binary(let op, let lhsExpr, let rhsExpr):
//        let lhs = try inferType(lhsExpr, env: env)
//        let rhs = try inferType(rhsExpr, env: env)
//        let resultType = (lhs.type == .float || rhs.type == .float) ? Typ.float : Typ.int
//        let lhsC = try coerce(lhs, to: resultType)
//        let rhsC = try coerce(rhs, to: resultType)
//        return .binary(op: try BinaryOp(op), lhs: lhsC, rhs: rhsC, type: resultType)
//
//    case .lambda(let param, let body):
//        let paramType: Typ = .int // default type
//        let extendedEnv = env.extending(param, with: paramType)
//        let typedBody = try inferType(body, env: extendedEnv)
//        return .lambda(param: param, paramType: paramType, body: typedBody, type: .fn(paramType, typedBody.type))
//
//    case .application(let f, let x):
//        let tf = try inferType(f, env: env)
//        let tx = try inferType(x, env: env)
//        guard case .fn(let paramType, let resultType) = tf.type else { throw TypeError.expectedFunction(tf.type) }
//        let arg = try coerce(tx, to: paramType)
//        return .application(funcExpr: tf, argExpr: arg, type: resultType)
//    }
//}
//
//func coerce(_ e: TypedExpr, to type: Typ) throws -> TypedExpr {
//    if e.type == type { return e }
//    switch (e, type) {
//    case (.intLiteral(let i), .float): return .floatLiteral(Double(i))
//    case (.variable(let name, .int), .float): return .variable(name, .float)
//    default: throw TypeError.typeMismatch(expected: type, actual: e.type)
//    }
//}
//
//enum LiteralValue {
//    case int(Int)
//    case float(Double)
//}
//indirect enum TypedLambdaExpr: CustomStringConvertible {
//    case variable(String, Typ)
//    case literal(LiteralValue, Typ)
//    case apply(TypedLambdaExpr, TypedLambdaExpr, Typ)
//    case lambda(String, Typ, TypedLambdaExpr, Typ)
//    case builtin(String, Typ) // e.g., "add", "negate"
//
//    var t: Typ {
//        switch self {
//        case .variable(_, let t): return t
//        case .literal(_, let t): return t
//        case .apply(_, _, let t): return t
//        case .lambda(_, _, _, let t): return t
//        case .builtin(_, let t): return t
//        }
//    }
//    var description: String {
//        switch self {
//        case .variable(let name, let t): return "(\(name): \(t))"
//        case .literal(let v, let t): return "\(v)/\(t)"
//        case .apply(let lhs, let rhs, let t): return "(\(t) \(lhs) \(rhs))"
//        case .lambda(let name, let t, let body, let r): return "λ/\(r) \(name)/\(t) = \(body)"
//        case .builtin(let name, let t): return "\(name)/\(t)"
//        }
//    }
//}
//
//func lower(_ expr: TypedExpr) -> TypedLambdaExpr {
//    switch expr {
//    case .intLiteral(let i): return .literal(.int(i), .int)
//    case .floatLiteral(let f): return .literal(.float(f), .float)
//    case .variable(let name, let t): return .variable(name, t)
//    case .unary(let op, let w, let t): return .apply(.builtin(op.to_s, .fn(t, t)), lower(w), t)
//    case .binary(let op, let a, let w, let t): return .apply(.apply(.builtin(op.to_s, .fn(t, .fn(t, t))), lower(a), .fn(t, t)), lower(w), t)
//    case .lambda(let arg, let arg_t, let body, let t): return .lambda(arg, arg_t, lower(body), t)
//    case .application(let f, let x, let t): return .apply(lower(f), lower(x), t)
//    }
//}
//
//let parsed = ParsedExpr.lambda(param: "x",
//                               body: .binary(op: "+",
//                                             lhs: .unary(op: "-", .number("3.5")),
//                                             rhs: .variable("x")))
//
//let env = TypeEnv(variables: ["x": .float])
//let typed = try inferType(parsed, env: env)
//let lowered = lower(typed)
//print(lowered)
//print(env)

//print(Exp.LET("f", .APP(.VAR("-"), .LIT(1)), .APP(.VAR("f"), .LIT(2))).eval(&env).intValue)

/*
 enum Value {
 case int(Int)
 case closure(String, Exp, [String: Value])
 //    case ambiv((Value) -> Value, (Value, Value) -> Value)
 case builtin((Value) -> Value)
 case builtin2((Value, Value) -> Value)
 var intValue: Int { switch self { case .int(let x): return x; default: fatalError() } }
 }

 indirect enum Exp {
 case literal(Int)
 case variable(String)
 case apply(Exp, Exp)
 case lambda(String, Exp)
 case letrec(String, Exp, in: Exp)

 func eval(_ env: inout [String: Value]) -> Value {
 switch self {
 case .literal(let x): return .int(x)
 case .variable(let x): return env[x]!
 case .apply(let fn, let arg):
 let fnVal = fn.eval(&env)
 let argVal = arg.eval(&env)
 switch fnVal {
 //            case .ambiv(let m, let d):
 case .builtin(let f): return f(argVal)
 case .builtin2(let f): return .builtin { snd in f(argVal, snd) }
 case .closure(let param, let body, var cl): cl[param] = argVal; return body.eval(&cl)
 default: fatalError()
 }
 case .lambda(let arg, let body): return .closure(arg, body, env)
 case .letrec(let v, let e, let body): env[v] = e.eval(&env); return body.eval(&env)
 }
 }
 }

 var env: [String: Value] = ["-@": .builtin {x in .int(Int(-x.intValue))},
 "@-@": .builtin2 {x, y in .int(x.intValue - y.intValue)}]
 print(Exp.letrec("f", .lambda("x", .apply(.variable("-"), .variable("x"))), in: .apply(.variable("f"), .literal(2))).eval(&env).intValue)


 */

/*

 
indirect enum Exp: CustomStringConvertible {
    enum Literal { case U; case B(Bool); case I(Int); case D(Double); case F }
    enum Typ { case U; case B; case I; case D; case F; case X }
    struct Ident { var name: String; var typ: Typ }

    // a lambda expression
    case LIT(Literal)    // built-in constants
    case VAR(Ident)      // variable names
    case APP(Exp, Exp)   // applications
    case LAM(Ident, Exp) // lambda abstractions

    static func i(_ value: Int) -> Exp { .LIT(.I(value)) }
    static func v(_ name: String) -> Exp { .VAR(Ident(name: name, typ: .X)) }
    static func l(_ name: String, _ a: Exp) -> Exp { .LAM(Ident(name: name, typ: .F), a) }

    static func u() -> Ex { .LIT(.U) }
    static func b(_ value: Bool) -> Ex { .LIT(.B(value)) }
    static func d(_ value: Double) -> Ex { .LIT(.D(value)) }

    static func jot(_ aa: Ex, _ op: Ex) -> Ex {
        return .LAM("w", .APP(aa, .APP(op, .v("w"))))
    }
    // dyadic -> monadic
    static func a_op(_ a: Ex, _ op: Ex) -> Ex {
        return .LAM("a", .APP(.APP(op, a), .v("a")))
    }
    static func op_w(_ op: Ex, _ w: Ex) -> Ex {
        return .LAM("w", .APP(.APP(op, .v("w")), w))
    }
    // dyadic -> niladic
    static func a_op_w(_ a: Ex, _ op: Ex, _ w: Ex) -> Ex {
        return .LAM("w", .LAM("a", .APP(.APP(op, .v("a")), .v("w"))))
    }
    // monadic -> niladic
    static func niladic(_ op: Ex, _ w: Ex) -> Ex {
        return .LAM("w", .APP(op, w))
    }

    var description: String {
        switch self {
        case .VAR(let name): return name.name
        case .LIT(let value): return "\(value)"
        case .APP(let fn, let arg): return "(\(fn) \(arg))"
        case .LAM(let arg, let body): return "λ\(arg).\(body)"
        }
    }
    enum Res {
        case I(Exp)
        case R(Literal)
    }
    func eval(_ env: inout [String: Exp], _ e: Exp) -> Res {
        switch self {
        case .LIT(let x): return .R(x)
        case .VAR(let x): return .I(env[x.name]!)
        case .APP(let fn, let arg): let f = eval(&env, fn); var e2 = e;
        case .LAM(let n, let fn): return .I(self) // TODO: ⍺conv
        }
    }
    enum Ev {
        case DYADIC((Exp, Exp) -> Exp)
//        case MONADIC(Ex)
    }
}

let m = Exp.l("w", .APP(.v("+"), .v("w")))
let x = Exp.APP(m, .i(2))



//let d = Exp.LAM("w", .LAM("a", .APP(.APP(.v("+"), .v("a")), .v("w"))))
//let y = Exp.APP(.APP(d, .i(1)), .i(2))
//
//var e = [String: Exp.Ev]()
//e["+"] = .DYADIC({ a, w in a.eval(&e) + w.eval(&e) })
//x.eval(&e, .LIT(.I(1)))

//let expr = Ex.APP(.APP(.VAR("-"), .APP(.APP(.VAR("+"), .LIT(1)), .APP(.APP(.VAR("*"), .LIT(2)), .LIT(3)))), .LIT(4))



let emit = Emit()

//let fn = llvm.emitfunc("main")
// return impl->appendbb(impl->func(name, impl->i32_t, llvm::Function::ExternalLinkage), name);
//let run = llvm.emitcall(pg, "calltmp")
// return impl->call(callee, {}, name);
//llvm.emitret(run)
// return impl->ret(value);

print(emit.dump())

let adder = llvm.makecls("adder", "y.7")
let y = llvm.arg(adder, 0)
let x = llvm.closure_arg(adder, 0, "x.5")
_ = llvm.ans(llvm.add(x, y))

let make_adder = llvm.makecls("make_adder", "x", true) // true: returns ptr instead of i32
let x1 = llvm.arg(make_adder, 0)
_ = llvm.ans(llvm.makeclosure(adder, x1))

let pg = llvm.entry("min_caml_start")
let i1 = llvm.set(".i1.9", 3)
let f2 = llvm.calldir(".f2.8", make_adder, i1)
let i3 = llvm.set(".i3.10", 7)
_ = llvm.ans(llvm.callcls(f2, i3))

let fn = llvm.emitfunc("main")
let run = llvm.emitcall(pg, "calltmp")
llvm.emitret(run)

llvm.dump()
*/
// opt -passes="mem2reg,instcombine,simplifycfg,
    //opt -passes="globalopt,globaldce,adce,dce,simplifycfg" ccc.ll -S -o test_dce_aggressive.ll

