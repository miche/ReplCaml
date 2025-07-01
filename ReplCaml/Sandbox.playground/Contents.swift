import Foundation

func test1() {
    let s: Syntax = .d("a", .i(2), in:
            .f("f", ["x"], .a(.m(.v("x"), .v("a")), .i(1)), in:
                    .a(.x(.v("f"), [.i(3)]), .m(.v("a"), .i(5)))))
    let t = Typing(s)
    print(s)
    let k = KNormal(s)
    print(k.k)

//    var e: [String: KNormalT] = [:]
//    ev(k.k, &e)
//    e
}

func ev(_ x: KNormalT, _ env: inout [String: KNormalT]) -> KNormalT {
    switch x {
    case .INT(let i): return x
    case .ADD(let l, let r): return .INT(env[l]!.to_i! + env[r]!.to_i!)
    case .MUL(let l, let r): return .INT(env[l]!.to_i! * env[r]!.to_i!)
    case .LET(let n, let v, let e): env[n.name] = ev(v, &env); return ev(e, &env)
    case .LETREC(let n, let a, let b, let e): env[n.name] = b; return ev(e, &env)
    case .APP(let f, let args):
        let x = env[f]!;
        var cl = env;
        // args.forEach { cl[$0.name] = ev($0, &cl) }; // TODO
        return ev(x, &cl)
    default: return .UNIT
    }
}
test1()



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
