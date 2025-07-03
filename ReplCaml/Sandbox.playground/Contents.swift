import Foundation

var x: Set<String> = ["aaa"]
print(Array(x))


// let rec quad x = let rec dbl x = x + x in dbl (dbl x) in quad 123

let k: KNormalT = .f("quad", ["x"],
                     .f("dbl", ["y"], .a("y", "y"), in: .d("t1", .x("dbl", ["x"]), in: .x("dbl", ["t1"]))),
                     in: .d("t2", .i(123), in:.x("quad", ["t2"])))
let t = Closure(k)
print(t.e)

func test5() {
    let k: KNormalT = .d("x", .i(3), in: .a("x", "y"))
  //  let k: KNormalT = .LET(IdentX("x", .INT), .INT(3), .ADD("x", "y"))
    print(k.fv)
}
test5()

func test4() {
    let s: Syntax = .f("f", ["x"], .a(.v("x"), .i(123)), in: .d("x", .i(456), in: .x(.v("f"), [.v("x")])))
    let t = Typing(s)
    print(s)
    let k = KNormal(s)
    print(k.k)
    let a = Alpha(k.k)
    print(a.k)
    print(a.env)
}
test4()

func test3() {
    let s: Syntax = .d("x", .i(123), in: .d("x", .i(456), in: .a(.v("x"), .v("x"))))
    let t = Typing(s)
    print(s)
    let k = KNormal(s)
    print(k.k)
    let a = Alpha(k.k)
    print(a.k)
    print(a.env)
}
test3()

func test2() {
    let s: Syntax = .f("f", ["x", "y"], .a(.m(.v("x"), .v("y")), .i(1)), in: .x(.v("f"), [.i(2), .i(3)]))
    let t = Typing(s)
    print(s)
    let k = KNormal(s)
    print(k.k)
}
test2()

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
