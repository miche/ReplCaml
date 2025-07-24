import Foundation
import Combine
import CxxStdlib

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

final class Opt: CustomStringConvertible, Hashable {
//final class Opt: Equatable, CustomStringConvertible, Hashable {
    nonisolated(unsafe) private static var counter = 0
    nonisolated(unsafe) private static var env: [Opt: String] = [:]

    var v: Typ?
    var t: String?
    init(_ v: Typ? = nil) {
        self.v = v
    }
    var tname: String  {
        get {
            if v != nil { return v!.description }
            else if t == nil { t = Opt.new_var(self) }
            return t!
        }
    }
    func update(_ v: Typ) {
        self.v = v
    }
    static func == (lhs: Opt, rhs: Opt) -> Bool {
        return lhs.v == rhs.v && lhs.tname == rhs.tname
    }
    static func new_var(_ t: Opt) -> String {
        var num = counter
        counter += 1
        let alphabet = Array("abcdefghijklmnopqrstuvwxyz")
        let base = alphabet.count
        var result = ""

        repeat {
            let remainder = num % base
            result = String(alphabet[remainder]) + result
            num = num / base
        } while num > 0
        let xt = "'\(result)"

        return xt
    }
    var description: String {
        return v?.description ?? tname
/*
let rec print_int x = x in
let rec f x = x + 123 in
let rec g y = f in
print_int ((g 456) 789)
*/
    }
    func hash(into hasher: inout Hasher) {
        hasher.combine(v ?? Typ.UNIT)
    }
}

struct TypS: CustomStringConvertible, Equatable { // type scheme
    typealias TS = Set<Opt>
    var rs_ref: TS // [Typ]
    var t: Typ

    init(_ rs_ref: TS, _ t: Typ) {
        self.rs_ref = rs_ref
        self.t = t
    }
    init(_ t: Typ) {
        self.init([], t)
    }
    var description: String {
        return "\(rs_ref.map(\.description).joined(separator: ". "))\n  \(t)"
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

    static func map_var(_ f: (Opt)->Typ, _ e: Typ) -> Typ {
        switch e {
        case .FUN(let t1s, let t2, let t3, let t4):
            return .FUN(t1s.map { map_var(f, $0) }, map_var(f, t2), map_var(f, t3), map_var(f, t4))
        case .TUPLE(let ts): return .TUPLE(ts.map { map_var(f, $0) })
        case .ARRAY(let t): return .ARRAY(map_var(f, t))
        case .LIST(let t): return .LIST(map_var(f, t))
        case .VAR(let r): if let t = r.v { return map_var(f, t) } else { return f(r) }
        default: return e
        }
    }

    static func subst(_ r: Opt, _ ra: Typ, _ t: Typ) -> Typ {
        map_var({ r0 in if r == r0 { return ra } else { return .VAR(r0) } }, t)
    //  let subst r r' = map_var (fun r0 -> if r == r0 then r' else Var (r0))
    }

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
        case .VAR(let r): return r.description
        }
    }
    var to_ts: TypS { TypS(self) }
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
        self.init(name.to_s)
    }
    init(_ x: IdentS) {
        self.init(x.name, x.typ.t)
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
        self.init(name.to_s, TypS([], Typ.gentyp()))
    }
    var description: String { "(\(name) : \(typ))" }
}

indirect enum Syntax: CustomStringConvertible, Equatable {
    case UNIT
    case BOOL(Bool)
    case INT(Int)
    case FLOAT(Double)

    case NOT(Syntax)
    case NEG(Syntax)
    case FNEG(Syntax)
    case ADD(Syntax, Syntax)
    case SUB(Syntax, Syntax)
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
    case MATCH(Syntax, Syntax, Pattern)
    case SHIFT(IdentS, Syntax, Typ)
    case RESET(Syntax, Typ)

    // temporary containers
    case fundef(Fundef) // LETREC
    case pattern(Pattern)
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

        case .IF(let e1, let e2, let e3): return "if \(e1)\n  then \(e2) \n  else \(e3)"
        case .LET(let xt, let e1, let e2): return "let \(xt) = \(e1) in\n  \(e2)"
        case .VAR(let x): return x
        case .LETREC(let fd, let e2):
            let ats = fd.args.map(\.description).joined(separator: " ")
            return "let rec \(fd.name) \(ats) =\n  \(fd.body) in\n\n  \(e2)"
        case .APP(let e, let es): return "(\(e) \(es.map(\.description).joined(separator: " ")))"
        case .TUPLE(let es): return "(\(es.map(\.description).joined(separator: ", ")))"
        case .LETTUPLE(let xts, let e1, let e2): return "let (\(xts.map(\.description).joined(separator: ", "))) = \(e1) in\n  \(e2)"
        case .ARRAY(let e1, let e2): return "Array.create \(e1) \(e2)"
        case .GET(let e1, let e2): return "\(e1).(\(e2))"
        case .PUT(let e1, let e2, let e3): return "\(e1).(\(e2)) <- \(e3)"
        case .EMPTY: return "[]"
        case .CONS(let e1, let e2): return "\(e1) :: \(e2)"  // TODO: loop [e1; e2] cons->list
        case .MATCH(let e1, let e2, let pat):
            return "match \(e1) with \n    [] -> \(e2)\n\(pat)"
        case .SHIFT(let xts, let e, _): return "shift (fun \(xts) -> \(e))"
        case .RESET(let e, _): return "<\(e)>"

        case .punct(let s): return "*punct:\(s)*"
        case .composite(let xs): return xs.map(\.description).joined(separator: " ")
        case .fundef(_): return "*fundef*"
        case .pattern(let pat): return "  | \(pat.xt) :: \(pat.yt) -> \(pat.e3)"
        }
    }

    var to_s: String { if case .punct(let s) = self { return s } else { fatalError() } }
    var to_a: [Syntax] { if case .composite(let xs) = self { return xs } else { fatalError() } }
    var to_fd: Fundef { if case .fundef(let fd) = self { return fd } else { fatalError() } }
    var to_p: Pattern { if case .pattern(let pat) = self { return pat } else { fatalError() } }
    struct Fundef: Equatable {
        var name: IdentS
        var args: [Ident]
        var body: Syntax
        init(_ name: IdentS, _ args: [Ident], _ body: Syntax) {
            self.name = name
            self.args = args
            self.body = body
        }
        init(_ name: Syntax, _ args: Syntax, _ body: Syntax) {
            self.init(IdentS(name), args.to_a.map { Ident($0) }, body)
        }
    }
    struct Pattern: Equatable {
        var xt: Ident
        var yt: Ident
        var e3: Syntax
        init(_ xt: Ident, _ yt: Ident, _ e3: Syntax) {
            self.xt = xt
            self.yt = yt
            self.e3 = e3
        }
        init(_ xt: Syntax, _ yt: Syntax, _ e3: Syntax) {
            self.init(Ident(xt), Ident(yt), e3)
        }
    }
}

indirect enum KNormal: CustomStringConvertible, Equatable {
    case UNIT
    case INT(Int)
    case FLOAT(Double)
    case NEG(Id.T)
    case ADD(Id.T, Id.T)
    case SUB(Id.T, Id.T)
    case FNEG(Id.T)
    case FADD(Id.T, Id.T)
    case FSUB(Id.T, Id.T)
    case FMUL(Id.T, Id.T)
    case FDIV(Id.T, Id.T)
    case IFEQ(Id.T, Id.T, KNormal, KNormal)
    case IFLE(Id.T, Id.T, KNormal, KNormal)
    case LET(Ident, KNormal, KNormal)
    case VAR(Id.T)
    case LETREC(Ident, [Ident], KNormal, KNormal)
    case APP(Id.T, [Id.T])
    case TUPLE([Id.T])
    case LETTUPLE([Ident], Id.T, KNormal)
    case GET(Id.T, Id.T)
    case PUT(Id.T, Id.T, Id.T)
    case EXTARRAY(Id.T)
    case EXTFUNAPP(Id.T, [Id.T])

    var fv: Set<Id.T> {
        switch self {
        case .UNIT, .INT, .FLOAT, .EXTARRAY: return []
        case .ADD(let x, let y), .SUB(let x, let y), .GET(let x, let y): return [x, y]
        case .FADD(let x, let y), .FSUB(let x, let y), .FMUL(let x, let y), .FDIV(let x, let y): return [x, y]
        case .NEG(let x), .FNEG(let x): return [x]
        case .IFEQ(let x, let y, let e1, let e2), .IFLE(let x, let y, let e1, let e2): return e1.fv.union(e2.fv).union([x, y])
        case .LET(let xt, let e1, let e2): return e1.fv.union(e2.fv.subtracting([xt.name]))
        case .VAR(let x): return [x]
        case .LETREC(let xt, let yts, let e1, let e2): let zs = e1.fv.subtracting(yts.map(\.name)); return zs.union(e2.fv).subtracting([xt.name])
        case .APP(let x, let ys): return Set([x] + ys)
        case .TUPLE(let xs), .EXTFUNAPP(_, let xs): return Set(xs)
        case .PUT(let x, let y, let z): return [x, y, z]
        case .LETTUPLE(let xs, let y, let e): return Set([y]).union(e.fv.subtracting(xs.map(\.name)))
        }
    }

    var description: String {
        switch self {
        case .UNIT: return "()"
        case .INT(let x): return "\(x)"
        case .FLOAT(let x): return "\(x)"
        case .NEG(let x): return "-\(x)"
        case .ADD(let x, let y): return "(\(x) + \(y))"
        case .SUB(let x, let y): return "(\(x) - \(y))"
        case .FNEG(let x): return "-\(x)"
        case .FADD(let x, let y): return "(\(x) +. \(y))"
        case .FSUB(let x, let y): return "(\(x) -. \(y))"
        case .FMUL(let x, let y): return "(\(x) *. \(y))"
        case .FDIV(let x, let y): return "(\(x) /. \(y))"
        case .IFEQ(let x, let y, let z, let w): return "if \(x) = \(y)\n  then \(z)\n  else \(w)"
        case .IFLE(let x, let y, let z, let w): return "if \(x) <= \(y)\n  then \(z)\n  else \(w)"
        case .LET(let x, let y, let z): return "let \(x) = \(y) in\n  \(z)"
        case .VAR(let x): return x
        case .LETREC(let xts, let yts, let e1, let e2): return "let rec \(xts) \(yts.map(\.description).joined(separator: " ")) = \(e1) in\n  \(e2)"
        case .APP(let x, let y): return "\(x) \(y.map(\.description).joined(separator: " "))"
        case .TUPLE(let x): return "(\(x.map(\.description).joined(separator: ", ")))"
        case .LETTUPLE(let x, let y, let z): return "let (\(x.map(\.description).joined(separator: ", "))) =\(y)in\n  \(z)"
        case .GET(let x, let y): return "\(x).(\(y))"
        case .PUT(let x, let y, let z): return "\(x).(\(y)) <- \(z)"
        case .EXTARRAY(let x): return x
        case .EXTFUNAPP(let x, let y): return "\(x) \(y.map(\.description).joined(separator: " "))"
        }
    }
}

struct Prog: CustomStringConvertible {
    var defs: [Fundef]
    var main: Closure

    struct Fundef: CustomStringConvertible {
        var name: Ident
        var args: [Ident]
        var formal_fv: [Ident]
        var body: Closure

        var description: String {
            let fvs = formal_fv.map(\.name.description)
            return "let rec \(name) [\(fvs.joined(separator: ", "))] \(fvs.joined(separator: " "))=\n  \(body))"
        }
    }
    indirect enum Closure: CustomStringConvertible {
        case UNIT
        case INT(Int)
        case FLOAT(Double)
        case NEG(Id.T)
        case ADD(Id.T, Id.T)
        case SUB(Id.T, Id.T)
        case FNEG(Id.T)
        case FADD(Id.T, Id.T)
        case FSUB(Id.T, Id.T)
        case FMUL(Id.T, Id.T)
        case FDIV(Id.T, Id.T)
        case IFEQ(Id.T, Id.T, Closure, Closure)
        case IFLE(Id.T, Id.T, Closure, Closure)
        case LET(Ident, Closure, Closure)
        case VAR(Id.T)
        case MAKECLS(Ident, Id.T, [Id.T], Closure)  // struct closure { var entry: Id.l; var actual_fv: [Id.t] }
        case APPCLS(Id.T, [Id.T])
        case APPDIR(Id.T, [Id.T])
        case TUPLE([Id.T])
        case LETTUPLE([Ident], Id.T, Closure)
        case GET(Id.T, Id.T)
        case PUT(Id.T, Id.T, Id.T)
        case EXTARRAY(Id.T)

        var fv: Set<Id.T> {
            switch self {
            case .UNIT, .INT, .FLOAT, .EXTARRAY: return []
            case .ADD(let x, let y), .SUB(let x, let y), .GET(let x, let y): return [x, y]
            case .FADD(let x, let y), .FSUB(let x, let y), .FMUL(let x, let y), .FDIV(let x, let y): return [x, y]
            case .NEG(let x), .FNEG(let x): return [x]
            case .IFEQ(let x, let y, let e1, let e2), .IFLE(let x, let y, let e1, let e2): return e1.fv.union(e2.fv).union([x, y])
            case .LET(let xt, let e1, let e2): return e1.fv.union(e2.fv.subtracting([xt.name]))
            case .VAR(let x): return [x]
            case .MAKECLS(let xt, _, let ys, let e): return e.fv.union(ys).subtracting([xt.name])
            case .APPCLS(let x, let ys): return Set([x] + ys)
            case .APPDIR(_, let xs), .TUPLE(let xs): return Set(xs)
            case .LETTUPLE(let xts, let y, let e): return e.fv.subtracting(xts.map(\.name)).union([y])
            case .PUT(let x, let y, let z): return [x, y, z]
            }
        }

        var description: String {
            switch self {
            case .UNIT: return "()"
            case .INT(let x): return "\(x)"
            case .FLOAT(let x): return "\(x)"
            case .NEG(let x), .FNEG(let x): return "-\(x)"
            case .ADD(let x, let y): return "\(x) + \(y)"
            case .SUB(let x, let y): return "\(x) - \(y)"
            case .FADD(let x, let y): return "\(x) +. \(y)"
            case .FSUB(let x, let y): return "\(x) -. \(y)"
            case .FMUL(let x, let y): return "\(x) *. \(y)"
            case .FDIV(let x, let y): return "\(x) /. \(y)"
            case .IFEQ(let x, let y, let e1, let e2): return "if \(x) = \(y)\n  then \(e1)\n  else \(e2)"
            case .IFLE(let x, let y, let e1, let e2): return "if \(x) <= \(y)\n  then \(e1)\n  else \(e2)"
            case .LET(let xt, let e1, let e2): return "let \(xt) = \(e1) in\n  \(e2)"
            case .VAR(let x): return x
            case .MAKECLS(let xts, let yt, let e1, let e2): return "let rec \(xts) \(yt) = \(e1) in\n  \(e2)"
            case .APPCLS(let x, let y): return "\(x) \(y.map(\.description).joined(separator: " "))"
            case .APPDIR(let x, let y): return "\(x) \(y.map(\.description).joined(separator: " "))"
            case .TUPLE(let x): return "(\(x.map(\.description).joined(separator: ", ")))"
            case .LETTUPLE(let xt, let y, let e): return "(\(xt.map(\.description).joined(separator: ", "))) = \(y) in\n  \(e)"
            case .GET(let x, let y): return "\(x).(\(y))"
            case .PUT(let x, let y, let z): return "\(x).(\(y)) <- \(z)"
            case .EXTARRAY(let x): return x
            }
        }
    }

    var description: String {
        "{\(defs.map(\.description).joined(separator: "\n"))}\n\(main)"
    }
}

struct Asm {
    var defs: [Fundef]
    var main: T
    indirect enum T: CustomStringConvertible {
        case ANS(Exp)
        case LET(Ident, Exp, T)
        var description: String {
            switch self {
            case .ANS(let e): return "ans \(e)"
            case .LET(let xt, let e, let t): return "let \(xt) \(e) \(t)"
            }
        }
    }
    enum Exp: CustomStringConvertible {
        case NOP
        case LOADINT(Int)   // Li of int
        case LOADFLOAT(Double)   // FLi of Id.l
        case NEG(Id.T)  // Neg of Id.t
        case ADD(Id.T, Id.T)                //        | Add of Id.t * id_or_imm
        case SUB(Id.T, Id.T)                 //        | Sub of Id.t * id_or_imm
        case FNEG(Id.T)  // FNeg of Id.t
        case FADD(Id.T, Id.T)                // FAdd of Id.t * Id.t
        case FSUB(Id.T, Id.T)                 //  FSub of Id.t * Id.t
        case FMUL(Id.T, Id.T)                 //  FMul of Id.t * Id.t
        case FDIV(Id.T, Id.T)                 //   FDiv of Id.t * Id.t
        case IFEQ(Id.T, Id.T, T, T)       //        | IfEq of Id.t * id_or_imm * t * t
        case IFLE(Id.T, Id.T, T, T)       //        | IfLE of Id.t * id_or_imm * t * t
                                              //        | IfGE of Id.t * id_or_imm * t * t (* for simm *)
                                              //        | IfFEq of Id.t * Id.t * t * t
                                              //        | IfFLE of Id.t * Id.t * t * t
        case SETL(Id.T) //        | SetL of Id.l
        //        | Mr of Id.t
        //        | Slw of Id.t * id_or_imm
        case LOAD(Id.T, Id.T) //        | Lwz of Id.t * id_or_imm
        //        | Stw of Id.t * Id.t * id_or_imm
        //        | FMr of Id.t
        //        | Lfd of Id.t * id_or_imm
        //        | Stfd of Id.t * Id.t * id_or_imm
        //        | Comment of string
        case CALLCLS(Id.T, [Id.T], [Id.T]) //        | CallCls of Id.t * Id.t list * Id.t list
        case CALLDIR(Id.T, [Id.T], [Id.T]) //        | CallDir of Id.l * Id.t list * Id.t list
        //        | Save of Id.t * Id.t
        //        | Restore of Id.t
        var description: String {
            switch self {
            case .NOP: return "nop"
            case .LOADINT(let i): return "i32 \(i)"
            case .LOADFLOAT(let f): return "double \(f)"
            case .NEG(let x): return "neg i32 \(x)"
            case .ADD(let x, let y): return "add i32 \(x), i32 \(y)"
            case .SUB(let x, let y): return "sub i32 \(x), i32 \(y)"
            case .FNEG(let x): return "neg double \(x)"
            case .FADD(let x, let y): return "add double \(x), double \(y)"
            case .FSUB(let x, let y): return "sub double \(x), double \(y)"
            case .FMUL(let x, let y): return "mul double \(x), double \(y)"
            case .FDIV(let x, let y): return "div double \(x), double \(y)"
            case .IFEQ(let x, let y, let e1, let e2): return "ifeq \(x) = \(y) then \(e1) else \(e2)"
            case .IFLE(let x, let y, let e1, let e2): return "ifle \(x) <= \(y) then \(e1) else \(e2)"
            case .SETL(let x): return "setl i32 \(x)"
            case .CALLCLS(let x, let ys, _): return "call cls \(x)"
            case .CALLDIR(let x, let ys, _): return "call dir \(x)"
            default: return "" // TODO:  
            }
        }
    }
    struct Fundef: CustomStringConvertible {
        var name: Id.T
        var args: [Id.T]
        var body: Exp
        var rett: Typ

        var description: String {
            return "let rec \(name) [\(args.map(\.description).joined(separator: ", "))] =\n  \(body))"
        }
    }
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

    static private func opFold(_ ast: [Syntax], _ opMap: [String: (Syntax, Syntax) -> Syntax]) -> [Syntax] { guard ast.count > 2 else { return ast }; var result = opMap[ast[1].to_s]!(ast[0], ast[2]); for i in stride(from: 4, through: ast.count, by: 2) { result = opMap[ast[i - 1].to_s]!(result, ast[i]) }; return [result]; }
    static private func letFold(_ ast: [Syntax]) -> Syntax { var result = ast.last!; for i in stride(from: ast.count - 3, through: 0, by: -2) { result = Syntax.LET(Ident(), ast[i], result) }; return result }

    static func term(_ r: Regex<Substring>) -> PEGRule<Syntax> { .terminal(r, { str in .punct(str) }) }
    static func seq(_ r: [PEGRule<Syntax>]) -> PEGRule<Syntax> { .sequence(r, { ast in ast }) }

    let lib: [String: PEGRule<Syntax>] = [
        "start": .ref("exp"),
        "exp": .ref("exp-let"),
        "exp-let": .choice([
            .sequence([term(/let\s+rec\b/), .ref("fundef"), term(/in\b/), .ref("exp")], { [.LETREC($0[1].to_fd, $0[3])] }),
            .sequence([term(/let\s*\(/), .ref("pat"), term(/\)\s*=/), .ref("exp"), term(/in\b/), .ref("exp")], { [.LETTUPLE($0[1].to_a.map(Ident.init), $0[3], $0[5])] }),
            .sequence([term(/let\b/), .ref("ident"), term(/=/), .ref("exp"), term(/in\b/), .ref("exp")], { [.LET(Ident($0[1]), $0[3], $0[5])] }),
            .sequence([term(/if\b/), .ref("exp-semicolon"), term(/then\b/), .ref("exp-semicolon"), term(/else\b/), .ref("exp-semicolon")], { [.IF($0[1], $0[3], $0[5])] }),
            .sequence([term(/shift\s*\(\s*fun\b/), .ref("ident"), term(/\-\>/), .ref("exp"), term(/\)/)], { [.SHIFT(IdentS($0[1]), $0[3], Typ.gentyp())] }),
            .sequence([term(/reset\s*\(\s*fun\s*\(\s*\)\s*\-\>/), .ref("exp"), term(/\)/)], { [.RESET($0[1], Typ.gentyp())] }),
            .sequence([term(/match\b/), .ref("exp"), term(/with\b/), .ref("match-clauses")], { [.MATCH($0[1], $0[3], $0[4].to_p)] }),
            .ref("exp-semicolon")], thru),
        "exp-semicolon": .sequence([.ref("exp-if"), .zeroMore([term(/\;/), .ref("exp-if")], thru)], thruOr(letFold)),
        "exp-if": .ref("exp-cons"),
        "exp-cons": .sequence([.ref("exp-assign"), .opt([term(/\:\:/), .ref("exp-cons")], thru)], thruOr({.CONS($0[0], $0[2])})),
        "exp-assign": .sequence([.ref("exp-comma"), .opt([term(/\<\-/), .ref("exp-comma")], thru)], thru),
        "exp-comma": .sequence([.ref("exp-equality"), .zeroMore([term(/\,/), .ref("exp-equality")], thru)], thruOr({.TUPLE(to_a2($0))})),
        "exp-equality": .sequence([.ref("exp-add"), .zeroMore([.choice([seq([term(/\=\b/), .ref("exp-add")]), seq([term(/\<\>\b/), .ref("exp-add")]), seq([term(/\<\b/), .ref("exp-add")]), seq([term(/\>\b/), .ref("exp-add")]), seq([term(/\<\=\b/), .ref("exp-add")]), seq([term(/\>\=\b/), .ref("exp-add")]) ], thru)], thru)], { ast in opFold(ast, ["=": Syntax.EQ, "<>": atop(Syntax.NOT, Syntax.EQ), "<": atop(Syntax.NOT, flip(Syntax.LE)), ">": atop(Syntax.NOT, Syntax.LE), "<=": Syntax.LE, ">=": flip(Syntax.LE)]) }),
        "exp-add": .sequence([.ref("exp-mul"), .zeroMore([.choice([seq([term(/\+/), .ref("exp-mul")]), seq([term(/\-/), .ref("exp-mul")]), seq([term(/\+\./), .ref("exp-mul")]), seq([term(/\-\./), .ref("exp-mul")]) ], thru)], thru)], { ast in opFold(ast, ["+": Syntax.ADD, "-": Syntax.SUB, "+.": Syntax.FADD, "-.": Syntax.FSUB]) }),
        "exp-mul": .sequence([.ref("exp-unary"), .zeroMore([.choice([seq([term(/\*\./), .ref("exp-unary")]), seq([term(/\/\./), .ref("exp-unary")])], thru)], thru)], { ast in opFold(ast, ["*.": Syntax.FMUL, "/.": Syntax.FDIV]) }),
        "exp-unary": .choice([
            .sequence([term(/not\b/), .ref("exp-unary")], {[.NOT($0[1])]}),
            .sequence([term(/\-\.\b/), .ref("exp-unary")], {[.FNEG($0[1])]}),
            .sequence([term(/\-\b/), .ref("exp-unary")], {if case .FLOAT(let f) = $0[1] {return [.FLOAT(-f)]} else {return [.NEG($0[1])]}}),
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
            .sequence([term(/\|\b/), .ref("empty_clause"), term(/\|\b/), .ref("cons_clause")], train(merge, e2, e4)),
            .sequence([term(/\|\b/), .ref("cons_clause"), term(/\|\b/), .ref("empty_clause")], train(merge, e4, e2)),
            .sequence([.ref("empty_clause"), term(/\|\b/), .ref("cons_clause")], train(merge, e1, e3)),
            .sequence([.ref("cons_clause"), term(/\|\b/), .ref("empty_clause")], train(merge, e3, e1)),
        ], thru),
        "empty_clause": .sequence([term(/\[\s*\]\s*\-\>\b/), .ref("exp")], e2),
        "cons_clause": .sequence([.ref("ident"), term(/::/), .ref("ident"), term(/->/), .ref("exp")], { [.pattern(Syntax.Pattern($0[0], $0[2], $0[4]))] }),
        "reserved": term(/(?:not|if|then|else|let|in|rec|match|with|fun|shift|reset|true|false)\b/),
        "ident": .sequence([.forbid([.ref("reserved")]), term(/[A-Za-z_][A-Za-z0-9_]*/)], thru),
    ]
    var parser: PEGParser<Syntax>
    init() { parser = PEGParser<Syntax>(lib) }
    func parse(_ input: String, _ top: String = "start") -> PEGResult<Syntax> { return parser.parse(input, top) }
}

typealias Env<T> = [Id.T: T]

final class Typing {
    /* typing.ml
     struct UnifyError: Error {
     var a: Typ
     var b: Typ
     }
     struct TypingError: Error {
     // Typing
     var a: Typ
     var b: Typ
     }
     */
    nonisolated(unsafe) static var extenv: Env<Typ> = [:]
    /* typing.mli
     exception Error of Syntax.t * Type.t * Type.t
     val extenv : Type.t M.t ref
     val f : Syntax.t -> Syntax.t
     val unify : Type.t -> Type.t -> unit
     val deref_typ : Type.t -> Type.t
     */
    func deref_typ(_ t: Typ) -> Typ {
        switch t {
        case .FUN(let t1s, let t2, let t3, let t4): return .FUN(t1s.map(deref_typ), deref_typ(t2), deref_typ(t3), deref_typ(t4))
        case .TUPLE(let ts): return .TUPLE(ts.map(deref_typ))
        case .ARRAY(let ta): return .ARRAY(deref_typ(ta))
        case .VAR(let r): if let x = r.v { let t_ = deref_typ(x); r.v = t_; return t_ } else { return t}
        case .LIST(let t): return .LIST(deref_typ(t))
        default: return t
        }
    }
    func deref_ts(_ ts: TypS) -> TypS { TypS(ts.rs_ref, deref_typ(ts.t)) }
    func deref_id_typ(_ x: Ident) -> Ident { Ident(x.name, deref_typ(x.typ)) }
    func deref_id_ts(_ x: IdentS) -> IdentS { IdentS(x.name, deref_ts(x.typ)) }
    func deref_term(_ x: Syntax) -> Syntax {
        switch x {
        case .NOT(let e): return .NOT(deref_term(e))
        case .NEG(let e): return .NEG(deref_term(e))
        case .ADD(let e1, let e2): return .ADD(deref_term(e1), deref_term(e2))
        case .SUB(let e1, let e2): return .SUB(deref_term(e1), deref_term(e2))
        case .EQ(let e1, let e2): return .EQ(deref_term(e1), deref_term(e2))
        case .LE(let e1, let e2): return .LE(deref_term(e1), deref_term(e2))
        case .FNEG(let e): return .FNEG(deref_term(e))
        case .FADD(let e1, let e2): return .FADD(deref_term(e1), deref_term(e2))
        case .FSUB(let e1, let e2): return .FSUB(deref_term(e1), deref_term(e2))
        case .FMUL(let e1, let e2): return .FMUL(deref_term(e1), deref_term(e2))
        case .FDIV(let e1, let e2): return .FDIV(deref_term(e1), deref_term(e2))
        case .IF(let e1, let e2, let e3): return .IF(deref_term(e1), deref_term(e2), deref_term(e3))
        case .LET(let xt, let e1, let e2): return .LET(deref_id_typ(xt), deref_term(e1), deref_term(e2))
        case .LETREC(let fd, let e2): return .LETREC(Syntax.Fundef(deref_id_ts(fd.name), fd.args.map(deref_id_typ), deref_term(fd.body)), deref_term(e2))
        case .APP(let e, let es): return .APP(deref_term(e), es.map(deref_term))
        case .TUPLE(let es): return .TUPLE(es.map(deref_term))
        case .LETTUPLE(let xts, let e1, let e2): return .LETTUPLE(xts.map(deref_id_typ), deref_term(e1), deref_term(e2))
        case .ARRAY(let e1, let e2): return .ARRAY(deref_term(e1), deref_term(e2))
        case .GET(let e1, let e2): return .GET(deref_term(e1), deref_term(e2))
        case .PUT(let e1, let e2, let e3): return .PUT(deref_term(e1), deref_term(e2), deref_term(e3))
        case .CONS(let e1, let e2): return .CONS(deref_term(e1), deref_term(e2))
        case .MATCH(let e1, let e2, let pat): return .MATCH(deref_term(e1), deref_term(e2), Syntax.Pattern(deref_id_typ(pat.xt), deref_id_typ(pat.yt), deref_term(pat.e3)))
        default: return x
        }
    }
    func occur(_ r1: Opt, _ e: Typ) -> Bool {
        switch e {
        case .FUN(let t2s, let t2, let t3, let t4):
            return t2s.contains { occur(r1, $0) } || occur(r1, t2) || occur(r1, t3) || occur(r1, t4)
        case .TUPLE(let t2s): return t2s.contains { occur(r1, $0) }
        case .ARRAY(let t2): return occur(r1, t2)
        case .VAR(let r2):
            if r2.v == nil { return false }
            else if case .VAR(let t2) = r2.v { return occur(r1, t2.v!) }
            else { return r1 == r2 }
        case .LIST(let t2): return occur(r1, t2)
        default: return false
        }
    }
    func free_typ(_ e: Typ) -> TypS.TS {
        switch e {
        case .FUN(let t1s, let t2, let t3, let t4):
            return ([t2, t3, t4] + t1s).reduce(Set<Opt>()) { acc, t in acc.union(free_typ(t))}
        case .TUPLE(let ts): return ts.reduce(Set<Opt>()) { acc, t in acc.union(free_typ(t))}
        case .ARRAY(let t), .LIST(let t): return free_typ(t)
        case .VAR(let r): if r.v == nil { return [r] } else { return free_typ(r.v!) }
        default: return []
        }
    }
    func free_typ_s(_ x: TypS) -> Set<Opt> { free_typ(x.t).subtracting(x.rs_ref) }
    func free_typ_e(_ env: Env<TypS>) -> TypS.TS {
        var fv = TypS.TS()
        for ts in env.values { fv.formUnion(free_typ_s(ts)) }
        return fv
    }
    func gen(_ t: Typ, _ env: Env<TypS>) -> TypS {
        return TypS(free_typ(t).subtracting(free_typ_e(env)), t)
    }
    func close(_ refs: inout TypS, _ env: Env<TypS>) {
        refs.rs_ref = free_typ(refs.t).subtracting(free_typ_e(env))
    }
    func instance(_ ts: TypS) -> Typ {
        return ts.rs_ref.reduce(ts.t) { t, r in Typ.subst(r, Typ.gentyp(), t) }
        //    let instance (refs, t) =
        //    Type.S.fold (fun r t -> Type.subst r (Type.gentyp ()) t) !refs t
    }
    func unify(_ t1: Typ, _ t2: Typ) -> () {
        switch (t1, t2) {
        case (.UNIT, .UNIT), (.BOOL, .BOOL), (.INT, .INT), (.FLOAT, .FLOAT): return
        case (.FUN(let t1s, let t1a, let t1b, let t1c), .FUN(let t2s, let t2a, let t2b, let t2c)):
            zip(t1s, t2s).forEach { unify($0, $1) }
            unify(t1a, t2a); unify(t1b, t2b); unify(t1c, t2c)
        case (.TUPLE(let t1s), .TUPLE(let t2s)): zip(t1s, t2s).forEach { unify($0, $1) }
        case (.ARRAY(let t1), .ARRAY(let t2)): unify(t1, t2)
        case (.LIST(let t1), .LIST(let t2)): unify(t1, t2)
        case (.VAR(let r1), .VAR(let r2)): if r1 == r2 { return } else if r1.v != nil { unify(r1.v!, t2) } else if r2.v != nil { unify(t1, r2.v!) }
        case (.VAR(let r1), _): if r1.v != nil { unify(r1.v!, t2) } else { if occur(r1, t2) {fatalError("unification failed")} else { r1.v = t2 } }
        case (_, .VAR(let r2)): if r2.v != nil { unify(t1, r2.v!) } else { if occur(r2, t1) {fatalError("unification failed")} else { r2.v = t1 } }
        default: fatalError("unification failed")
        }
    }
    func g(_ env: inout Env<TypS>, _ e: Syntax) -> (Typ, Typ, Typ) {
        switch e {
        case .UNIT: let t = Typ.gentyp(); return (t, .UNIT, t)
        case .BOOL: let t = Typ.gentyp(); return (t, .BOOL, t)
        case .INT: let t = Typ.gentyp(); return (t, .INT, t)
        case .FLOAT: let t = Typ.gentyp(); return (t, .FLOAT, t)
        case .NOT(let e): let (t2, t, t1) = g(&env, e); unify(.BOOL, t); return (t2, .BOOL, t1)
        case .NEG(let e): let (t2, t, t1) = g(&env, e); unify(.INT, t); return (t2, .INT, t1)
        case .ADD(let e1, let e2), .SUB(let e1, let e2): let (t2, i1, t1) = g(&env, e1); unify(.INT, i1); let (t3, i2, t2a) = g(&env, e2); unify(.INT, i2); unify(t2, t2a); return (t3, .INT, t1)
        case .FNEG(let e): let (t2, t, t1) = g(&env, e); unify(.FLOAT, t); return (t2, .FLOAT, t1)
        case .FADD(let e1, let e2), .FSUB(let e1, let e2), .FMUL(let e1, let e2), .FDIV(let e1, let e2): let (t2, f1, t1) = g(&env, e1); unify(.FLOAT, f1); let (t3, f2, t2a) = g(&env, e2); unify(.FLOAT, f2); unify(t2, t2a); return (t3, .FLOAT, t1)
        case .EQ(let e1, let e2), .LE(let e1, let e2): let (t2, a, t1) = g(&env, e1); let (t3, a2, t2a) = g(&env, e2); unify(a, a2); unify(t2, t2a); return (t3, .BOOL, t1)
        case .IF(let e1, let e2, let e3): let (t1, b, t2) = g(&env, e1); unify(.BOOL, b); let (t3, a, t1a) = g(&env, e2); unify(t1, t1a); let (t3a, a2, t1b) = g(&env, e3); unify(t1, t1b); unify(t3, t3a); unify(a, a2); return (t3, a, t2)
        case .LET(let xt, let e1, let e2): let (t2, t4, t2a) = g(&env, e1); unify(xt.typ, t4); unify(t2, t2a); env[xt.name] = TypS(t4); let (t1, t3, t2b) = g(&env, e2); unify(t2, t2b); return (t1, t3, t2)
        case .VAR(let x):
            if let v = env[x] { let t = Typ.gentyp(); return (t, instance(v), t) }
            else if let v = Typing.extenv[x] { let t = Typ.gentyp(); return (t, v, t) }
            else { let t = Typ.gentyp(); let t2 = Typ.gentyp(); Typing.extenv[x] = t; return (t2, t, t2) }
        case .LETREC(let fd, let e2):
            env[fd.name.name] = fd.name.typ; var env1 = env;
            fd.args.forEach { env1[$0.name] = $0.typ.to_ts }; var env2 = env1
            let (t1, t2, t3) = g(&env2, fd.body)
            unify(fd.name.typ.t, .FUN(fd.args.map(\.typ), t1, t2, t3))
            var ref = fd.name.typ; close(&ref, env)
            return g(&env1, e2)
        case .APP(let e, let es):
            let t = Typ.gentyp(), t1 = Typ.gentyp()
            let (t2, ts, t3) = g_list(&env, es)
            let (t3a, ta, t4) = g(&env, e)
            unify(t3, t3a)
            unify(ta, .FUN(ts, t1, t, t2))
            return (t1, t, t4)
        case .TUPLE(let es):
            let (t1, ts, t2) = g_list(&env, es)
            return (t1, .TUPLE(ts), t2)
        case .LETTUPLE(let xts, let e1, let e2):
            let (t1, tlst, t2) = g(&env, e1)
            unify(.TUPLE(xts.map(\.typ)), tlst)
            xts.forEach { env[$0.name] = $0.typ.to_ts }; var env1 = env
            let (t3, t4, t1a) = g(&env1, e2)
            unify(t1, t1a)
            return (t3, t4, t2)
        case .ARRAY(let e1, let e2):
            let (t1, i, t2) = g(&env, e1)
            unify(.INT, i)
            let (t3, a, t1a) = g(&env, e2)
            unify(t1, t1a)
            return (t3, .ARRAY(a), t2)
        case .GET(let e1, let e2):
            let (t1, array, t2) = g(&env, e1)
            let a = Typ.gentyp()
            unify(.ARRAY(a), array)
            let (t3, i, t1a) = g(&env, e2)
            unify(.INT, i)
            unify(t1, t1a)
            return (t3, a, t2)
        case .PUT(let e1, let e2, let e3):
            let (t2, array, _) = g(&env, e1)
            let a = Typ.gentyp()
            unify(.ARRAY(a), array)
            let (t3, i, t2a) = g(&env, e2)
            unify(.INT, i)
            unify(t2, t2a)
            let (t4, a2, t3a) = g(&env, e3)
            unify(t3, t3a)
            unify(a, a2)
            return (t4, .UNIT, t3)
        case .EMPTY: let a = Typ.gentyp(), t = Typ.gentyp(); return (t, .LIST(a), t)
        case .CONS(let e1, let e2):
            let (t2, a, t1) = g(&env, e1)
            let (t3, alst, t2a) = g(&env, f(e2))
            unify(t2, t2a)
            unify(.LIST(a), alst)
            return (t3, alst, t1)
        case .MATCH(let e1, let e2, let pat):
            let (t2, tlst, t1) = g(&env, e1)
            unify(.LIST(pat.xt.typ), tlst)
            unify(pat.yt.typ, tlst)
            let (t3, a, t2a) = g(&env, e2)
            unify(t2, t2a)
            env[pat.yt.name] = pat.yt.typ.to_ts
            env[pat.xt.name] = pat.xt.typ.to_ts
            let (t3a, a1, t2b) = g(&env, pat.e3)
            unify(t3, t3a)
            unify(a, a1)
            unify(t2, t2b)
            return (t3, a, t1)
        case .SHIFT(let xts, let e, let ta):
            let t1 = Typ.gentyp(), t2 = Typ.gentyp()
            let r3 = Opt()
            let t3 = Typ.VAR(r3)
            unify(xts.typ.t, .FUN([t2], t3, t1, t3))
            var refs = xts.typ.rs_ref
            refs.insert(r3) // rs_ref := r3
            env[xts.name] = xts.typ
            let (t4, t4a, t5) = g(&env, e)
            unify(t4, t4a)
            unify(ta, t5)
            return (t1, t2, t5)
        case .RESET(let e, let t):
            let (t1, t1a, t2) = g(&env, e)
            unify(t1, t1a)
            let t3 = Typ.gentyp()
            unify(t, t2)
            return (t3, t2, t3)

        case .fundef, .pattern, .punct, .composite: fatalError() // unreachable
        }
    }
    struct UnifyError: Error {
        init(t1: Typ, t2: Typ) {
//            with Unify(t1, t2) ->
//            let e = deref_term e and t1 = deref_typ t1 and t2 = deref_typ t2 in
//            print_string "at ";
//            Syntax.print e; print_newline ();
//            Printf.printf "%s and %s\n" (Type.t_to_string t1) (Type.t_to_string t2);
//            raise (Error(e, t1, t2))
        }
    }
    func g_list(_ env: inout Env<TypS>, _ es: [Syntax]) -> (Typ, [Typ], Typ) {
        switch es {
        case []: let t = Typ.gentyp(); return (t, [], t)
        default:
            let (t1, a, t2) = g(&env, es.first!)
            let (t3, rest, t1a) =  g_list(&env, [Syntax](es.dropFirst()))
            unify(t1, t1a)
            return (t3, [a] + rest, t2)
        }
    }
    func f(_ e: Syntax) -> Syntax {
        Typing.extenv = [:]
        var env = Env<TypS>()
        let (_, t2, _) = g(&env, e)
        unify(.UNIT, t2)
        for (k, v) in Typing.extenv { Typing.extenv.updateValue(deref_typ(v), forKey: k) }
        return deref_term(e)
    }
    init() { }
}

struct KNormalize {

    func insert_let(_ et: (KNormal, Typ), _ k: ((Id.T)->(KNormal, Typ))) -> (KNormal, Typ) {
        if case .VAR(let x) = et.0 { return k(x) }
        else { let x = Id.gentmp(et.1); let (ea, ta) = k(x); return (.LET(Ident(x, et.1), et.0, ea), ta) }
    }
    func g(_ env: inout [String: Typ], _ s: Syntax) -> (KNormal, Typ) {
        switch s {
        case .UNIT: return (.UNIT, .UNIT)
        case .BOOL(let b): return (.INT(b ? 1 : 0), .INT)
        case .INT(let i): return (.INT(i), .INT)
        case .FLOAT(let d): return (.FLOAT(d), .FLOAT)
        case .NOT(let e): return g(&env, .IF(e, .BOOL(false ), .BOOL(true)))
        case .NEG(let e): return insert_let(g(&env, e), {(.NEG($0), .INT)})
        case .ADD(let e1, let e2): return insert_let(g(&env, e1), {x in insert_let(g(&env, e2)) {y in (.ADD(x, y), .INT)}})
        case .SUB(let e1, let e2): return insert_let(g(&env, e1), {x in insert_let(g(&env, e2)) {y in (.SUB(x, y), .INT)}})
        case .FNEG(let e): return insert_let(g(&env, e), {(.FNEG($0), .FLOAT)})
        case .FADD(let e1, let e2): return insert_let(g(&env, e1), {x in insert_let(g(&env, e2)) {y in (.FADD(x, y), .FLOAT)}})
        case .FSUB(let e1, let e2): return insert_let(g(&env, e1), {x in insert_let(g(&env, e2)) {y in (.FSUB(x, y), .FLOAT)}})
        case .FMUL(let e1, let e2): return insert_let(g(&env, e1), {x in insert_let(g(&env, e2)) {y in (.FMUL(x, y), .FLOAT)}})
        case .FDIV(let e1, let e2): return insert_let(g(&env, e1), {x in insert_let(g(&env, e2)) {y in (.FDIV(x, y), .FLOAT)}})
        case .EQ, .LE: return g(&env, .IF(s, .BOOL(true), .BOOL(false)))
        case .IF(let cmp, let e3, let e4):
            switch cmp {
            case .NOT(let e1): return g(&env, .IF(e1, e4, e3))
            case .EQ(let e1, let e2): return insert_let(g(&env, e1)) { x in insert_let(g(&env, e2)) { y in let (e3a, t3) = g(&env, e3); let (e4a, _) = g(&env, e4); return (.IFEQ(x, y, e3a, e4a), t3) } }
            case .LE(let e1, let e2): return insert_let(g(&env, e1)) { x in insert_let(g(&env, e2)) { y in let (e3a, t3) = g(&env, e3); let (e4a, _) = g(&env, e4); return (.IFLE(x, y, e3a, e4a), t3) } }
            default: return g(&env, .IF(.EQ(cmp, .BOOL(false)), e4, e3))
            }
        case .LET(let xt, let e1, let e2): let (e1a, _) = g(&env, e1); env[xt.name] = xt.typ; let (e2a, t2) = g(&env, e2); return (.LET(xt, e1a, e2a), t2)
        case .VAR(let x): if env[x] != nil { return (.VAR(x), env[x]!) } else {fatalError()} //else { if case .ARRAY = extenv[x] { return .EXTARRAY(x, extenc[x]!) } }
        case .LETREC(let fd, let e2):
            env[fd.name.name] = fd.name.typ.t; var enva = env;
            let (e2a, t2) = g(&enva, e2);
            fd.args.forEach { enva[$0.name] = $0.typ }
            let (e1a, _) = g(&enva, fd.body);
            return (.LETREC(Ident(fd.name), fd.args, e1a, e2a), t2)
        case .APP(let e1, let e2s):
            if case .VAR(let f) = e1, env[f] == nil {
                if let fn = Typing.extenv[f], case .FUN(_, _, let t, _) = fn {
                    func bind(_ xs: [Id.T], _ y: [Syntax]) -> (KNormal, Typ) {
                        if y.isEmpty { return (.EXTFUNAPP(f, xs), t) }
                        else { return insert_let(g(&env, y.first!), {x in bind(xs + [x], Array(y.dropFirst()))})}
                    }
                    return bind([], e2s)
                } else { fatalError() }
            } else {
                let g_e1 = g(&env, e1)
                if case .FUN(_, _, let t, _) = g_e1.1 {
                    return insert_let(g_e1) { f in
                        func bind(_ xs: [Id.T], _ y: [Syntax]) -> (KNormal, Typ) {
                            if y.isEmpty { return (.APP(f, xs), t) }
                            else { return insert_let(g(&env, y.first!)) { x in bind(xs + [x], Array(y.dropFirst(1)))} }
                        }
                        return bind([], e2s)
                    }
                } else {
                    fatalError()
                }
            }
        case .TUPLE(let es):
            func bind(_ xs: [Id.T], _ ts: [Typ], _ es: [Syntax]) -> (KNormal, Typ) {
                if es.isEmpty { return (.TUPLE(xs), .TUPLE(ts)) }
                else { let g_e = g(&env, es.first!);
                    return insert_let(g_e) { x in bind(xs + [x], ts + [g_e.1], Array(es.dropLast())) }
                }
            }
            return bind([], [], es)
        case .LETTUPLE(let xts, let e1, let e2):
            return insert_let(g(&env, e1)) { y in
                xts.forEach { env[$0.name] = $0.typ };
                let (e2a, t2) = g(&env, e2);
                return (.LETTUPLE(xts, y, e2a), t2)
            }
        case .ARRAY(let e1, let e2):
            return insert_let(g(&env, e1)) { x in
                let g_e2 = g(&env, e2);
                return insert_let(g_e2) { y in
                    let l = g_e2.1 == .FLOAT ? "create_float_array" : "create_array"
                    return (.EXTFUNAPP(l, [x, y]), .ARRAY(g_e2.1))
                }
            }
        case .GET(let e1, let e2):
            let g_e1 = g(&env, e1);
            if case .ARRAY(let t) = g_e1.1 {
                return insert_let(g_e1) { x in
                    insert_let(g(&env, e2)) { y in return (.GET(x, y), t)}
                }
            } else { fatalError() }
        case .PUT(let e1, let e2, let e3):
            return insert_let(g(&env, e1)) { x in
                return insert_let(g(&env, e2)) { y in
                    return insert_let(g(&env, e3)) { z in
                        return (.PUT(x, y, y), .UNIT) }}}
        case .EMPTY: return (.INT(0), .INT)
        case .CONS(let e1, let e2):
            let g_e1 = g(&env, e1);
            let g_e2 = g(&env, e2);
            return insert_let(g_e1) { x in
                return insert_let(g_e2) { y in
                    return (.TUPLE([x, y]), .TUPLE([g_e1.1, g_e2.1])) }}
        case .MATCH(let lst, let e1, let pat):
            return insert_let(g(&env, lst)) { x in
                let (e1a, _) = g(&env, e1);
                env[pat.yt.name] = pat.yt.typ
                env[pat.xt.name] = pat.xt.typ
                let (e2a, t2) = g(&env, pat.e3);
                return insert_let((.INT(0), .INT)) { y in
                    return (.IFEQ(y, x, e1a, .LETTUPLE([pat.xt, pat.yt], x, e2a)), t2)
                }
            }
        case .SHIFT(let xt, let e, let ta):
            env[xt.name] = xt.typ.t;
            let (e, tb) = g(&env, e);
            let v = Id.genid("s")
            let t = xt.typ.t
            guard case .FUN(let t1s, _, _, _) = t else {fatalError()}
            let tv = Typ.FUN([t], tb, tb, ta)
            return (.LETREC(Ident(v, tv), [Ident(xt.name, xt.typ.t)], e, .EXTFUNAPP("shift", [v])), t1s.first!)
        case .RESET(let e, let t):
            let (e, ta) = g(&env, e);
            let v = Id.genid("r"), x = Id.genid("x")
            return (.LETREC(Ident(v, .FUN([.UNIT], t, ta, t)), [Ident(x, .UNIT)], e, .EXTFUNAPP("reset", [v])), t)

        case .fundef, .pattern, .punct, .composite: fatalError()
        }
    }
    func f(_ e: Syntax) -> KNormal {
        var env: [String: Typ] = [:]
        return g(&env, e).0
    }
    init() {}
}

struct AlphaConv {
    func find(_ x: Id.T, _ env: Env<Id.T>) -> Id.T { return env[x] ?? x }

    func g(_ env: inout Env<Id.T>, _ k: KNormal) -> KNormal {
        switch k {
        case .UNIT: return .UNIT
        case .INT(let i): return .INT(i)
        case .FLOAT(let d): return .FLOAT(d)
        case .NEG(let x): return .NEG(find(x, env))
        case .ADD(let x, let y): return .ADD(find(x, env), find(y, env))
        case .SUB(let x, let y): return .SUB(find(x, env), find(y, env))
        case .FNEG(let x): return .FNEG(find(x, env))
        case .FADD(let x, let y): return .FADD(find(x, env), find(y, env))
        case .FSUB(let x, let y): return .FSUB(find(x, env), find(y, env))
        case .FMUL(let x, let y): return .FMUL(find(x, env), find(y, env))
        case .FDIV(let x, let y): return .FDIV(find(x, env), find(y, env))
        case .IFEQ(let x, let y, let e1, let e2): return .IFEQ(find(x, env), find(y, env), g(&env, e1), g(&env, e2))
        case .IFLE(let x, let y, let e1, let e2): return .IFLE(find(x, env), find(y, env), g(&env, e1), g(&env, e2))
        case .LET(let xt, let e1, let e2): let xa = Id.genid(xt.name); env[xt.name] = xa; return .LET(Ident(xa, xt.typ), g(&env, e1), g(&env, e2))
        case .VAR(let x): return .VAR(find(x, env))
        case .LETREC(let xt, let yts, let e1, let e2):
            env[xt.name] = Id.genid(xt.name)
            let ys = yts.map(\.name)
            ys.forEach { env[$0] = Id.genid($0) }
            var enva = env
            return .LETREC(Ident(find(xt.name, env), xt.typ), yts.map { Ident(find($0.name, enva), $0.typ) }, g(&enva, e1), g(&env, e2))
        case .APP(let x, let ys): return .APP(find(x, env), ys.map { find($0, env) })
        case .TUPLE(let xs): return .TUPLE(xs.map { find($0, env) })
        case .LETTUPLE(let xts, let y, let e):
            let xs = xts.map(\.name)
            xs.forEach { env[$0] = Id.genid($0) }
            var enva = env
            return .LETTUPLE(xts.map { Ident(find($0.name, enva), $0.typ) }, find(y, env), g(&enva, e))
        case .GET(let x, let y): return .GET(find(x, env), find(y, env))
        case .PUT(let x, let y, let z): return .PUT(find(x, env), find(y, env), find(z, env))
        case .EXTARRAY(let x): return .EXTARRAY(x)
        case .EXTFUNAPP(let x, let ys): return .EXTFUNAPP(x, ys.map { find($0, env) })
        }
    }
    func f(_ e: KNormal) -> KNormal {
        var env = Env<Id.T>()
        return g(&env, e)
    }
    init() {}
}

struct BetaReduct {
    func find(_ x: Id.T, _ env: Env<Id.T>) -> Id.T { return env[x] ?? x }

    func g(_ env: inout Env<Id.T>, _ k: KNormal) -> KNormal {
        switch k {
        case .UNIT: return .UNIT
        case .INT(let i): return .INT(i)
        case .FLOAT(let d): return .FLOAT(d)
        case .NEG(let x): return .NEG(find(x, env))
        case .ADD(let x, let y): return .ADD(find(x, env), find(y, env))
        case .SUB(let x, let y): return .SUB(find(x, env), find(y, env))
        case .FNEG(let x): return .FNEG(find(x, env))
        case .FADD(let x, let y): return .FADD(find(x, env), find(y, env))
        case .FSUB(let x, let y): return .FSUB(find(x, env), find(y, env))
        case .FMUL(let x, let y): return .FMUL(find(x, env), find(y, env))
        case .FDIV(let x, let y): return .FDIV(find(x, env), find(y, env))
        case .IFEQ(let x, let y, let e1, let e2): return .IFEQ(find(x, env), find(y, env), g(&env, e1), g(&env, e2))
        case .IFLE(let x, let y, let e1, let e2): return .IFLE(find(x, env), find(y, env), g(&env, e1), g(&env, e2))
        case .LET(let xt, let e1, let e2): let e1a = g(&env, e1);
            if case .VAR(let y) = e1a { env[xt.name] = y; return g(&env, e2) }
            else { let e2a = g(&env, e2); return .LET(xt, e1a, e2a) }
        case .LETREC(let xt, let yts, let e1, let e2): return .LETREC(xt, yts, g(&env, e1), g(&env, e2))
        case .VAR(let x): return .VAR(find(x, env))
        case .TUPLE(let xs): return .TUPLE(xs.map { find($0, env) })
        case .LETTUPLE(let xts, let y, let e): return .LETTUPLE(xts, find(y, env), g(&env, e))
        case .GET(let x, let y): return .GET(find(x, env), find(y, env))
        case .PUT(let x, let y, let z): return .PUT(find(x, env), find(y, env), find(z, env))
        case .APP(let x, let ys): return .APP(find(x, env), ys.map { find($0, env) })
        case .EXTARRAY(let x): return .EXTARRAY(x)
        case .EXTFUNAPP(let x, let ys): return .EXTFUNAPP(x, ys.map { find($0, env) })
        }
    }
    func f(_ e: KNormal) -> KNormal {
        var env = Env<Id.T>()
        return g(&env, e)
    }
    init() {}
}

struct Reassoc {
    func f(_ k: KNormal) -> KNormal {
        switch k {
        case .IFEQ(let x, let y, let e1, let e2): return .IFEQ(x, y, f(e1), f(e2))
        case .IFLE(let x, let y, let e1, let e2): return .IFLE(x, y, f(e1), f(e2))
        case .LET(let xt, let e1, let e2):
            func insert(_ e: KNormal) -> KNormal {
                switch e {
                case .LET(let yt, let e3, let e4): return .LET(yt, e3, insert(e4))
                case .LETREC(let yt, let ys, let e3, let e4): return .LETREC(yt, ys, e3, insert(e4))
                case .LETTUPLE(let yts, let z, let e3): return .LETTUPLE(yts, z, insert(e3))
                default: return .LET(xt, e, f(e2))
                }
            }
            return insert(f(e1))
        case .LETREC(let xt, let xs, let e1, let e2): return .LETREC(xt, xs, f(e1), f(e2))
        case .LETTUPLE(let xts, let y, let e): return .LETTUPLE(xts, y, f(e))
        default: return k
        }
    }
    init() {}
}

struct Inlining {
    func size(of e: KNormal) -> Int {
        switch e {
        case .IFEQ(_, _, let e1, let e2), .IFLE(_, _, let e1, let e2), .LET(_, let e1, let e2), .LETREC(_, _, let e1, let e2):
            return 1 + size(of: e1) + size(of: e2)
        case .LETTUPLE(_, _, let e): return 1 + size(of: e)
        default : return 1
        }
    }
    func g(_ env: inout Env<([Ident], KNormal)>, _ k: KNormal) -> KNormal {
        switch k {
        case .IFEQ(let x, let y, let e1, let e2): return .IFEQ(x, y, g(&env, e1), g(&env, e2))
        case .IFLE(let x, let y, let e1, let e2): return .IFLE(x, y, g(&env, e1), g(&env, e2))
        case .LET(let xt, let e1, let e2): return .LET(xt, g(&env, e1), g(&env, e2))
        case .LETREC(let xt, let yts, let e1, let e2):
            if size(of: e1) <= threshold { env[xt.name] = (yts, e1) }
            return .LETREC(xt, yts, g(&env, e1), g(&env, e2))
        case .APP(let x, let ys):
            if let (zs, e) = env[x] {
                var enva = Env<Id.T>(uniqueKeysWithValues: zip(zs, ys).map {zt, y in (zt.name, y) })
                return AlphaConv().g(&enva, e)
            } else { return k }
        case .LETTUPLE(let xts, let y, let e): return .LETTUPLE(xts, y, g(&env, e))
        default: return k
        }
    }
    func f(_ e: KNormal) -> KNormal {
        var env = Env<([Ident], KNormal)>()
        return g(&env, e)
    }
    var threshold: Int
    init(_ threshold: Int = 20) { self.threshold = threshold }
}

struct ConstFold {
    func memi(_ x: Id.T, _ env: Env<KNormal>) -> Bool {
        if let t = env[x], case .INT(_) = t { return true } else { return false }
    }
    func memf(_ x: Id.T, _ env: Env<KNormal>) -> Bool {
        if let t = env[x], case .FLOAT(_) = t { return true } else { return false }
    }
    func memt(_ x: Id.T, _ env: Env<KNormal>) -> Bool {
        if let t = env[x], case .TUPLE(_) = t { return true } else { return false }
    }
    func findi(_ x: Id.T, _ env: Env<KNormal>) -> Int {
        if let t = env[x], case .INT(let i) = t { return i } else { fatalError() }
    }
    func findf(_ x: Id.T, _ env: Env<KNormal>) -> Double {
        if let t = env[x], case .FLOAT(let d) = t { return d } else { fatalError() }
    }
    func findt(_ x: Id.T, _ env: Env<KNormal>) -> [Id.T] {
        if let t = env[x], case .TUPLE(let ys) = t { return ys } else { fatalError() }
    }

    func g(_ env: inout Env<KNormal>, _ k: KNormal) -> KNormal {
        switch k {
        case .VAR(let x): return memi(x, env) ? .INT(findi(x, env)) : k
        case .NEG(let x): return memi(x, env) ? .INT(-findi(x, env)) : k
        case .ADD(let x, let y): return memi(x, env) && memi(y, env) ? .INT(findi(x, env) + findi(y, env)) : k
        case .SUB(let x, let y): return memi(x, env) && memi(y, env) ? .INT(findi(x, env) - findi(y, env)) : k
        case .FNEG(let x): return memi(x, env) ? .FLOAT(-findf(x, env)) : k
        case .FADD(let x, let y): return memf(x, env) && memf(y, env) ? .FLOAT(findf(x, env) + findf(y, env)) : k
        case .FSUB(let x, let y): return memf(x, env) && memf(y, env) ? .FLOAT(findf(x, env) - findf(y, env)) : k
        case .FMUL(let x, let y): return memf(x, env) && memf(y, env) ? .FLOAT(findf(x, env) * findf(y, env)) : k
        case .FDIV(let x, let y): return memf(x, env) && memf(y, env) ? .FLOAT(findf(x, env) / findf(y, env)) : k
        case .IFEQ(let x, let y, let e1, let e2): return memi(x, env) && memi(y, env) ? (findi(x, env) == findi(y, env) ? g(&env, e1) : g(&env, e1)) :
                                                        (memf(x, env) && memf(y, env) ? (findf(x, env) == findf(y, env) ? g(&env, e1) : g(&env, e2)) :
                                                        .IFEQ(x, y, g(&env, e1), g(&env, e2)))
        case .IFLE(let x, let y, let e1, let e2): return memi(x, env) && memi(y, env) ? (findi(x, env) <= findi(y, env) ? g(&env, e1) : g(&env, e1)) :
                                                        (memf(x, env) && memf(y, env) ? (findf(x, env) <= findf(y, env) ? g(&env, e1) : g(&env, e2)) :
                                                        .IFEQ(x, y, g(&env, e1), g(&env, e2)))

        case .LET(let xt, let e1, let e2): let e1a = g(&env, e1); env[xt.name] = e1a; let e2a = g(&env, e2); return .LET(xt, e1a, e2a)
        case .LETREC(let xt, let xs, let e1, let e2): return .LETREC(xt, xs, g(&env, e1), g(&env, e2))
        case .LETTUPLE(let xts, let y, let e): return memt(y, env) ? zip(xts, findt(y, env)).reduce(g(&env, e)) { acc, e in .LET(e.0, .VAR(e.1), acc)} : k
        default: return k
        }
    }
    func f(_ e: KNormal) -> KNormal {
        var env = Env<KNormal>()
        return g(&env, e)
    }
    init() {}
}

struct ElimDeadCodes {
    func hasSideEffect(_ k: KNormal) -> Bool {
        switch k {
        case .LET(_, let e1, let e2), .IFEQ(_, _, let e1, let e2), .IFLE(_, _, let e1, let e2): return hasSideEffect(e1) || hasSideEffect(e2)
        case .LETREC(_, _, _, let e), .LETTUPLE(_, _, let e): return hasSideEffect(e)
        case .APP, .PUT, .EXTFUNAPP: return true
        default: return false
        }
    }
    func f(_ k: KNormal) -> KNormal {
        switch k {
        case .IFEQ(let x, let y, let e1, let e2): return .IFEQ(x, y, f(e1), f(e2))
        case .IFLE(let x, let y, let e1, let e2): return .IFLE(x, y, f(e1), f(e2))
        case .LET(let xt, let e1, let e2):
            let e1a = f(e1), e2a = f(e2)
            if hasSideEffect(e1a) || e2a.fv.contains(xt.name) { return .LET(xt, e1a, e2a) } else { return e2a }
        case .LETREC(let xt, let xs, let e1, let e2):
            let e2a = f(e2);
            if e2a.fv.contains(xt.name) { return .LETREC(xt, xs, e1, e2a) } else { return e2a }
        case .LETTUPLE(let xts, let y, let e):
            let xs = xts.map(\.name)
            let ea = f(e)
            let live = ea.fv
            if xs.contains(where: live.contains) { return .LETTUPLE(xts, y, ea) } else { return ea }
        default : return k
        }
    }
    init() {}
}

final class ClosureConv {
    var toplevel: [Prog.Fundef] = []
    func g(_ env: inout Env<Typ>, _ known: inout Set<Id.T>, _ k: KNormal) -> Prog.Closure {
        switch k {
        case .UNIT: return .UNIT
        case .INT(let i): return .INT(i)
        case .FLOAT(let d): return .FLOAT(d)
        case .NEG(let x): return .NEG(x)
        case .ADD(let x, let y): return .ADD(x, y)
        case .SUB(let x, let y): return .SUB(x, y)
        case .FNEG(let x): return .FNEG(x)
        case .FADD(let x, let y): return .FADD(x, y)
        case .FSUB(let x, let y): return .FSUB(x, y)
        case .FMUL(let x, let y): return .FMUL(x, y)
        case .FDIV(let x, let y): return .FDIV(x, y)
        case .IFEQ(let x, let y, let e1, let e2): return .IFEQ(x, y, g(&env, &known, e1), g(&env, &known, e2))
        case .IFLE(let x, let y, let e1, let e2): return .IFLE(x, y, g(&env, &known, e1), g(&env, &known, e2))
        case .LET(let xt, let e1, let e2): env[xt.name] = xt.typ; return .LET(xt, g(&env, &known, e1), g(&env, &known, e2))
        case .VAR(let x): return .VAR(x)
        case .LETREC(let xt, let yts, let e1, let e2):
            let toplevel_backup = toplevel
            env[xt.name] = xt.typ
            var enva = env
            known.insert(xt.name)
            var knowna = known
            yts.forEach { enva[$0.name] = $0.typ }
            var e1a = g(&enva, &knowna, e1)
            var zs = e1a.fv.subtracting(yts.map(\.name))
            if !zs.isEmpty {
                toplevel = toplevel_backup
                yts.forEach { enva[$0.name] = $0.typ }
                e1a = g(&enva, &known, e1)
                knowna = known
            }
            zs = e1a.fv.subtracting(Set([xt.name]).union(yts.map(\.name)))
            let zts = zs.map { z in Ident(z, enva[z]!) }
            toplevel.append(.init(name: xt, args: yts, formal_fv: zts, body: e1a))
            let e2a = g(&enva, &knowna, e2)
            if e2a.fv.contains(xt.name) { return .MAKECLS(xt, xt.name, Array(zs), e2a) } else { return e2a }
        case .APP(let x, let ys): if known.contains(x) { return .APPDIR(x, ys) } else { return .APPCLS(x, ys) }
        case .TUPLE(let xs): return .TUPLE(xs)
        case .LETTUPLE(let xts, let y, let e): xts.forEach { env[$0.name] = $0.typ }; return .LETTUPLE(xts, y, g(&env, &known, e))
        case .GET(let x, let y): return .GET(x, y)
        case .PUT(let x, let y, let z): return .PUT(x, y, z)
        case .EXTARRAY(let x): return .EXTARRAY(x)
        case .EXTFUNAPP(let x, let ys): return .APPDIR("min_caml_\(x)", ys)
        }
    }
    func f(_ e: KNormal) -> Prog {
        toplevel = []
        var env = Env<Typ>()
        var known: Set<Id.T> = []
        let ea = g(&env, &known, e)
        return Prog(defs: toplevel, main: ea)
    }
    init() {}
}

struct Virtual {
    func concat(_ e1: Asm.T, _ xt: Ident, _ e2: Asm.T) -> Asm.T {
        switch e1 {
        case .ANS(let exp): return .LET(xt, exp, e2)
        case .LET(let yt, let exp, let e1a): return .LET(yt, exp, concat(e1a, xt, e2))
        }
    }
    func g(_ env: inout Env<Typ>, _ c: Prog.Closure) -> Asm.T {
        switch c {
        case .UNIT: return .ANS(.NOP)
        case .INT(let i): return .ANS(.LOADINT(i))
        case .FLOAT(let d): return .ANS(.LOADFLOAT(d))
        case .NEG(let x): return .ANS(.NEG(x))
        case .ADD(let x, let y): return .ANS(.ADD(x, y))
        case .SUB(let x, let y): return .ANS(.SUB(x, y))
        case .FNEG(let x): return .ANS(.FNEG(x))
        case .FADD(let x, let y): return .ANS(.FADD(x, y))
        case .FSUB(let x, let y): return .ANS(.FSUB(x, y))
        case .FMUL(let x, let y): return .ANS(.FMUL(x, y))
        case .FDIV(let x, let y): return .ANS(.FDIV(x, y))
        case .IFEQ(let x, let y, let e1, let e2): return .ANS(.IFEQ(x, y, g(&env, e1), g(&env, e2)))
        case .IFLE(let x, let y, let e1, let e2): return .ANS(.IFLE(x, y, g(&env, e1), g(&env, e2)))
        case .LET(let xt, let e1, let e2): let e1a = g(&env, e1); env[xt.name] = xt.typ; let e2a = g(&env, e2); return concat(e1a, xt, e2a)
        case .VAR(let x): fatalError()
        case .MAKECLS(let xt, let l, let ys, let e2): fatalError()
        case .APPCLS(let x, let ys): fatalError()
        case .APPDIR(let x, let ys): fatalError()
        case .TUPLE(let xs): fatalError()
        case .LETTUPLE(let xts, let y, let e2): fatalError()
        case .GET(let x, let y): fatalError()
        case .PUT(let x, let y, let z): let offset = Id.genid("o")
            switch env[x]! {
                default: fatalError()
            }
        case .EXTARRAY(let x): return .ANS(.SETL("min_caml_\(x)"))
        }
    }
    let reg_cl = Id.genid("reg_cl")
    func h(_ f: Prog.Fundef) -> Asm.Fundef {
        return .init(name: "todo", args: [], body: .NOP, rett: .UNIT)
//        var env: Env<Typ> = [:]
//        f.formal_fv.forEach { env[$0.name] = $0.typ }
//        f.args.forEach { env[$0.name] = $0.typ }
//        env[f.name.name] = f.name.typ
//        let (offset, load) = f.formal_fv.reduce((4, g(&env, f.body))) { off, load in
//            return (off.0 + 4, .LET(load, .LOAD(reg_cl, "\(off.0)"), off.1))
//        }
//        if case .FUN(_, _, let t2, _) = f.name.typ {
//            return .init(name: f.name.name, args: f.args.map(\.name), body: load, rett: t2)
//        }

        //{ Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e}
//        let (offset, load) =
//        expand
//        zts
//        (4, g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
//        (fun z offset load -> fletd (z, Lfd (reg_cl, C (offset)), load))
//        (fun z t offset load -> Let ((z, t), Lwz (reg_cl, C (offset)), load)) in

//        match t with
//        | Type.Fun (_, _, t2, _) ->
//        { name = Id.L(x); args = int; fargs = float; body = load; ret = t2 }
//        | _ -> assert false

    }
    func f(_ p: Prog) -> Asm {
        let fundefs = p.defs.map(h)
        var env: Env<Typ> = [:]
        let e = g(&env, p.main)
        return Asm(defs: fundefs, main: e)
    }
    init() {}
}

struct Pass {
    init() {}

    func g0(_ env: inout Env<Id.T>, _ k: KNormal) -> KNormal {
        switch k {
        case .UNIT: return .UNIT
        case .INT(let i): return .INT(i)
        case .FLOAT(let d): return .FLOAT(d)
        case .NEG(let x): return .NEG(x)
        case .ADD(let x, let y): return .ADD(x, y)
        case .SUB(let x, let y): return .SUB(x, y)
        case .FNEG(let x): return .FNEG(x)
        case .FADD(let x, let y): return .FADD(x, y)
        case .FSUB(let x, let y): return .FSUB(x, y)
        case .FMUL(let x, let y): return .FMUL(x, y)
        case .FDIV(let x, let y): return .FDIV(x, y)
        case .IFEQ(let x, let y, let e1, let e2): return .IFEQ(x, y, e1, e2)
        case .IFLE(let x, let y, let e1, let e2): return .IFLE(x, y, e1, e2)
        case .LET(let xt, let e1, let e2): return .LET(xt, e1, e2)
        case .VAR(let x): return .VAR(x)
        case .LETREC(let xt, let xs, let e1, let e2): return .LETREC(xt, xs, e1, e2)
        case .APP(let x, let ys): return .APP(x, ys)
        case .TUPLE(let xs): return .TUPLE(xs)
        case .LETTUPLE(let xts, let y, let e): return .LETTUPLE(xts, y, e)
        case .GET(let x, let y): return .GET(x, y)
        case .PUT(let x, let y, let z): return .PUT(x, y, z)
        case .EXTARRAY(let x): return .EXTARRAY(x)
        case .EXTFUNAPP(let x, let ys): return .EXTFUNAPP(x, ys)
        }
    }
}

@MainActor
final class Compiler: ObservableObject {
    @Published private(set) var out: [ChatMsg] = []
    @Published var isProcessing: Bool = false
    private var pendingChats = CurrentValueSubject<[Chat], Never>([])
    private var cancellables = Set<AnyCancellable>()
    private var optIter = 1

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
        guard pendingChats.value.last != chat else { return }
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
            guard r.rest.isEmpty else { let taken = code[..<r.rest.startIndex]; append(.error(tag: "Syntax error", text: "\(taken)\n\(spacer(taken.count))\(r.rest)")); return }
            let a = Typing().f(ast); append(.response(tag: "Typing", text: a.description))
            var k = KNormalize().f(a); append(.response(tag: "K Normalizaton", text: k.description))
            k = AlphaConv().f(k); append(.response(tag: "α conversion - renaming", text: k.description))

            for n in 1..<optIter {
                let k0 = k
                append(.info(tag: "Optimization", text: "iteration \(n)"))
                k = BetaReduct().f(k); append(.response(tag: "β reduction - variable", text: k.description))
                k = Reassoc().f(k); append(.response(tag: "Reassociation", text: k.description))
                k = Inlining().f(k); append(.response(tag: "β reduction - function", text: k.description))
                k = ConstFold().f(k); append(.response(tag: "δ rule - evaluate builtins", text: k.description))
                k = ElimDeadCodes().f(k); append(.response(tag: "Eliminalte Dead Codes", text: k.description))
                if k0 == k { break }
            }
            let prog = ClosureConv().f(k);
            for fd in prog.defs { append(.response(tag: "-", text: fd.description)) }
            append(.response(tag: "Closure Conversion", text: prog.main.description))

//            let (fda, asm) = Virtual.f(fdc, c); for fd in fda { await append("-", fd.description) }
//            await append("Virtual", asm.description)
//            let x = Emit.f(fda, asm); await append("Emit", x)
            Typing.extenv.forEach { k, v in
                append(.info(tag: "extenv", text: "\(k) : \(v)"))
            }
        }
    }
}
