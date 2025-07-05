import Foundation

var llvm = LLVMKit("test")

//llvm.emitptrinc()

let adder = llvm.makecls("adder", "y")
let y = llvm.arg(adder, 0)
let x = llvm.closure_arg(1, "x")
_ = llvm.ansadd(x, y)

let make_adder = llvm.emitfunc("make_adder")
llvm.emitret(0)

let pg = llvm.emitfunc("min_caml_start")
let i = llvm.emitletset(".i1.9", 3)
let f = llvm.emitletcalldir(".f2.8", make_adder, i)
llvm.emitret(0)

let fn = llvm.emitfunc("main")
let run = llvm.emitcall(pg, "calltmp")
//let c = llvm.emitload(add, "c")
//let sin = llvm.extfn("sin")
//let zero = llvm.emitcall(sin, .pi)
llvm.emitret(0)

llvm.dump()
