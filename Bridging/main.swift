import Foundation

var llvm = LLVMKit("test")

let pg = llvm.emitfunc("mincaml")
let a = llvm.emitlet("a", 100)
let b = llvm.emitlet("b", 19)
let add = llvm.emitadd(a, b, "addtmp")

llvm.emitptrinc()

llvm.emitret(add)

let fn = llvm.emitfunc("main")
let run = llvm.emitcall(pg, "calltmp")
//let c = llvm.emitload(add, "c")
//let sin = llvm.extfn("sin")
//let zero = llvm.emitcall(sin, .pi)
llvm.emitret(0)

llvm.dump()
