import Foundation

var llvm = LLVMKit("test")
let fn = llvm.emitfunc("main")
let a = llvm.emitlet("a", 100)
let b = llvm.emitlet("b", 19)
let add = llvm.emitadd(a, b, "addtmp")
//let c = llvm.emitload(add, "c")
llvm.emitret(add)
//let sin = llvm.extfn("sin")
//let zero = llvm.emitcall(sin, .pi)
//llvm.emitret(0)
llvm.dump()
