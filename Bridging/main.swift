import Foundation

var llvm = LLVMKit("test")

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

// opt -passes="mem2reg,instcombine,simplifycfg,
    //opt -passes="globalopt,globaldce,adce,dce,simplifycfg" ccc.ll -S -o test_dce_aggressive.ll


