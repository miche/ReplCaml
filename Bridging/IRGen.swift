import Foundation
import CxxStdlib

enum LLVMIR {
    indirect enum T {
        case INT1
        case INT32
        case INT64
        case FLOAT
        case DOUBLE
        case ARRAY(T, Int)      // (type, count) -> type
        case FUNCTION(T, [T])
        // VECTOR
        case PTR
        case PTRTO(T)    // (type) -> type to ptr
        case VOID
        case STRUCT(String)    // (name) -> type
        case SETBODY(T, [T])    // (type(strucy), [membertype]) -> type
    }
    indirect enum V {
        //        class Value;    // super class of Argument, BasicBlock, Constant(ConstantInt/ConstantFP/GEP), Instruction
        case CONSTINT(T, Int)
        case CONSTFLOAT(T, Double)
        case CONSTZERO(T)     // (type) -> value(storage)
        case GLOBALVAR(T, V, String) // (type, ptr, name) -> value(global storage)
        case ALIGN(V, Int)    // (value, alignment)
        case NULLP
        // instruction
        case ALLOCA(T)        // (type) -> inst_ptr
        case LOAD(T, V)       // (type, value) -> inst_ptr
        case STORE(V, V)      // (value, ptr) -> inst_ptr
        case GEP(T, V, [V])   // (type, ptr, [int]) -> ptr
        case RET(V)           // (value) -> inst_ptr
        case RETVOID          // () -> inst_ptr
        case CALL(T, V, [V])     // (type, func, [value]) -> inst_ptr
        case UNREACHABLE      // () -> inst_ptr
                              //        class Function; // subclass of Value,User,Constant,GlobalValue,GlobalObject
        case FUNC(String, T) // (name, function_type) -> value(function)
        case EXTFUNC(String, T) // (name, function_type) -> value(function)
        case GETFUNCTYPE(V)   // (function) -> type(function type)
        case ARG(V, Int)                     // (function, index) -> value(argument)
        case SETNAME(V, String)              // (argument, name) -> value(argument)
        case BASICBLOCK(V?, String)        // BasicBlock::Create(*context, name, function)
                                           // modifier
        case FASTCC(V)        // (func) -> value(inst/function)
        case TAILCC(V)        // (func) -> value(inst/function)
        case INSERTPOINT(V)   // Builder->SetInsertPoint(V bb);
        case CURRBB           // Builder->GetInsertBlock()


        // CreateUnOp, CreateBinOp, CreateNAryOp
        case NEG(V)     // (value) -> value CreateNeg
        case ADD(V, V)  // (value, value) -> value CreateAdd
        case SUB(V, V)  // (value, value) -> value CreateSub
        case FNEG(V)     // (value) -> value CreateFNeg
        case FADD(V, V)  // (value, value) -> value CreateFAdd
        case FSUB(V, V)  // (value, value) -> value CreateFSub
        case FMUL(V, V)  // (value, value) -> value CreateFMul
        case FDIV(V, V)  // (value, value) -> value CreateFDiv

        case BR(V)    // (destbb) -> inst_ptr        // CreateBr
        case CONDBR(V, V, V)  // (cond, truebb, falsebb) -> inst_ptr  CreateCondBr
    }
    case DUMP
    func test() {
        let cl_t = T.STRUCT("ClosureT")
        _ = T.SETBODY(cl_t, [T.PTR, T.INT32])
        let a_t = T.ARRAY(cl_t, 10)
        let zero = V.CONSTZERO(a_t)
        let closures = V.GLOBALVAR(a_t, zero, "closures")
        _ = V.ALIGN(closures, 4)
        let stp_t = T.PTRTO(a_t)
        let closureptr = V.GLOBALVAR(stp_t, closures, "closureptr")
        _ = V.ALIGN(closureptr, 4)



        let a = V.ALLOCA(T.INT32)
        _ = V.STORE(V.CONSTINT(T.INT32, 1), a)
        let b = V.ADD(a, a)
        let c = V.LOAD(T.INT32, V.CONSTINT(T.INT32, 10))

        let ft = T.FUNCTION(T.INT32, [T.INT32, T.INT32])
        let fn = V.FUNC("fn", ft)
        _ = V.SETNAME(V.ARG(fn, 0), "arg1")
        let bb  = V.BASICBLOCK(fn, "entry")
        _ = V.INSERTPOINT(bb)
        let cond = V.CONSTINT(T.INT1, 1)
        // if cond then bb else bb endif
        var tbb = V.BASICBLOCK(fn, "true")
        let fbb = V.BASICBLOCK(nil, "false")
        let endif = V.BASICBLOCK(nil, "endif")
        _ = V.CONDBR(cond, tbb, fbb)
        _ = V.INSERTPOINT(tbb)
        // emit then block
        _ = V.BR(endif)
        tbb = V.CURRBB // Builder->GetInsertBlock(), tbb may change

        //
        _ = V.INSERTPOINT(fbb)
        _ = V.BR(endif)
        _ = V.INSERTPOINT(endif)
        _ = V.RET(V.CONSTINT(T.INT32, 0))

        let r = V.CALL(ft, fn, [a, b])
    }
}

struct Emit {
    var llvm: LLVMKit
    init() {
        llvm = LLVMKit("test")
    }
    func dump() -> String {
        return String(llvm.dump())
    }
    func f(_ ir: LLVMIR) {
        switch ir {
        case .DUMP: 
            break
        }
    }
}
