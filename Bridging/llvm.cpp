#include "llvm.hpp"

//#include <iostream>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

class LLVMKit::Impl {
public:
    Impl(const char *name): mod(name, ctx), bldr(llvm::IRBuilder<>(ctx)) {};
    llvm::LLVMContext ctx;
    llvm::Module mod;
    llvm::IRBuilder<> bldr;

    llvm::GlobalVariable *closures;
    llvm::GlobalVariable *closureptr;
    const int num_of_closures = 32;

    llvm::Type *ptr_t;
    llvm::Type *i1_t;
    llvm::Type *i32_t;
    llvm::Type *i64_t;
    llvm::Type *dbl_t;
    llvm::StructType *cl_t;
    llvm::ConstantPointerNull *nullp;

    inline llvm::Constant *i1(const bool value) { return llvm::ConstantInt::get(i1_t, value); };
    inline llvm::Constant *i32(const int value) { return llvm::ConstantInt::get(i32_t, value); };
    inline llvm::Constant *i64(const int value) { return llvm::ConstantInt::get(i64_t, value); };
    inline llvm::Constant *dbl(const double value) { return llvm::ConstantFP::get(dbl_t, value); };

    inline llvm::LoadInst *load(const void *typ, const void *value) { return bldr.CreateLoad((llvm::Type *)typ, (llvm::Value *)value); };
    inline llvm::StoreInst *store(const void *value, const void *ptr) { return bldr.CreateStore((llvm::Value *)value, (llvm::Value *)ptr); };
    inline llvm::Value *gep(const void *typ, const void *ptr, llvm::ArrayRef<llvm::Value *> index)
        { return bldr.CreateInBoundsGEP((llvm::Type *)typ, (llvm::Value *)ptr, index); };
};

LLVMKit::LLVMKit(const char *name) {
    impl = new Impl(name);

    impl->nullp = llvm::ConstantPointerNull::get(llvm::PointerType::get(impl->ctx, 0));
    impl->ptr_t = llvm::PointerType::get(impl->ctx, 0);
    impl->i1_t = llvm::Type::getInt1Ty(impl->ctx);
    impl->i32_t = llvm::Type::getInt32Ty(impl->ctx);
    impl->i64_t = llvm::Type::getInt64Ty(impl->ctx);
    impl->dbl_t = llvm::Type::getDoubleTy(impl->ctx);

    impl->cl_t = llvm::StructType::create(impl->ctx, "ClosureT");
    impl->cl_t->setBody({impl->ptr_t, impl->i32_t}); // TODO: more arguments
    llvm::Type *a_t = llvm::ArrayType::get(impl->cl_t, impl->num_of_closures); // ClosureT[num_of_closures]
    llvm::ConstantAggregateZero *zero = llvm::ConstantAggregateZero::get(a_t);
    impl->closures = new llvm::GlobalVariable(impl->mod, a_t, false, llvm::GlobalVariable::InternalLinkage, zero, "closures");
    impl->closures->setAlignment(llvm::Align(4));
    llvm::PointerType *stp_t = llvm::PointerType::get(a_t, 0); // ClosureT *
    impl->closureptr = new llvm::GlobalVariable(impl->mod, stp_t, false, llvm::GlobalVariable::InternalLinkage, impl->closures, "closureptr");
    impl->closureptr->setAlignment(llvm::Align(8));
}

void *LLVMKit::makecls(const char *name, const char *w, const bool ret_fun) const {
    llvm::FunctionType *ft = llvm::FunctionType::get(ret_fun ? impl->ptr_t : impl->i32_t, {impl->ptr_t, impl->i32_t}, 0);
    llvm::Function *fn = llvm::Function::Create(ft, llvm::Function::InternalLinkage, name, impl->mod);
    llvm::Argument *arg0 = fn->getArg(0); arg0->setName("cl_ptr");
    llvm::Argument *arg1 = fn->getArg(1); arg1->setName(w);
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(impl->ctx, "entry", fn);
    impl->bldr.SetInsertPoint(bb);
    return fn;
}

void *LLVMKit::makeclosure(const void *fn, const void *w) const {
    // llvm::LoadInst *orgp = impl->bldr.CreateLoad(impl->ptr_t, impl->closureptr);
    llvm::LoadInst *orgp = impl->load(impl->ptr_t, impl->closureptr);
    //llvm::Value *fnp = impl->bldr.CreateInBoundsGEP(impl->cl_t, orgp, {impl->i32(0), impl->i32(0)});
    llvm::Value *fnp = impl->gep(impl->cl_t, orgp, {impl->i32(0), impl->i32(0)});
    impl->store(fn, fnp);
    //llvm::Value *argp = impl->bldr.CreateInBoundsGEP(impl->cl_t, orgp, {impl->i32(0), impl->i32(1)});
    llvm::Value *argp = impl->gep(impl->cl_t, orgp, {impl->i32(0), impl->i32(1)});
    impl->store(w, argp);
    //llvm::Value *newclptr = impl->bldr.CreateInBoundsGEP(impl->cl_t, orgp, {impl->i32(1)});
    llvm::Value *newclptr = impl->gep(impl->cl_t, orgp, {impl->i32(1)});
    impl->store(newclptr, impl->closureptr);
    return orgp;
}

void *LLVMKit::entry(const char* name) const {
    llvm::FunctionType *ft = llvm::FunctionType::get(impl->i32_t, 0);
    llvm::Function *fn = llvm::Function::Create(ft, llvm::Function::InternalLinkage, name, impl->mod);
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(impl->ctx, "entry", fn);
    impl->bldr.SetInsertPoint(bb);
    return fn;
}
void *LLVMKit::arg(const void *fn, const int index) const {
    llvm::Argument *arg = ((llvm::Function *)fn)->getArg(index + 1);
    return arg;
}
void *LLVMKit::closure_arg(const void *fn, const int index, const char *name) const {
    llvm::Argument *cl_ptr = ((llvm::Function *)fn)->getArg(0);
    llvm::Value *ptr = impl->bldr.CreateInBoundsGEP(impl->cl_t, cl_ptr, {impl->i32(0), impl->i32(index + 1)});
    llvm::LoadInst *x = impl->bldr.CreateLoad(impl->i32_t, ptr, name);
    return x;
}

void *LLVMKit::letset(const char *name, const int value) const {
    llvm::Value *r = llvm::ConstantInt::get(impl->i32_t, value);
    return r;
}
void *LLVMKit::letcalldir(const char *name, const void *callee, const void *w) const {
    llvm::Function *fn = (llvm::Function *)callee;
    llvm::FunctionType *ft = fn->getFunctionType();
    llvm::CallInst *x = impl->bldr.CreateCall(ft, fn, {impl->nullp, (llvm::Value *)w}, name);
    return x;
}
void *LLVMKit::letcalldir(const char *name, const void *callee, const void *a, const void *w) const {
    llvm::Function *fn = (llvm::Function *)callee;
    llvm::FunctionType *ft = fn->getFunctionType();
    llvm::CallInst *x = impl->bldr.CreateCall(ft, fn, {impl->nullp, (llvm::Value *)a, (llvm::Value *)w}, name);
    return x;
}

void *LLVMKit::ansadd(const void *a, const void *w) const {
    llvm::Value *add = impl->bldr.CreateAdd((llvm::Value *)a, (llvm::Value *)w);
    llvm::ReturnInst *x = impl->bldr.CreateRet(add);
    return x;
}
void *LLVMKit::anscallcls(const void *cl, const void *w) const {
    llvm::Value *fnp = impl->bldr.CreateInBoundsGEP(impl->cl_t, (llvm::Value *)cl, {impl->i32(0), impl->i32(0)});
    llvm::Function *fn = (llvm::Function *)impl->bldr.CreateLoad(impl->ptr_t, fnp);
    llvm::FunctionType *ft = llvm::FunctionType::get(impl->i32_t, {impl->ptr_t, impl->i32_t}, 0);
    llvm::CallInst *r = impl->bldr.CreateCall(ft, fn, {(llvm::Value *)cl, (llvm::Value *)w});
    llvm::ReturnInst *x = impl->bldr.CreateRet(r);
    return x;
}



// -----
void LLVMKit::dump() const {
    impl->mod.print(llvm::outs(), nullptr);
}

// function/call/ret
void *LLVMKit::emitfunc(const char* name) const {
    llvm::FunctionType *ft = llvm::FunctionType::get(impl->i32_t, 0);
    llvm::Function *fn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, impl->mod);
#if 0
    fn->setCallingConv(llvm::CallingConv::Tail);
#endif
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(impl->ctx, "entry", fn);
    impl->bldr.SetInsertPoint(bb);    // move insert point to the end of bb, ready to insert, needed?
    return fn;
}

void *LLVMKit::emitret(const void *ptr) const {
    llvm::ReturnInst *x = impl->bldr.CreateRet((llvm::Value *)ptr);
    return x;
}
void *LLVMKit::emitret(const int value) const {
    llvm::ReturnInst *x = impl->bldr.CreateRet(impl->i32(value));
    return x;
}
void *LLVMKit::emitret(void) const {
    llvm::ReturnInst *x = impl->bldr.CreateRetVoid();
    return x;
}
void *LLVMKit::emitcall(const void *callee, const char *name) const {
    llvm::FunctionType *ft = llvm::FunctionType::get(impl->i32_t, {}, false);
    llvm::CallInst *x = impl->bldr.CreateCall(ft, (llvm::Function *)callee, {}, name);
#if 0
    x->setTailCall();
    x->setCallingConv(llvm::CallingConv::Tail);;
    llvm::UnreachableInst *r = bldr.CreateUnreachable();
#endif
    return x;
}

// stack
void *LLVMKit::emitalloca(const char *name) const {
    llvm::AllocaInst *v = impl->bldr.CreateAlloca(impl->i32_t, nullptr, name);
    return v;
}
void *LLVMKit::emitstore(const void *name, const int value) const {
    llvm::StoreInst *x = impl->bldr.CreateStore(impl->i32(value), (llvm::Value *)name);
    return x;
}
void *LLVMKit::emitload(const void *ptr, const char *name) const {
    llvm::LoadInst *x = impl->bldr.CreateLoad(impl->i32_t, (llvm::Value *)ptr, name);
    return x;
}
void *LLVMKit::emitlet(const char *name, const int value) const {
    llvm::AllocaInst *ptr = impl->bldr.CreateAlloca(impl->i32_t, nullptr, name);
    impl->bldr.CreateStore(impl->i32(value), ptr);
    return ptr;
}
