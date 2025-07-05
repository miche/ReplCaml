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

    inline llvm::Constant *i1(const bool value) const { return llvm::ConstantInt::get(i1_t, value); };
    inline llvm::Constant *i32(const int value) const { return llvm::ConstantInt::get(i32_t, value); };
    inline llvm::Constant *i64(const int value) const { return llvm::ConstantInt::get(i64_t, value); };
    inline llvm::Constant *dbl(const double value) const { return llvm::ConstantFP::get(dbl_t, value); };

    inline llvm::AllocaInst *alloc(llvm::Type *typ, const char *name)
    { return bldr.CreateAlloca(typ, nullptr, name); };
    inline llvm::LoadInst *load(llvm::Type *typ, const void *value, const char *name)
    { return bldr.CreateLoad(typ, (llvm::Value *)value, name); };
    inline llvm::LoadInst *load(llvm::Type *typ, const void *value)
    { return bldr.CreateLoad(typ, (llvm::Value *)value); };
    inline llvm::StoreInst *store(const void *value, const void *ptr, const char *name)
    { return bldr.CreateStore((llvm::Value *)value, (llvm::Value *)ptr, name); };
    inline llvm::StoreInst *store(const void *value, const void *ptr)
    { return bldr.CreateStore((llvm::Value *)value, (llvm::Value *)ptr); };
    inline llvm::Value *gep(const void *typ, const void *ptr, llvm::ArrayRef<llvm::Value *> index)
    { return bldr.CreateInBoundsGEP((llvm::Type *)typ, (llvm::Value *)ptr, index); };
    inline llvm::ReturnInst *ret(const void *value)
    { return bldr.CreateRet((llvm::Value *)value); };
    inline llvm::ReturnInst *ret(const int value)
    { return bldr.CreateRet(i32(value)); };
    inline llvm::ReturnInst *ret(void)
    { return bldr.CreateRetVoid(); };
    inline llvm::CallInst *call(const void *callee, llvm::ArrayRef<llvm::Value *> args, const char *name)
    { return bldr.CreateCall(((llvm::Function *)callee)->getFunctionType(), (llvm::Function *)callee, args, name); }
    inline llvm::CallInst *call(const void *callee, llvm::ArrayRef<llvm::Value *> args)
    { return bldr.CreateCall(((llvm::Function *)callee)->getFunctionType(), (llvm::Function *)callee, args); }
    inline llvm::CallInst *call(llvm::FunctionType *ft, const void *callee, llvm::ArrayRef<llvm::Value *> args)
    { return bldr.CreateCall(ft, (llvm::Function *)callee, args); }
#if 0
    x->setTailCall();
    x->setCallingConv(llvm::CallingConv::Tail);;
    llvm::UnreachableInst *r = bldr.CreateUnreachable();
#endif
    inline llvm::Argument *arg(const void *fn, const int index) const
    { return ((llvm::Function *)fn)->getArg(index); }

    inline llvm::Function *func(const char *name, llvm::Type *ret_t, llvm::Function::LinkageTypes linkage) {
        llvm::FunctionType *ft = llvm::FunctionType::get(ret_t, false);
        llvm::Function *fn = llvm::Function::Create(ft, linkage, name, mod);
        return fn;
    }
    inline llvm::Function *func(const char *name, llvm::Type *ret_t) {
        llvm::FunctionType *ft = llvm::FunctionType::get(ret_t, {ptr_t}, false);
        llvm::Function *fn = llvm::Function::Create(ft, llvm::Function::InternalLinkage, name, mod);
        arg(fn, 0)->setName("cl_ptr");
        return fn;
    }
    inline llvm::Function *func(const char *name, llvm::Type *ret_t, const char *w, llvm::Type *arg_t) {
        llvm::FunctionType *ft = llvm::FunctionType::get(ret_t, {ptr_t, arg_t}, false);
        llvm::Function *fn = llvm::Function::Create(ft, llvm::Function::InternalLinkage, name, mod);
        arg(fn, 0)->setName("cl_ptr"); arg(fn, 1)->setName(w);
        return fn;
    }
    inline llvm::BasicBlock *bb(llvm::Function *fn, const char *name)
    { llvm::BasicBlock *bb = llvm::BasicBlock::Create(ctx, name, fn); bldr.SetInsertPoint(bb); return bb; }

    inline llvm::Value *add(const void *a, const void *w)
    { return bldr.CreateAdd((llvm::Value *)a, (llvm::Value *)w); };



    inline void *closure_arg(const void *fn, const int index, const char *name)
    { return load(i32_t, gep(cl_t, arg(fn, 0), {i32(0), i32(index + 1)}), name); }
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
    llvm::Function *fn = impl->func(name, ret_fun ? impl->ptr_t : impl->i32_t, w, impl->i32_t);
    impl->bb(fn, "entry");
    return fn;
}

void *LLVMKit::makeclosure(const void *fn, const void *w) const {
    llvm::LoadInst *ptr = impl->load(impl->ptr_t, impl->closureptr);
    impl->store(fn, impl->gep(impl->cl_t, ptr, {impl->i32(0), impl->i32(0)}));
    impl->store(w, impl->gep(impl->cl_t, ptr, {impl->i32(0), impl->i32(1)}));
    impl->store(impl->gep(impl->cl_t, ptr, {impl->i32(1)}), impl->closureptr);
    return ptr;
}

void *LLVMKit::entry(const char* name) const {
    llvm::Function *fn = impl->func(name, impl->i32_t, llvm::Function::InternalLinkage);
    impl->bb(fn, "entry");
    return fn;
}
void *LLVMKit::arg(const void *fn, const int index) const {
    return impl->arg(fn, index + 1);
}
void *LLVMKit::closure_arg(const void *fn, const int index, const char *name) const {
    return impl->closure_arg(fn, index, name);
}

void *LLVMKit::letset(const char *name, const int value) const {
    return impl->i32(value);
}
void *LLVMKit::letcalldir(const char *name, const void *callee, const void *w) const {
    return impl->call(callee, {impl->nullp, (llvm::Value *)w}, name);
}
void *LLVMKit::letcalldir(const char *name, const void *callee, const void *a, const void *w) const {
    return impl->call(callee, {impl->nullp, (llvm::Value *)a, (llvm::Value *)w}, name);
}

void *LLVMKit::ansadd(const void *a, const void *w) const {
    return impl->ret(impl->add(a, w));
}
void *LLVMKit::anscallcls(const void *cl, const void *w) const {
    llvm::Value *fnp = impl->gep(impl->cl_t, (llvm::Value *)cl, {impl->i32(0), impl->i32(0)});
    llvm::Function *fn = (llvm::Function *)impl->bldr.CreateLoad(impl->ptr_t, fnp);
    llvm::FunctionType *ft = llvm::FunctionType::get(impl->i32_t, {impl->ptr_t, impl->i32_t}, 0);   // TODO: function should be installed!
    return impl->ret(impl->call(ft, fn, {(llvm::Value *)cl, (llvm::Value *)w}));
}



// -----
void LLVMKit::dump() const {
    impl->mod.print(llvm::outs(), nullptr);
}

// function/call/ret
void *LLVMKit::emitfunc(const char* name) const {
    llvm::Function *fn = impl->func(name, impl->i32_t, llvm::Function::ExternalLinkage);
//    llvm::FunctionType *ft = llvm::FunctionType::get(impl->i32_t, 0);
//    llvm::Function *fn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, impl->mod);
#if 0
    fn->setCallingConv(llvm::CallingConv::Tail);
#endif
    impl->bb(fn, "entry");
//    llvm::BasicBlock *bb = llvm::BasicBlock::Create(impl->ctx, "entry", fn);
//    impl->bldr.SetInsertPoint(bb);    // move insert point to the end of bb, ready to insert, needed?
    return fn;
}

void *LLVMKit::emitret(const void *ptr) const {
    return impl->ret(ptr);
}
void *LLVMKit::emitret(const int value) const {
    return impl->ret(value);
}
void *LLVMKit::emitret(void) const {
    return impl->ret();
}
void *LLVMKit::emitcall(const void *callee, const char *name) const {
    return impl->call(callee, {}, name);
}

// stack
void *LLVMKit::emitalloca(const char *name) const {
    return impl->alloc(impl->i32_t, name);
}
void *LLVMKit::emitstore(const int value, const void *ptr) const {
    return impl->store(impl->i32(value), ptr);
}
void *LLVMKit::emitload(const void *ptr, const char *name) const {
    return impl->load(impl->i32_t, ptr, name);
}
void *LLVMKit::emitlet(const char *name, const int value) const {
    return impl->store(impl->i32(value), impl->alloc(impl->i32_t, name));
}
