#include "llvm.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

class LLVMKit::Impl {
public:
    Impl(const char *name);
    llvm::LLVMContext ctx;
    llvm::Module mod;
    llvm::IRBuilder<> bldr;

    llvm::GlobalVariable *closures;
    llvm::GlobalVariable *closureptr;
    const int num_of_closures = 32;

    llvm::Function::LinkageTypes defaultLinkage = llvm::Function::InternalLinkage;

    llvm::PointerType *ptr_t;
    llvm::Type *i1_t;
    llvm::Type *i32_t;
    llvm::Type *i64_t;
    llvm::Type *flt_t;
    llvm::Type *dbl_t;
    llvm::StructType *cl_t;
    llvm::ConstantPointerNull *nullp;

    inline llvm::Constant *i1(const bool value) const { return llvm::ConstantInt::get(i1_t, value); };
    inline llvm::Constant *i32(const int value) const { return llvm::ConstantInt::get(i32_t, value); };
    inline llvm::Constant *i64(const int value) const { return llvm::ConstantInt::get(i64_t, value); };
    inline llvm::Constant *flt(const float value) const { return llvm::ConstantFP::get(flt_t, value); };
    inline llvm::Constant *dbl(const double value) const { return llvm::ConstantFP::get(dbl_t, value); };

    inline llvm::AllocaInst *alloc(llvm::Type *typ, const char *name) { return bldr.CreateAlloca(typ, nullptr, name); };
    inline llvm::LoadInst *load(llvm::Type *typ, const void *value, const char *name) { return bldr.CreateLoad(typ, (llvm::Value *)value, name); };
    inline llvm::LoadInst *load(llvm::Type *typ, const void *value) { return bldr.CreateLoad(typ, (llvm::Value *)value); };
    inline llvm::StoreInst *store(const void *value, const void *ptr, const char *name) { return bldr.CreateStore((llvm::Value *)value, (llvm::Value *)ptr, name); };
    inline llvm::StoreInst *store(const void *value, const void *ptr) { return bldr.CreateStore((llvm::Value *)value, (llvm::Value *)ptr); };
    inline llvm::Value *gep(const void *typ, const void *ptr, llvm::ArrayRef<llvm::Value *> index) { return bldr.CreateInBoundsGEP((llvm::Type *)typ, (llvm::Value *)ptr, index); };
    inline llvm::ReturnInst *ans(const void *value) { return bldr.CreateRet((llvm::Value *)value); };
    inline llvm::ReturnInst *ret(const void *value) { return bldr.CreateRet((llvm::Value *)value); };
    inline llvm::ReturnInst *ret(const int value) { return bldr.CreateRet(i32(value)); };
    inline llvm::ReturnInst *ret(void) { return bldr.CreateRetVoid(); };
    inline llvm::CallInst *call(const void *callee, llvm::ArrayRef<llvm::Value *> args, const char *name) { return bldr.CreateCall(((llvm::Function *)callee)->getFunctionType(), (llvm::Function *)callee, args, name); }
    inline llvm::CallInst *call(const void *callee, llvm::ArrayRef<llvm::Value *> args) { return bldr.CreateCall(((llvm::Function *)callee)->getFunctionType(), (llvm::Function *)callee, args); }
    inline llvm::CallInst *call(llvm::FunctionType *ft, const void *callee, llvm::ArrayRef<llvm::Value *> args) { return bldr.CreateCall(ft, (llvm::Function *)callee, args); }
    inline llvm::CallInst *tail(llvm::CallInst *ln) const { ln->setTailCall(); return ln; }
    inline llvm::CallInst *fastcc(llvm::CallInst *ln) const { ln->setCallingConv(llvm::CallingConv::Fast); return ln; }
    inline llvm::CallInst *tailcc(llvm::CallInst *ln) const { ln->setCallingConv(llvm::CallingConv::Tail); return ln; }
    inline llvm::UnreachableInst *unreachable() { return bldr.CreateUnreachable(); }
    inline llvm::Argument *arg(const void *fn, const int index) const { return ((llvm::Function *)fn)->getArg(index); }
    inline llvm::LoadInst *closure_arg(const void *fn, const int index, const char *name, llvm::Type *arg_t) { return load(arg_t, gep(cl_t, arg(fn, 0), {i32(0), i32(index + 1)}), name); }
    inline llvm::Function *fastcc(llvm::Function *fn) const { fn->setCallingConv(llvm::CallingConv::Fast); return fn; }
    inline llvm::Function *tailcc(llvm::Function *fn) const { fn->setCallingConv(llvm::CallingConv::Tail); return fn; }

    inline llvm::Function *func(const char *name, llvm::Type *ret_t, llvm::Function::LinkageTypes linkage) {
        llvm::FunctionType *ft = llvm::FunctionType::get(ret_t, false);
        llvm::Function *fn = llvm::Function::Create(ft, linkage, name, mod);
        return fn;
    }
    inline llvm::Function *func(const char *name, llvm::Type *ret_t) {
        llvm::FunctionType *ft = llvm::FunctionType::get(ret_t, {ptr_t}, false);
        llvm::Function *fn = llvm::Function::Create(ft, defaultLinkage, name, mod);
        arg(fn, 0)->setName("cl_ptr");
        return fn;
    }
    inline llvm::Function *func(const char *name, llvm::Type *ret_t, const char *w, llvm::Type *arg_t) {
        llvm::FunctionType *ft = llvm::FunctionType::get(ret_t, {ptr_t, arg_t}, false);
        llvm::Function *fn = llvm::Function::Create(ft, defaultLinkage, name, mod);
        arg(fn, 0)->setName("cl_ptr"); arg(fn, 1)->setName(w);
        return fn;
    }
    inline llvm::BasicBlock *bb(llvm::Function *fn, const char *name) { llvm::BasicBlock *bb = llvm::BasicBlock::Create(ctx, name, fn); bldr.SetInsertPoint(bb); return bb; }
    inline llvm::Function *appendbb(llvm::Function *fn, const char *name) { bb(fn, name); return fn; }

    inline llvm::Value *add(const void *a, const void *w) { return bldr.CreateAdd((llvm::Value *)a, (llvm::Value *)w); };

    inline void *makeclosure(const void *fn, const void *w) {
        llvm::LoadInst *ptr = load(ptr_t, closureptr);
        store(fn, gep(cl_t, ptr, {i32(0), i32(0)}));
        store(w, gep(cl_t, ptr, {i32(0), i32(1)}));
        store(gep(cl_t, ptr, {i32(1)}), closureptr);
        return ptr;
    }
    inline void *callclosure(const void *cl, const void *w) {
        llvm::Value *fnp = gep(cl_t, cl, {i32(0), i32(0)});
        llvm::Function *fn = (llvm::Function *)load(ptr_t, fnp);
        llvm::FunctionType *ft = llvm::FunctionType::get(i32_t, {ptr_t, i32_t}, 0);
        return call(ft, fn, {(llvm::Value *)cl, (llvm::Value *)w});
    }

    inline std::string dump() { std::string buf; llvm::raw_string_ostream os(buf); mod.print(os, nullptr); os.flush(); return buf; };
};

LLVMKit::Impl::Impl(const char *name): mod(name, ctx), bldr(llvm::IRBuilder<>(ctx)) {
    ptr_t = llvm::PointerType::get(ctx, 0);
    nullp = llvm::ConstantPointerNull::get(ptr_t);
    i1_t = llvm::Type::getInt1Ty(ctx);
    i32_t = llvm::Type::getInt32Ty(ctx);
    i64_t = llvm::Type::getInt64Ty(ctx);
    flt_t = llvm::Type::getFloatTy(ctx);
    dbl_t = llvm::Type::getDoubleTy(ctx);

    cl_t = llvm::StructType::create(ctx, "ClosureT");
    cl_t->setBody({ptr_t, i32_t}); // TODO: more arguments
    llvm::Type *a_t = llvm::ArrayType::get(cl_t, num_of_closures); // ClosureT[num_of_closures]
    llvm::ConstantAggregateZero *zero = llvm::ConstantAggregateZero::get(a_t);
    closures = new llvm::GlobalVariable(mod, a_t, false, defaultLinkage, zero, "closures");
    closures->setAlignment(llvm::Align(4));
    llvm::PointerType *stp_t = llvm::PointerType::get(a_t, 0); // ClosureT *
    closureptr = new llvm::GlobalVariable(mod, stp_t, false, defaultLinkage, closures, "closureptr");
    closureptr->setAlignment(llvm::Align(8));
}

// ---

LLVMKit::LLVMKit(const char *name): impl(new Impl(name)) { }

void *LLVMKit::makecls(const char *name, const char *w, const bool ret_fun) const {
    return impl->appendbb(impl->func(name, ret_fun ? impl->ptr_t : impl->i32_t, w, impl->i32_t), "entry");
}
void *LLVMKit::makeclosure(const void *fn, const void *w) const {
    return impl->makeclosure(fn, w);
}
void *LLVMKit::entry(const char* name) const {
    return impl->appendbb(impl->func(name, impl->i32_t, impl->defaultLinkage), "entry");
}
void *LLVMKit::arg(const void *fn, const int index) const {
    return impl->arg(fn, index + 1);
}
void *LLVMKit::closure_arg(const void *fn, const int index, const char *name) const {
    return impl->closure_arg(fn, index, name, impl->i32_t);
}

void *LLVMKit::set(const char *name, const int value) const {
    return impl->i32(value);
}
void *LLVMKit::calldir(const char *name, const void *callee, const void *w) const {
    return impl->call(callee, {impl->nullp, (llvm::Value *)w}, name);
}
void *LLVMKit::calldir(const char *name, const void *callee, const void *a, const void *w) const {
    return impl->call(callee, {impl->nullp, (llvm::Value *)a, (llvm::Value *)w}, name);
}

void *LLVMKit::add(const void *a, const void *w) const {
    return impl->add(a, w);
}
void *LLVMKit::callcls(const void *cl, const void *w) const {
    return impl->callclosure(cl, w);
}
void *LLVMKit::ans(const void *value) const {
    return impl->ans(value);
}
// -----
std::string LLVMKit::dump() const {
    return impl->dump();
}

void *LLVMKit::emitfunc(const char* name) const {
    return impl->appendbb(impl->func(name, impl->i32_t, llvm::Function::ExternalLinkage), name);
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
