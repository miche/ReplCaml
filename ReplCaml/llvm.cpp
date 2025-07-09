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
    inline llvm::LoadInst *load(llvm::Type *typ, llvm::Value *value, const char *name) { return bldr.CreateLoad(typ, value, name); };
    inline llvm::LoadInst *load(llvm::Type *typ, llvm::Value *value) { return bldr.CreateLoad(typ, value); };
    inline llvm::StoreInst *store(llvm::Value *value, llvm::Value *ptr, const char *name) { return bldr.CreateStore(value, ptr, name); };
    inline llvm::StoreInst *store(llvm::Value *value, llvm::Value *ptr) { return bldr.CreateStore(value, ptr); };
    inline llvm::Value *gep(llvm::Type *typ, llvm::Value *ptr, llvm::ArrayRef<llvm::Value *> index) { return bldr.CreateInBoundsGEP(typ, ptr, index); };
    inline llvm::ReturnInst *ans(llvm::Value *value) { return bldr.CreateRet(value); };
    inline llvm::ReturnInst *ret(llvm::Value *value) { return bldr.CreateRet(value); };
    inline llvm::ReturnInst *ret(const int value) { return bldr.CreateRet(i32(value)); };
    inline llvm::ReturnInst *ret(void) { return bldr.CreateRetVoid(); };
    inline llvm::CallInst *call(llvm::Function *callee, llvm::ArrayRef<llvm::Value *> args, const char *name) { return bldr.CreateCall(callee->getFunctionType(), callee, args, name); }
    inline llvm::CallInst *call(llvm::Function *callee, llvm::ArrayRef<llvm::Value *> args) { return bldr.CreateCall(callee->getFunctionType(), callee, args); }
    inline llvm::CallInst *call(llvm::FunctionType *ft, llvm::Function *callee, llvm::ArrayRef<llvm::Value *> args) { return bldr.CreateCall(ft, callee, args); }
    inline llvm::CallInst *tail(llvm::CallInst *ln) const { ln->setTailCall(); return ln; }
    inline llvm::CallInst *fastcc(llvm::CallInst *ln) const { ln->setCallingConv(llvm::CallingConv::Fast); return ln; }
    inline llvm::CallInst *tailcc(llvm::CallInst *ln) const { ln->setCallingConv(llvm::CallingConv::Tail); return ln; }
    inline llvm::UnreachableInst *unreachable() { return bldr.CreateUnreachable(); }
    inline llvm::Argument *arg(llvm::Function *fn, const int index) const { return fn->getArg(index); }
    inline llvm::LoadInst *closure_arg(llvm::Value *cl, const int index, const char *name, llvm::Type *arg_t) { return load(arg_t, gep(cl_t, arg((llvm::Function *)cl, 0), {i32(0), i32(index + 1)}), name); }
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

    inline llvm::Value *add(llvm::Value *a, llvm::Value *w) { return bldr.CreateAdd(a, w); };
    inline llvm::Value *mul(llvm::Value *a, llvm::Value *w) { return bldr.CreateMul(a, w); };

    inline llvm::LoadInst *makeclosure(llvm::Function *fn, llvm::Value *w) {
        llvm::LoadInst *ptr = load(ptr_t, closureptr);
        store(fn, gep(cl_t, ptr, {i32(0), i32(0)}));
        store(w, gep(cl_t, ptr, {i32(0), i32(1)}));
        store(gep(cl_t, ptr, {i32(1)}), closureptr);
        return ptr;
    }
    inline llvm::CallInst *callclosure(llvm::Value *cl, llvm::Value *w) {
        llvm::Value *fnp = gep(cl_t, cl, {i32(0), i32(0)});
        llvm::Function *fn = (llvm::Function *)load(ptr_t, fnp);
        llvm::FunctionType *ft = llvm::FunctionType::get(i32_t, {ptr_t, i32_t}, 0);
        return call(ft, fn, {(llvm::Value *)cl, (llvm::Value *)w});
    }
    inline std::string dump() {
        std::string buf;
        llvm::raw_string_ostream os(buf);
        mod.print(os, nullptr);
        os.flush();
        return buf;
    };
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

LLVMKit::LLVMKit(const char *name): impl(new Impl(name)) {
    unit = nullptr;
    i32 = impl->i32_t;
    dbl = impl->dbl_t;
    ptr = impl->ptr_t;
}

value_t LLVMKit::nop(void) const { return nullptr; }
value_t LLVMKit::ans(value_t value) const  { return impl->ans(value); }
value_t LLVMKit::set(str_t name, const int value) const {  return impl->i32(value); }
value_t LLVMKit::add(value_t a, value_t w) const { return impl->add(a, w); }
value_t LLVMKit::mul(value_t a, value_t w) const { return impl->mul(a, w); }
value_t LLVMKit::calldir(str_t name, func_t callee, value_t w) const { return impl->call(callee, {impl->nullp, w}, name); }
value_t LLVMKit::callcls(closure_t cl, value_t w) const { return impl->callclosure(cl, w); }
func_t LLVMKit::makecls(str_t cl, str_t w, type_t typ) const { return impl->appendbb(impl->func(cl, typ, w, impl->i32_t), "entry"); }
value_t LLVMKit::arg(func_t link, const int index) const { return impl->arg(link, index + 1); }
value_t LLVMKit::closure_arg(closure_t link, const int index, str_t name) const { return impl->closure_arg(link, index, name, impl->i32_t); }
closure_t LLVMKit::makeclosure(func_t cl, value_t w, type_t typ) const { return impl->makeclosure(cl, w); }

func_t LLVMKit::entry(str_t name) const { return impl->appendbb(impl->func(name, impl->i32_t, impl->defaultLinkage), "entry"); }

func_t LLVMKit::emitfunc(str_t name) const { return impl->appendbb(impl->func(name, impl->i32_t, llvm::Function::ExternalLinkage), name); }
value_t LLVMKit::emitcall(func_t link) const { return impl->call(link, {}); }
value_t LLVMKit::emitret(value_t value) const { return impl->ret(value); }

//value_t LLVMKit::emitload(type_t typ, value_t ptr) const { return impl->load(typ, ptr); }
//value_t LLVMKit::gep(value_t ptr, int index, int field) const { return impl->gep(impl->cl_t, ptr, {impl->i32(index), impl->i32(field)}); }
//value_t LLVMKit::emitstore(type_t typ, value_t value, value_t ptr, int offset) const { return impl->store(impl->i32(value), ptr); }

std::string LLVMKit::dump(void) const { return impl->dump(); }

/*
 void *LLVMKit::calldir(const char *name, const void *callee, const void *a, const void *w) const {
 return impl->call(callee, {impl->nullp, (llvm::Value *)a, (llvm::Value *)w}, name);
 }
 void *LLVMKit::emitret(const int value) const {
 return impl->ret(value);
 }
 void *LLVMKit::emitret(void) const {
 return impl->ret();
 }
 void *LLVMKit::emitalloca(const char *name) const {
 return impl->alloc(impl->i32_t, name);
 }

 void *LLVMKit::emitlet(const char *name, const int value) const {
 return impl->store(impl->i32(value), impl->alloc(impl->i32_t, name));
 }
 */
