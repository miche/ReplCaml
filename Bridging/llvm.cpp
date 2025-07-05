#include "llvm.hpp"

//#include <iostream>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

static llvm::LLVMContext ctx;
static llvm::IRBuilder<> bldr(ctx);
static std::unique_ptr<llvm::Module> mod;
static llvm::GlobalVariable *closures;
static llvm::GlobalVariable *closureptr;
static const int num_of_closures = 32;

LLVMKit::LLVMKit(const char *name) : modulename(name) {
    mod = std::make_unique<llvm::Module>(modulename, ctx);

    llvm::Type *i32 = llvm::Type::getInt32Ty(ctx);
    llvm::StructType *st = llvm::StructType::create(ctx, "ClosureT");
    st->setBody({i32, i32});
    llvm::Type *at = llvm::ArrayType::get(st, num_of_closures);
    llvm::ConstantAggregateZero *zero = llvm::ConstantAggregateZero::get(at);
    closures = new llvm::GlobalVariable(*mod, at, false, llvm::GlobalVariable::InternalLinkage, zero, "closures");
    closures->setAlignment(llvm::Align(4));

    llvm::PointerType *pt = llvm::PointerType::get(at, 0);
    closureptr = new llvm::GlobalVariable(*mod, pt, false, llvm::GlobalVariable::InternalLinkage, closures, "closureptr");
    closureptr->setAlignment(llvm::Align(8));
}

void *LLVMKit::emitptrinc() const {  // not needed??? CreateInBoundsGEP index is enough to manage closures.
    llvm::Type *st = llvm::StructType::getTypeByName(ctx, "ClosureT");
    llvm::LoadInst *ptr = bldr.CreateLoad(bldr.getPtrTy(), closureptr, "clptr");
    llvm::Value *pa = bldr.CreateInBoundsGEP(st, ptr, {bldr.getInt32(0), bldr.getInt32(0)});
    llvm::StoreInst *ta = bldr.CreateStore(bldr.getInt32(123), pa);
    llvm::Value *pb = bldr.CreateInBoundsGEP(st, ptr, {bldr.getInt32(0), bldr.getInt32(1)});
    llvm::StoreInst *tb = bldr.CreateStore(bldr.getInt32(456), pb);
    llvm::Value *ptrtmp = bldr.CreateInBoundsGEP(st, ptr, {bldr.getInt32(1)});
    llvm::StoreInst *x = bldr.CreateStore(ptrtmp, closureptr);
    return x;
}

void *LLVMKit::emitfunc(const char* name) const {
    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), 0);
    llvm::Function *fn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, mod.get());
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(ctx, "entry", fn);
    bldr.SetInsertPoint(bb);    // move insert point to the end of bb, ready to insert, needed?

    return fn;
}

void *LLVMKit::makecls(const char *name) const {
    llvm::Type *i32 = llvm::Type::getInt32Ty(ctx);
    llvm::FunctionType *ft = llvm::FunctionType::get(i32, {}, 0);
    llvm::Function *fn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, mod.get());
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(ctx, "entry", fn);
    bldr.SetInsertPoint(bb);    // move insert point to the end of bb, ready to insert, needed?

    return fn;
}
void *LLVMKit::makecls(const char *name, const char *arg1) const {
    llvm::Type *i32 = llvm::Type::getInt32Ty(ctx);
    llvm::FunctionType *ft = llvm::FunctionType::get(i32, {i32}, 0);
    llvm::Function *fn = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, mod.get());
    llvm::Argument *a = fn->getArg(0);
    a->setName(arg1);
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(ctx, "entry", fn);
    bldr.SetInsertPoint(bb);    // move insert point to the end of bb, ready to insert, needed?

    return fn;
}
void *LLVMKit::arg(const void *fn, const int index, const char* name) const {
    llvm::Argument *arg = ((llvm::Function *)fn)->getArg(index);
    llvm::AllocaInst *ptr = bldr.CreateAlloca(bldr.getInt32Ty(), nullptr, name);
    llvm::StoreInst *x = bldr.CreateStore(arg, ptr);
    x->setAlignment(llvm::Align(4));

    return ptr;
}
void *LLVMKit::arg(const void *fn, const int index) const {
    llvm::Argument *arg = ((llvm::Function *)fn)->getArg(index);
    return arg;
}
void *LLVMKit::closure_arg(const int index, const char* name) const {
    llvm::Type *i32 = bldr.getInt32Ty();
    llvm::Value *ptr = bldr.CreateGEP(i32, closures, {bldr.getInt32(index)}, "ptr");
    llvm::LoadInst *x = bldr.CreateLoad(bldr.getInt32Ty(), ptr, name);
    return x;
}

void *LLVMKit::emitletset(const char *name, const int value) const {
    llvm::Type *i32 = bldr.getInt32Ty();
#if 0
    // %name = alloca i32, align 4
    llvm::AllocaInst *ptr = bldr.CreateAlloca(bldr.getInt32Ty(), nullptr, name);
    ptr->setAlignment(llvm::Align(4));
    // store i32 value, ptr %name, align 4
    llvm::StoreInst *x = bldr.CreateStore(bldr.getInt32(value), ptr);
    x->setAlignment(llvm::Align(4));
    return ptr;
#endif
    llvm::Value *r = llvm::ConstantInt::get(i32, value);
    return r;
}

void *LLVMKit::emitalloca(const char *name) const {
    // %name = alloca i32, align 4
    llvm::AllocaInst *v = bldr.CreateAlloca(bldr.getInt32Ty(), /* ArraySize */ nullptr, /* Twine */name);
    return v;
}
void *LLVMKit::emitstore(const void *name, const int value) const {
    // store i32 value, ptr %name, align 4
    llvm::StoreInst *x = bldr.CreateStore(bldr.getInt32(value), (llvm::Value *)name);
    return x;
}
void *LLVMKit::emitlet(const char *name, const int value) const {
    llvm::AllocaInst *ptr = bldr.CreateAlloca(bldr.getInt32Ty(), nullptr, name);
    bldr.CreateStore(bldr.getInt32(value), ptr);
    return ptr;
}
void *LLVMKit::emitload(const void *ptr, const char *name) const {
    // %name = load i32, i32* %ptr
    llvm::LoadInst *x = bldr.CreateLoad(bldr.getInt32Ty(), (llvm::Value *)ptr, name);
    return x;
}
void *LLVMKit::emitldi(const void *ptr, const void *dest, const int offset) const {
    // %0 = getelementptr i8, ptr %ptr, i32 offset
    // store ptr %0, ptr %dest
    llvm::Value *v = bldr.CreateGEP(bldr.getInt8Ty(), (llvm::Value *)ptr, {bldr.getInt32(offset)});
    llvm::StoreInst *x = bldr.CreateStore(v, (llvm::Value *)dest);
    return x;
}
void *LLVMKit::emitret(const void *ptr) const {
    llvm::ReturnInst *x = bldr.CreateRet((llvm::Value *)ptr);
    return x;
}
void *LLVMKit::emitret(const int value) const {
    llvm::ReturnInst *x = bldr.CreateRet(bldr.getInt32(value));
    return x;
}
void *LLVMKit::emitret(void) const {
    llvm::ReturnInst *x = bldr.CreateRetVoid();
    return x;
}
void *LLVMKit::emitadd(const void *lptr, const void *rptr, const char *name) const {
    llvm::LoadInst *l = bldr.CreateLoad(bldr.getInt32Ty(), (llvm::Value *)lptr, name);
    llvm::LoadInst *r = bldr.CreateLoad(bldr.getInt32Ty(), (llvm::Value *)rptr, name);
    llvm::Value *x = bldr.CreateAdd(l, r, name);
    return x;
}
void *LLVMKit::ansadd(const void *a, const void *w) const {
//    llvm::Type *i32 = bldr.getInt32Ty();
//    llvm::LoadInst *a = bldr.CreateLoad(i32, (llvm::Value *)l);
//    llvm::LoadInst *w = bldr.CreateLoad(i32, (llvm::Value *)r);
    llvm::Value *add = bldr.CreateAdd((llvm::Value *)a, (llvm::Value *)w);
    llvm::ReturnInst *x = bldr.CreateRet(add);

    return x;
}

void *LLVMKit::extfn(const char *name) {
    llvm::Type *dbl = bldr.getDoubleTy();
    //llvm::Type *params[1] = {dbl};
    //auto dbl = llvm::Type::getDoubleTy(ctx);;
    //auto ft = llvm::FunctionType::get(dbl, {dbl}, false);
//    llvm::FunctionType//(bldr.getDoubleTy(), params, false);
    auto fc = mod->getOrInsertFunction(name, dbl, dbl);
    //llvm::Function *fn = fc.getCallee();
//    bldr.getDoubleTy()
 //   llvm::Type::getDoubleTy(ctx);
//    llvm::Function *fn = mod->getOrInsertFunction(name, dbl, dbl, nullptr);
    return fc.getCallee(); // llvm::Function *
}

void *LLVMKit::emitcall(const void *callee, const void *ft, const char *name, const double value) {
//    llvm::Value *callee;
//    llvm::FunctionType *ft;
    llvm::Value *arg = llvm::ConstantFP::get(bldr.getDoubleTy(), value);
    llvm::Value *args = {arg};
    llvm::CallInst *x = bldr.CreateCall((llvm::FunctionType *)ft, (llvm::Value *)callee, args, name);
    return x;
}

void *LLVMKit::emitcall(const void *callee, const char *name) {
    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), {}, false);
    llvm::CallInst *x = bldr.CreateCall(ft, (llvm::Function *)callee, {}, name);
    return x;
}


void *emitproto1(const char *name) {
    /*
    llvm::Function *fn = llvm::Function::Create(llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), false),
                                                llvm::Function::ExternalLinkage, "main", mod.get());

     LLVMValueRef LLVMAddFunction(LLVMModuleRef M, const char *Name, LLVMTypeRef FunctionTy) {
     return wrap(Function::Create(unwrap<FunctionType>(FunctionTy),
     GlobalValue::ExternalLinkage, Name, unwrap(M)));
     */
    llvm::Type *ret = llvm::Type::getDoubleTy(ctx);
    llvm::Type *a1 = llvm::Type::getDoubleTy(ctx);
    llvm::Type *a2 = llvm::Type::getDoubleTy(ctx);
    llvm::FunctionType *ft = llvm::FunctionType::get(ret, {a1, a2}, false);
    llvm::Function *fn = llvm::Function::Create(ft, llvm::GlobalValue::InternalLinkage, 0, name, mod.get());
    for (llvm::Argument &a: fn->args()) {
        a.setName("a1");
    }
    return fn;
}
void nameargs(llvm::ArrayRef<const char *>names) {
}

void *recallproto(const char *name) {
    // LLVMGetNamedFunction
    llvm::Function *f = mod->getFunction(name);
    return f;
}

void *LLVMKit::emitletcalldir(const char *name, const void *callee, const void *arg1) const {
    llvm::Type *i32 = llvm::Type::getInt32Ty(ctx);
#if 0
    // %name = alloca i32, align 4
    llvm::AllocaInst *ptr = bldr.CreateAlloca(i32, nullptr, name);
    ptr->setAlignment(llvm::Align(4));

    // %0 = load i32, ptr %arg1, align 4
    llvm::LoadInst *w = bldr.CreateLoad(i32, (llvm::Value *)arg1);
    w->setAlignment(llvm::Align(4));

    // %1 = call i32 @calee(i32 noundef %0)
    llvm::FunctionType *ft = llvm::FunctionType::get(i32, {i32}, false);
    llvm::CallInst *x = bldr.CreateCall(ft, (llvm::Value *)callee, {w});

    // store i32 %1, ptr %name, align 4
    llvm::StoreInst *y = bldr.CreateStore(x, ptr);
    y->setAlignment(llvm::Align(4));

    return ptr;
#endif
    // %1 = call i32 @calee(i32 noundef %0)
    llvm::FunctionType *ft = llvm::FunctionType::get(i32, {i32}, false);
    llvm::CallInst *x = bldr.CreateCall(ft, (llvm::Value *)callee, {(llvm::Value *)arg1}, name);
    return x;
}

// letcalldir(const char *name, const void *fn, const void *arg1, const void *arg2) const;
// letcallcls(const char *name, const void *fn, const void *arg1) const;
// letcallcls(const char *name, const void *fn, const void *arg1, const void *arg2) const;
// anscalldir(const char *name, const void *fn, const void *arg1) const;
// anscalldir(const char *name, const void *fn, const void *arg1, const void *arg2) const;
// anscallcls(const char *name, const void *fn, const void *arg1) const;
// anscallcls(const char *name, const void *fn, const void *arg1, const void *arg2) const;

// letmov
// letaddi
// letsetl
// letsti
// ansmov

// letldi
// letmul
// ansadd
// -----------

void LLVMKit::dump() const {
    mod->print(llvm::outs(), nullptr);
}
