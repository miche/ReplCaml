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
    llvm::Type *t = llvm::ArrayType::get(st, num_of_closures);
    llvm::ConstantAggregateZero *zero = llvm::ConstantAggregateZero::get(t);
    closures = new llvm::GlobalVariable(*mod, t, false, llvm::GlobalVariable::InternalLinkage, zero, "closures");
    closures->setAlignment(llvm::Align(4));

    llvm::PointerType *pt = llvm::PointerType::get(t, 0);
    closureptr = new llvm::GlobalVariable(*mod, pt, false, llvm::GlobalVariable::InternalLinkage, closures, "closureptr");
    closureptr->setAlignment(llvm::Align(8));
}

void *LLVMKit::emitptrinc() const {
    llvm::Type *st = llvm::StructType::getTypeByName(ctx, "ClosureT");
    llvm::LoadInst *ptr = bldr.CreateLoad(bldr.getPtrTy(), closureptr, "clptr");
    llvm::Value *ptrtmp = bldr.CreateInBoundsGEP(st, ptr, {bldr.getInt32(1)});
    llvm::StoreInst *x = bldr.CreateStore(ptrtmp, closureptr);
    return x;
}

void *LLVMKit::emitfunc(const char* name) const {
    // define i32 @name() {
    llvm::Function *fn = llvm::Function::Create(llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), 0),
                                                llvm::Function::ExternalLinkage, name, mod.get());
    // entry:
    llvm::BasicBlock *bb = llvm::BasicBlock::Create(ctx, "entry", fn);
    bldr.SetInsertPoint(bb);    // move insert point to the end of bb, ready to insert, needed?

    return fn;
}



void *LLVMKit::emitalloca(const char *name) const {
    // %name = alloca i32, align 4
    llvm::AllocaInst *v = bldr.CreateAlloca(bldr.getInt32Ty(), /* ArraySize */ nullptr, /* Twine */name);
    return v;
}
void *LLVMKit::emitstore(const void *ptr, const int value) const {
    // store i32 value, ptr %name, align 4
    llvm::StoreInst *x = bldr.CreateStore(bldr.getInt32(value), (llvm::Value *)ptr);
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
    bldr.getDoubleTy();
    llvm::Value *arg = llvm::ConstantFP::get(llvm::Type::getDoubleTy(ctx), value);
    llvm::Value *args = {arg};
    llvm::CallInst *x = bldr.CreateCall((llvm::FunctionType *)ft, (llvm::Value *)callee, args, name);
    return x;
}

void *LLVMKit::emitcall(const void *callee, const char *name) {
    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), {}, false);
    llvm::CallInst *x = bldr.CreateCall(ft, (llvm::Function *)callee, {}, name);
    return x;
}

void functype() {

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
// -----------

void LLVMKit::dump() const {
    mod->print(llvm::outs(), nullptr);
}
