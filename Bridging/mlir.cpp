#pragma once

#include "mlir.hpp"

#include <mlir/IR/MLIRContext.h>
#include <mlir/IR/Builders.h>
#include <mlir/IR/BuiltinOps.h>
#include <mlir/Pass/PassManager.h>
#include <mlir/Conversion/ArithToLLVM/ArithToLLVM.h>
#include <mlir/Conversion/ComplexToLLVM/ComplexToLLVM.h>
#include <mlir/Conversion/MathToLLVM/MathToLLVM.h>
//#include <mlir/Conversion/VectorToLLVM/ConvertVectorToLLVM.h>
#include <mlir/Conversion/MemRefToLLVM/MemRefToLLVM.h>
#include <mlir/Conversion/Passes.h>
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include <mlir/Dialect/Math/IR/Math.h>
#include <mlir/Dialect/Arith/IR/Arith.h>
#include <mlir/Dialect/Complex/IR/Complex.h>
#include <mlir/Dialect/Func/IR/FuncOps.h>
#include <mlir/Dialect/MemRef/IR/MemRef.h>
//#include "mlir/Dialect/Affine/IR/AffineOps.h"
//#include "mlir/Dialect/Ptr/IR/PtrOps.h"
//#include "mlir/Dialect/Shape/IR/Shape.h"
//#include "mlir/Dialect/Tensor/IR/Tensor.h"
//#include "mlir/Dialect/Vector/IR/VectorOps.h"
#include <mlir/Target/LLVMIR/Dialect/Builtin/BuiltinToLLVMIRTranslation.h>
#include <mlir/Target/LLVMIR/Dialect/LLVMIR/LLVMToLLVMIRTranslation.h>
#include <mlir/Target/LLVMIR/ModuleTranslation.h>
#include <mlir/Target/LLVMIR/LLVMTranslationInterface.h>
//#include "mlir/Target/LLVMIR/Export.h"
#include <memory>

class MLIRKit::Impl {
public:
    Impl();
    ~Impl();

    void generateIR();

    mlir::IntegerType i32_t;
    mlir::Float32Type f32_t;
    mlir::ComplexType complex_t;

    inline mlir::Location loc() {
        return modBuilder->getUnknownLoc();
    }

    // llvm dialect
    inline mlir::Type llvm_int32_t(mlir::OpBuilder &bldr) {
        return bldr.getI32Type();
    }
    inline mlir::Type llvm_ptr_t(void) {
        return mlir::LLVM::LLVMPointerType::get(context.get());
    }
    inline mlir::LLVM::ConstantOp llvm_constant(mlir::Type typ, int64_t value) {
        return funcBuilder->create<mlir::LLVM::ConstantOp>(loc(), typ, value);  // llvm.mlir.constant
    }
    inline mlir::LLVM::AllocaOp llvm_alloca(mlir::TypeAttr t, mlir::Value n) {
        return funcBuilder->create<mlir::LLVM::AllocaOp>(loc(), llvm_ptr_t(), n, funcBuilder->getI64IntegerAttr(4), t);
    }
    inline mlir::LLVM::StoreOp llvm_store(mlir::Value value, mlir::Value ref) {
        return funcBuilder->create<mlir::LLVM::StoreOp>(loc(), value, ref, 4);
    }
    inline mlir::LLVM::LoadOp llvm_load(mlir::Type t, mlir::Value ref) {
        return funcBuilder->create<mlir::LLVM::LoadOp>(loc(), t, ref, 4);
    }

    // memref dialect
    inline mlir::memref::AllocaOp memref_alloca(mlir::MemRefType typ) {
        return funcBuilder->create<mlir::memref::AllocaOp>(loc(), typ);
    }
    inline mlir::memref::StoreOp memref_store(mlir::Value value, mlir::Value memref, mlir::ValueRange indices) {
        return funcBuilder->create<mlir::memref::StoreOp>(loc(), value, memref, indices);
    }
    inline mlir::memref::LoadOp memref_load(mlir::Value memref, mlir::ValueRange indices) {
        return funcBuilder->create<mlir::memref::LoadOp>(loc(), memref, indices);
    }

    // arith dialect
    inline mlir::arith::AddIOp arith_addi(mlir::Value lhs, mlir::Value rhs) {
        return funcBuilder->create<mlir::arith::AddIOp>(loc(), lhs, rhs);
    }
    inline mlir::arith::ConstantOp arith_constant_i32_op(int32_t value) {
        return funcBuilder->create<mlir::arith::ConstantIntOp>(loc(), value, i32_t);
    }
    inline mlir::arith::ConstantOp arith_constant_f32_op(mlir::FloatAttr attr) {
        return funcBuilder->create<mlir::arith::ConstantOp>(loc(), attr);
    }
    inline mlir::arith::ConstantOp arith_constant_f32_op(float value) {
        return arith_constant_f32_op(funcBuilder->getF32FloatAttr(value));
    }
    inline mlir::TypedValue<mlir::Type> arith_constant_v(mlir::arith::ConstantOp op) {
        return op.getResult();
    }
    inline mlir::TypedValue<mlir::Type> arith_constant_f32(float value) {
        return arith_constant_v(arith_constant_f32_op(value));
    }
    inline mlir::TypedValue<mlir::Type> arith_constant_i32(int32_t value) {
        return arith_constant_v(arith_constant_i32_op(value));
    }
    inline mlir::arith::ConstantOp arith_constant_index(int32_t value) {
        return funcBuilder->create<mlir::arith::ConstantIndexOp>(loc(), value);
    }

    // complex dialect
    inline mlir::complex::CreateOp complex(float real, float image) {
        return funcBuilder->create<mlir::complex::CreateOp>(loc(), complex_t, arith_constant_f32_op(real), arith_constant_f32_op(image));
    }
    inline mlir::complex::MulOp complex_mul(mlir::Value lhs, mlir::Value rhs) {
        return funcBuilder->create<mlir::complex::MulOp>(loc(), complex_t, lhs, rhs);
    }
    inline mlir::complex::ConjOp complex_conj(mlir::Value rhs) {
        return funcBuilder->create<mlir::complex::ConjOp>(loc(), complex_t, rhs);
    }

    // func dialect
    inline mlir::func::CallIndirectOp func_call_indirect(mlir::FunctionType ft, mlir::func::ConstantOp fc, mlir::ValueRange arg) {
        return funcBuilder->create<mlir::func::CallIndirectOp>(loc(), ft.getResults(), fc.getResult(), arg); // func.call_indirect
    }
    inline mlir::func::CallOp func_call(mlir::func::FuncOp fn, mlir::ValueRange args) {
        return funcBuilder->create<mlir::func::CallOp>(loc(), fn, args); // func.call
    }
    inline mlir::func::ConstantOp func_constant(mlir::FunctionType ft, mlir::StringRef name) {
        return funcBuilder->create<mlir::func::ConstantOp>(loc(), ft, name); // func.constant
    }
    inline mlir::func::FuncOp func_func(mlir::StringRef name, mlir::TypeRange arg, mlir::TypeRange ret) {
        mlir::FunctionType funcType = modBuilder->getFunctionType(arg, ret);
        mlir::func::FuncOp func = modBuilder->create<mlir::func::FuncOp>(loc(), name, funcType);    // func.func
        entryBlock = func.addEntryBlock(); // TODO: memory may leak
        mlir::OpBuilder t(entryBlock, entryBlock->begin());
        funcBuilder = std::make_shared<mlir::OpBuilder>(t);
        return func;
    }
    inline mlir::BlockArgument func_arg(unsigned int index) {
        return entryBlock->getArgument(index);
    }
    inline mlir::func::ReturnOp func_ret(mlir::TypedValue<mlir::Type> value) {
        return funcBuilder->create<mlir::func::ReturnOp>(loc(), value);    // func.ret
    }
    inline mlir::func::FuncOp func_lookup(mlir::StringRef name) {
        return mod->lookupSymbol<mlir::func::FuncOp>(name);
    }
    inline mlir::FunctionType getFunctionType(mlir::func::FuncOp fn) {
        return fn.getFunctionType();
    }

private:
    std::unique_ptr<mlir::DialectRegistry> registry;
    std::unique_ptr<mlir::MLIRContext> context;
    std::unique_ptr<mlir::ModuleOp> mod;
    std::unique_ptr<mlir::OpBuilder> modBuilder;
    std::shared_ptr<mlir::OpBuilder> funcBuilder;
    mlir::Block *entryBlock;
};

MLIRKit::MLIRKit() : impl(new Impl()) {}
MLIRKit::~MLIRKit() { delete impl; }

void MLIRKit::generateIR() { impl->generateIR(); }

MLIRKit::Impl::Impl() {
    registry = std::make_unique<mlir::DialectRegistry>();
    registry->insert<mlir::BuiltinDialect>();
    registry->insert<mlir::LLVM::LLVMDialect>();
    registry->insert<mlir::arith::ArithDialect>();
    registry->insert<mlir::complex::ComplexDialect>();
    registry->insert<mlir::func::FuncDialect>();
    registry->insert<mlir::memref::MemRefDialect>();

    mlir::registerBuiltinDialectTranslation(*registry);
    mlir::registerLLVMDialectTranslation(*registry);

    context = std::make_unique<mlir::MLIRContext>(*registry);
    context->loadAllAvailableDialects();
    mlir::OpBuilder bldr(context.get());
    mod = std::make_unique<mlir::ModuleOp>(mlir::ModuleOp::create(bldr.getUnknownLoc()));
    modBuilder = std::make_unique<mlir::OpBuilder>(mod->getBodyRegion());

    i32_t = mlir::IntegerType::get(context.get(), 32);
    f32_t = mlir::Float32Type::get(context.get());
    complex_t = mlir::ComplexType::get(f32_t);
}

MLIRKit::Impl::~Impl() {}

void MLIRKit::Impl::generateIR() {
    modBuilder->setInsertionPointToStart(mod->getBody());

    if (true) {
        mlir::func::FuncOp func = func_func("sample_function", {i32_t}, {i32_t});
        func.setPrivate();
        mlir::BlockArgument arg = func_arg(0);
        mlir::arith::AddIOp addOp = arith_addi(arg, arith_constant_i32(42));
        func_ret(addOp.getResult());
    }
    if (true) {
        mlir::func::FuncOp cmul = func_func("complex_mul", {complex_t, complex_t}, {complex_t});
        cmul.setPrivate();
        mlir::BlockArgument lhs = func_arg(0);
        mlir::BlockArgument rhs = func_arg(1);
        mlir::complex::MulOp mulOp = complex_mul(lhs, rhs);
        func_ret(mulOp.getResult());
    }
    if (true) {
        mlir::func::FuncOp func = func_func("memref_sample", {}, {i32_t});
        mlir::memref::AllocaOp pos = memref_alloca(mlir::MemRefType::get({1}, i32_t));
        mlir::Value index = arith_constant_index(0);
        memref_store(arith_constant_i32(123), pos, index);
        memref_load(pos, index);
        func_ret(arith_constant_i32(0));
    }
    if (true) {
        mlir::func::FuncOp func = func_func("complex_sample", {}, {i32_t});
        auto v = complex(3.0, 4.0);
        mlir::func::FuncOp cmul = func_lookup("complex_mul");
        func_call(cmul, {v, v});
        mlir::FunctionType cft = cmul.getFunctionType();
        mlir::func::ConstantOp fc = func_constant(cft, "complex_mul");
        func_call_indirect(cft, fc, {v, v});

        func_ret(arith_constant_i32(0));
    }
    if (true) {
        mlir::func::FuncOp func = func_func("main", {}, {i32_t});
        auto i1 = llvm_constant(i32_t, 1);
        auto i0 = llvm_constant(i32_t, 0);
        auto a = llvm_alloca(mlir::TypeAttr::get(i32_t), i1.getResult());
        llvm_store(i1, a);
        auto b = llvm_load(i32_t, a);

        func_ret(arith_constant_i32(0));
    }

    mod->print(llvm::outs());

    mlir::PassManager pm(context.get());
    pm.addPass(mlir::createConvertComplexToLLVMPass());
    pm.addPass(mlir::createArithToLLVMConversionPass());
//    pm.addPass(mlir::createConvertMathToLLVMPass());
    pm.addPass(mlir::createConvertFuncToLLVMPass());
    pm.addPass(mlir::createFinalizeMemRefToLLVMConversionPass());
    pm.addPass(mlir::createReconcileUnrealizedCastsPass());    // eliminates noop `unrealized_conversion_cast` operation sequences.
    if (failed(pm.run(*mod))) { llvm::errs() << "Failed to lower to LLVM dialect\n"; return; }

    llvm::LLVMContext llvmCtx;
    auto llvmModule = mlir::translateModuleToLLVMIR(*mod, llvmCtx);
    if (!llvmModule) { llvm::errs() << "Translation to LLVM IR failed\n"; return; }

    llvmModule->print(llvm::outs(), nullptr);
}
