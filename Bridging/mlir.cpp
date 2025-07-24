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
//#include "llvm/IR/Module.h"  // TODO:
//#include "llvm/Support/TargetSelect.h"  // TODO:
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
        return loc(*modBuilder);
    }
    inline mlir::Location loc(mlir::OpBuilder &bldr) {
        return bldr.getUnknownLoc();
    }
    inline mlir::TypedValue<mlir::Type> i32_v(mlir::arith::ConstantOp op) {
        return op.getResult();
    }
    inline mlir::TypedValue<mlir::Type> i32(mlir::OpBuilder &bldr, int32_t value) {
        return i32_v(i32_op(bldr, value));
    }
    inline mlir::TypedValue<mlir::Type> f32_v(mlir::arith::ConstantOp op) {
        return op.getResult();
    }
    inline mlir::TypedValue<mlir::Type> f32(mlir::OpBuilder &bldr, float value) {
        return f32_v(f32_op(bldr, value));
    }

    inline mlir::TypedValue<mlir::Type> i32(int32_t value) {
        return i32_v(i32_op(value));
    }


    inline mlir::func::FuncOp lookup(mlir::StringRef name) {
        return mod->lookupSymbol<mlir::func::FuncOp>(name);
    }
    inline mlir::FunctionType getFunctionType(mlir::func::FuncOp fn) {
        return fn.getFunctionType();
    }

    // llvm dialect
    inline mlir::Type llvm_int32_t(mlir::OpBuilder &bldr) {
        return bldr.getI32Type();
    }
    inline mlir::Type llvm_ptr_t(void) {
        return mlir::LLVM::LLVMPointerType::get(context.get());
    }
    // llvm.mlir.constant
    inline mlir::LLVM::ConstantOp llvm_constant(mlir::OpBuilder &bldr, mlir::Type typ, int64_t value) {
        return bldr.create<mlir::LLVM::ConstantOp>(loc(), typ, value);
    }
    inline mlir::LLVM::AllocaOp llvm_alloca(mlir::OpBuilder &bldr, mlir::TypeAttr t, mlir::Value n) {
        return bldr.create<mlir::LLVM::AllocaOp>(loc(), llvm_ptr_t(), n, bldr.getI64IntegerAttr(4), t);
    }
    inline mlir::LLVM::StoreOp llvm_store(mlir::OpBuilder &bldr, mlir::Value value, mlir::Value ref) {
        return bldr.create<mlir::LLVM::StoreOp>(loc(), value, ref, 4);
    }
    inline mlir::LLVM::LoadOp llvm_load(mlir::OpBuilder &bldr, mlir::Type t, mlir::Value ref) {
        return bldr.create<mlir::LLVM::LoadOp>(loc(), t, ref, 4);
    }

    // memref dialect
    inline mlir::memref::AllocaOp memref_alloca(mlir::OpBuilder &bldr, mlir::MemRefType typ) {
        return bldr.create<mlir::memref::AllocaOp>(loc(), typ);
    }
    inline mlir::memref::StoreOp memref_store(mlir::OpBuilder &bldr, mlir::Value value, mlir::Value memref, mlir::ValueRange indices) {
        return bldr.create<mlir::memref::StoreOp>(loc(), value, memref, indices);
    }
    inline mlir::memref::LoadOp memref_load(mlir::OpBuilder &bldr, mlir::Value memref, mlir::ValueRange indices) {
        return bldr.create<mlir::memref::LoadOp>(loc(), memref, indices);
    }

    // arith dialect
    inline mlir::arith::AddIOp arith_addi(mlir::OpBuilder &bldr, mlir::Value lhs, mlir::Value rhs) {
        return bldr.create<mlir::arith::AddIOp>(loc(), lhs, rhs);
    }
    inline mlir::arith::ConstantOp i32_op(mlir::OpBuilder &bldr, int32_t value) {
        return bldr.create<mlir::arith::ConstantIntOp>(loc(bldr), value, i32_t);
    }
    inline mlir::arith::ConstantOp f32_op(mlir::OpBuilder &bldr, mlir::FloatAttr attr) {
        return bldr.create<mlir::arith::ConstantOp>(loc(bldr), attr);
    }
    inline mlir::arith::ConstantOp f32_op(mlir::OpBuilder &bldr, float value) {
        return f32_op(bldr, bldr.getF32FloatAttr(value));
    }

    inline mlir::arith::AddIOp arith_addi(mlir::Value lhs, mlir::Value rhs) {
        return funcBuilder->create<mlir::arith::AddIOp>(loc(), lhs, rhs);
    }
    inline mlir::arith::ConstantOp i32_op(int32_t value) {
        return funcBuilder->create<mlir::arith::ConstantIntOp>(loc(), value, i32_t);
    }

    // complex dialect
    inline mlir::complex::CreateOp complex(mlir::OpBuilder &bldr, float real, float image) {
        return bldr.create<mlir::complex::CreateOp>(loc(bldr), complex_t, f32_op(bldr, real), f32_op(bldr, image));
    }
    inline mlir::complex::MulOp complex_mul(mlir::OpBuilder &bldr, mlir::Value lhs, mlir::Value rhs) {
        return bldr.create<mlir::complex::MulOp>(loc(), complex_t, lhs, rhs);
    }
    inline mlir::complex::ConjOp complex_conj(mlir::OpBuilder &bldr, mlir::Value rhs) {
        return bldr.create<mlir::complex::ConjOp>(loc(), complex_t, rhs);
    }

    // func dialect
    inline mlir::func::CallIndirectOp func_call_indirect(mlir::OpBuilder &bldr, mlir::FunctionType ft, mlir::func::ConstantOp fc, mlir::ValueRange arg) {
        return bldr.create<mlir::func::CallIndirectOp>(loc(), ft.getResults(), fc.getResult(), arg); // func.call_indirect
    }
    inline mlir::func::CallOp func_call(mlir::OpBuilder &bldr, mlir::func::FuncOp fn, mlir::ValueRange args) {
        return bldr.create<mlir::func::CallOp>(loc(), fn, args); // func.call
    }
    inline mlir::func::ConstantOp func_constant(mlir::OpBuilder &bldr, mlir::FunctionType ft, mlir::StringRef name) {
        return bldr.create<mlir::func::ConstantOp>(loc(), ft, name); // func.constant
    }
    inline mlir::func::FuncOp func_func(mlir::OpBuilder &bldr, mlir::StringRef name, mlir::FunctionType ft) {
        return bldr.create<mlir::func::FuncOp>(loc(), name, ft);    // func.func
    }
    inline mlir::func::ReturnOp func_ret(mlir::OpBuilder &bldr, mlir::TypedValue<mlir::Type> value) {
        return bldr.create<mlir::func::ReturnOp>(loc(), value);    // func.ret
    }
    inline mlir::func::ReturnOp func_ret(mlir::TypedValue<mlir::Type> value) {
        return funcBuilder->create<mlir::func::ReturnOp>(loc(), value);    // func.ret
    }

private:
    std::unique_ptr<mlir::DialectRegistry> registry;
    std::unique_ptr<mlir::MLIRContext> context;
    std::unique_ptr<mlir::ModuleOp> mod;
    std::unique_ptr<mlir::OpBuilder> modBuilder;
    std::shared_ptr<mlir::OpBuilder> funcBuilder;
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
        mlir::FunctionType funcType = modBuilder->getFunctionType({i32_t}, {i32_t});
        mlir::func::FuncOp func = func_func(*modBuilder, "sample_function", funcType);
        func.setPrivate();
        mlir::Block &entryBlock = *func.addEntryBlock();
        mlir::OpBuilder t(&entryBlock, entryBlock.begin());
        funcBuilder = std::make_shared<mlir::OpBuilder>(t);
        mlir::BlockArgument arg = entryBlock.getArgument(0);
//        mlir::arith::AddIOp addOp = arith_addi(funcBuilder, arg, i32(funcBuilder, 42));
//        func_ret(funcBuilder, addOp.getResult());
        mlir::arith::AddIOp addOp = arith_addi(arg, i32(42));
        func_ret(addOp.getResult());
    }
    if (true) {
        mlir::FunctionType cft = modBuilder->getFunctionType({complex_t, complex_t}, {complex_t});
        mlir::func::FuncOp cmul = modBuilder->create<mlir::func::FuncOp>(loc(), "complex_mul", cft);
        cmul.setPrivate();
        mlir::Block &entryBlock = *cmul.addEntryBlock();
        mlir::OpBuilder funcBuilder(&entryBlock, entryBlock.begin());
        mlir::BlockArgument lhs = entryBlock.getArgument(0);
        mlir::BlockArgument rhs = entryBlock.getArgument(1);
        auto mulOp = complex_mul(funcBuilder, lhs, rhs);
        func_ret(funcBuilder, mulOp.getResult());
    }
    if (true) {
        auto funcType = modBuilder->getFunctionType({}, {i32_t});
        auto func = modBuilder->create<mlir::func::FuncOp>(loc(), "main", funcType);
        auto &entryBlock = *func.addEntryBlock();
        mlir::OpBuilder funcBuilder(&entryBlock, entryBlock.begin());
        //builder->setInsertionPoint(&entryBlock, entryBlock.begin());

//        auto pos = memref_alloca(funcBuilder, mlir::MemRefType::get({1}, i32_t));
//        mlir::Value index = funcBuilder.create<mlir::arith::ConstantIndexOp>(loc(), 0);
//        memref_store(funcBuilder, i32(funcBuilder, 123), pos, index);
//        memref_load(funcBuilder, pos, index);

        auto i1 = llvm_constant(funcBuilder, i32_t, 1);
        auto i0 = llvm_constant(funcBuilder, i32_t, 0);
        auto a = llvm_alloca(funcBuilder, mlir::TypeAttr::get(i32_t), i1.getResult());
        llvm_store(funcBuilder, i1, a);
        auto b = llvm_load(funcBuilder, i32_t, a);

        auto cmul = lookup("complex_mul");
        auto v = complex(funcBuilder, 3.0, 4.0);
        func_call(funcBuilder, cmul, {v, v});

        mlir::FunctionType cft = cmul.getFunctionType();
        mlir::func::ConstantOp fc = func_constant(funcBuilder, cft, "complex_mul");
        func_call_indirect(funcBuilder, cft, fc, {v, v});

        func_ret(funcBuilder, i32(funcBuilder, 0));
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
