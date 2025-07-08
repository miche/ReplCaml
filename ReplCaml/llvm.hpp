#include "swift/bridging"
//#include "ReplCaml-Bridging-Swift.h"
#include <string>

#pragma once

namespace llvm {  // becomes enum in swift, may be
class Type;     // super class of ArrayType, FunctionType, IntegerType, PointerType, StructType, VectorType
class Value;    // super class of Argument, BasicBlock, Constant(ConstantInt/ConstantFP/GEP), Instruction
class Function; // subclass of Value,User,Constant,GlobalValue,GlobalObject
}
typedef llvm::Type *type_t;
typedef llvm::Value *value_t;
typedef llvm::Function *func_t;
typedef llvm::Value *closure_t;
typedef const char *str_t;

class LLVMKit {
public:
    LLVMKit(str_t name);

    value_t nop(void) const;
    value_t ans(value_t value) const;
    value_t set(str_t name, const int value) const;
    value_t add(value_t a, value_t w) const;
    value_t mul(value_t a, value_t w) const;
    value_t calldir(str_t name, func_t callee, value_t w) const;
    value_t callcls(closure_t cl, value_t w) const;
    func_t makecls(str_t name, str_t w, type_t typ) const;
    value_t arg(func_t link, const int index) const;
    value_t closure_arg(closure_t f, const int index, str_t n) const;
    closure_t makeclosure(func_t cl, value_t w, type_t t) const;
    func_t entry(str_t name) const;

    func_t emitfunc(str_t name) const;
    value_t emitcall(func_t link) const;
    value_t emitret(value_t value) const;
    std::string dump(void) const;

    type_t i32;
    type_t dbl;
    type_t ptr;
    type_t unit;

private:
    class Impl;
    Impl *impl;
};

