#include "llvm.hpp"

class LLVMKit::Impl {
public:
    Impl(const char *name);
};

LLVMKit::Impl::Impl(const char *name) {}

LLVMKit::LLVMKit(const char *name): impl(new Impl(name)) {}

const void *LLVMKit::nop(void) const { return nullptr; }
const void *LLVMKit::ans(const void *value) const  { return value; }
const void *LLVMKit::set(const void *n, const int i) const { return n; }
const void *LLVMKit::add(const void *l, const void *r) const { return l; }
const void *LLVMKit::mul(const void *l, const void *r) const { return l; }
const void *LLVMKit::calldir(const char *name, const void *callee, const void *w) const { return name; }
const void *LLVMKit::callcls(const void *cl, const void *w) const { return cl; }
