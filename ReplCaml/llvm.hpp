#include "swift/bridging"
//#include "ReplCaml-Bridging-Swift.h"

#pragma once

class LLVMKit {
public:
    LLVMKit(const char *name);

    const void *nop(void) const;
    const void *ans(const void *value) const;
    const void *set(const void *n, const int i) const;
    const void *add(const void *l, const void *r) const;
    const void *mul(const void *l, const void *r) const;
    const void *calldir(const char *name, const void *callee, const void *w) const;
    const void *callcls(const void *cl, const void *w) const;

private:
    class Impl;
    Impl *impl;
};
