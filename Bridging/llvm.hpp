#pragma once

class LLVMKit {
public:
    LLVMKit(const char *name);

//    void *makecls(const char *name) const;
    void *makecls(const char *name, const char *w, const bool ret_fun = false) const;
//    void *makecls(const char *name, const char *a, const char *w) const;
    void *makeclosure(const void *fn, const void *w) const;
    
    void *entry(const char *name) const;
    void *arg(const void *fn, const int index) const;
    void *closure_arg(const void *fn, const int index, const char *name) const;

    void *set(const char *name, const int value) const;
    void *calldir(const char *name, const void *callee, const void *w) const;
    void *calldir(const char *name, const void *callee, const void *a, const void *w) const;

    void *ans(const void *value) const;
    void *add(const void *l, const void *r) const;
    void *callcls(const void *cl, const void *w) const;

    void dump() const;

    void *emitfunc(const char *name) const;
    void *emitret(const void *ptr) const;
    void *emitret(const int value) const;
    void *emitret(void) const;
    void *emitcall(const void *callee, const char *name) const;

    void *emitalloca(const char *name) const;
    void *emitstore(const int value, const void *ptr) const;
    void *emitload(const void *ptr, const char *name) const;
    void *emitlet(const char *name, const int value) const;

private:
    class Impl;
    Impl *impl;
};

