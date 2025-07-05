#pragma once

class LLVMKit {
public:
    LLVMKit(const char* name);
    void *emitfunc(const char* name) const;
    void *arg(const void *fn, const int index, const char* name) const;
    void *arg(const void *fn, const int index) const;
    void *closure_arg(const int index, const char* name) const;
    void *ansadd(const void *l, const void *r) const;
    void *emitletset(const char *name, const int value) const;
    void *emitletcalldir(const char *name, const void *callee, const void *arg1) const;

    void *emitalloca(const char *name) const;
    void *emitstore(const void *ptr, const int value) const;
    void *emitload(const void *ptr, const char *name) const;
    void *emitret(const void *ptr) const;
    void *emitret(const int value) const;
    void *emitret(void) const;
    void *emitadd(const void *lptr, const void *rptr, const char *name) const;
    void *emitlet(const char *name, const int value) const;

    void *emitptrinc() const;  // closure pointer increment
    void *emitldi(const void *ptr, const void *dest, const int offset) const;

    void *makecls(const char* name) const;
    void *makecls(const char *name, const char *arg1) const;

    // letcalldir(const char *name, const void *fn, const void *arg1) const;
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


    //
// ans int
// ans mov
// ans add

    void *extfn(const char *name);
    void *emitcall(const void *callee, const void *ft, const char *name, const double value);
    void *emitcall(const void *callee, const char *name);

    void appCls() const;
    void appDir() const;

    void dump() const;

private:
    const char* modulename;
};

