#pragma once

class LLVMKit {
public:
    LLVMKit(const char* name);
    void *emitfunc(const char* name) const;
    void *emitalloca(const char *name) const;
    void *emitstore(const void *ptr, const int value) const;
    void *emitload(const void *ptr, const char *name) const;
    void *emitret(const void *ptr) const;
    void *emitret(const int value) const;
    void *emitret(void) const;
    void *emitadd(const void *lptr, const void *rptr, const char *name) const;
    void *emitlet(const char *name, const int value) const;

    void *emitptrinc() const;  // closure pointer increment

    void *extfn(const char *name);
    void *emitcall(const void *callee, const void *ft, const char *name, const double value);
    void *emitcall(const void *callee, const char *name);

    void makeCls() const;
    void appCls() const;
    void appDir() const;

    void dump() const;

private:
    const char* modulename;
};

