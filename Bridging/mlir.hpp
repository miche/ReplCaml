#pragma once

class MLIRKit {
public:
    MLIRKit();
    ~MLIRKit();

    void generateIR();

private:
    class Impl;
    Impl *impl;
};
