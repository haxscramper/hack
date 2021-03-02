#include "cppderived.hpp"


void CppBaseDerived::baseMethod(int arg) {
    if (this->baseMethodWrap == 0) {
        puts("--- No override used, fallback to default implementation\n");
        CppBase::baseMethod(arg);

    } else {
        puts("--- Using nim implementation\n");
        this->baseMethodWrap(
          this->derivedImpl, arg,
          this->baseMethodEnv, this->baseMethodProc);
    }
}
