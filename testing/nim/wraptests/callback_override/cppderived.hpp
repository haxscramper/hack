#pragma once

#include <stdio.h>
#include "cppbase.hpp"

struct CppBaseDerived : public CppBase {
  // Pointer to wrapper object
  void* derivedImpl;

  // Callback for nim implementation.
  void (*baseMethodWrap)(void* derivedImpl, int arg, void* closureEnv, void* closureProc);
  void* baseMethodProc;
  void* baseMethodEnv;

  void baseMethod(int arg) override;
};

