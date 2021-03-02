{.emit: """/*INCLUDESECTION*/
#include <stdio.h>
""".}

{.emit: """/*TYPESECTION*/
class CppBase {
  public:
    virtual void baseMethod(int arg) {
      printf("arg from nim - %d -\n", arg);
    }
};

class NimInterface : public CppBase {
  public:
    TNimType* m_type;
};
""".}

type
  NimInterface {.importcpp: "NimInterface".} = object of RootObj

method baseMethod(ni: NimInterface, arg: int): void =
  {.emit: "`ni`->baseMethod(`arg`);".}


# Emit override is declared from `Derived` type
{.emit: """/*TYPESECTION*/

struct Derived;

extern "C" N_LIB_PRIVATE N_NIMCALL(void, baseMethod_implForDerived)(
    Derived* d,
    NI       arg);

struct Derived : public NimInterface {
  void baseMethod(int arg) override {
    baseMethod_implForDerived(this, arg);
  }

  NF f1;
};
""".}

type
  Derived {.importcpp: "Derived".} = object of NimInterface
    f1: float

# Generated from `{.cOverrideMethod.}` pragma on method implementation
proc baseMethod_implForDerived(d: Derived, arg: int): void {.exportc.} =
  echo "Nim override for method"


Derived().baseMethod(12)
NimInterface().baseMethod(22)

import std/macros

macro sighash(s: typed) =
  echo signatureHash(s)

sighash(NimInterface)
