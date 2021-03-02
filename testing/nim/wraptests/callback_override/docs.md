Base C++ class `cppbase.cpp`

```cpp
#pragma once

#include <stdio.h>

struct CppBase {
  virtual void baseMethod(int arg) {
    printf("arg from nim - %d -\n", arg);
  }
};
```

Automatically derived generated file - generated for every class definition during wrapping process.

**Generated derived class definition** `cppderived.hpp`

```cpp
#pragma once

#include <stdio.h>
#include "cppbase.hpp"

struct CppBaseDerived : public CppBase {
  // Callback for nim implementation.
  void (*baseMethodImpl)(void*, int);

  void baseMethodOverride(
    void* userData,  /// Custom user data
    int arg  /// Original argument to method
  );
};
```

Generated implementation for method **implementations** `cppderived.cpp`

```cpp
#include "cppderived.hpp"


void CppBaseDerived::baseMethodOverride(void* userdata, int arg) {
    if (this->baseMethodImpl == 0) {
        puts("--- No override used, fallback to default implementation\n");
        CppBase::baseMethod(arg);

    } else {
        puts("--- Using nim implementation\n");
        this->baseMethodImpl(userdata, arg);
    }
}
```

Wrappers `callbacks.nim`

```nim
const derivedHeader* = "cppderived.hpp"

{.compile: "cppderived.cpp".}

type
  CppBaseDerivedRaw* {.
    importcpp: "CppBaseDerived",
    header: derivedHeader
  .} = object

    baseMethodImplProc* {.importcpp: "baseMethodImpl".}:
      proc(userData: pointer, arg: cint) {.cdecl.}

  CppBaseDerived*[T] = object
    ## Wrapper object might (in theory) also serve as a way to manage CPP
    ## objects using nim memory management. Destruction heap-allocated object
    ## will be performed on `destroy=` hook. Using composition instead of
    ## pointer to implementation is also possible.

    d*: ptr CppBaseDerivedRaw ## Pointer to raw object implementation

    userData*: T ## Custom user data

    # Callback closure implementation, separated into underlying parts.
    clos: tuple[
      # C function callback, with additional argument for closure environment
      impl: proc(this: var CppBaseDerived[T], arg: int, env: pointer) {.cdecl.},

      # Pointer to environment itself
      env: pointer
    ]


proc setBaseMethod*[T](
    self: var CppBaseDerived[T],
    cb: proc(this: var CppBaseDerived[T], arg: cint)
  ) =

  # `{.cdecl.}` implementation callback that will be passed back to
  # raw derived class
  let implCallback = proc(userData: pointer, arg: cint ): void {.cdecl.} =
    # Uncast pointer to derived class
    var derived = cast[ptr CppBaseDerived[T]](userData)

    # Call closure implementation, arguments and closure environment.
    derived.clos.impl(derived[], arg, derived.clos.env)


  self.d.baseMethodImplProc = implCallback
  self.clos.env = cb.rawEnv()
  self.clos.impl = cast[CppBaseDerived[T].clos.impl](cb.rawProc())

proc newCppBaseDerivedRaw(): ptr CppBaseDerivedRaw
  # Implementation for raw object
  {.
    importcpp: "new CppBaseDerived(@)",
    constructor,
    header: derivedHeader
  .}

proc newCppBaseDerived*[T](): CppBaseDerived[T] =
  ## Wrapper constructor. All implementation detauls for closure will be
  ## set using `setBaseMethod`, so we only initialize base object.
  CppBaseDerived[T](d: newCppBaseDerivedRaw())

proc baseMethod*[T](derived: var CppBaseDerived[T], arg: int): void =
  proc baseMethod(
    impl: ptr CppBaseDerivedRaw,
    userData: pointer,
    arg: int
  ): void {.importcpp: "#.baseMethodOverride(@)", header: derivedHeader.}

  baseMethod(derived.d, cast[pointer](addr derived), arg)
```

To override **behavior** of the class you can set implementation callback to a new functions:

`main.nim`

```nim
import callbacks



proc main() =
  var derived = newCppBaseDerived[int]()

  let capture = "hello"

  derived.setBaseMethod proc(this: var CppBaseDerived[int], arg: cint) =
      echo capture
      echo "Override callback with nim implementation", arg

  derived.baseMethod(12)

main()
```

But I still can provide override for behavior of the object without actually overriding anything, which might be quite useful for various 'DelegatePainter' OOP patterns, where you actually only want to overide implementation of a single method and nothing else. With support for passing user data, and setting closures as implementation (and not just `{.cdecl.}` callbacks) it won't be necessary to derive from C++ classes in most cases anyway.

-&#x2014;

In rarer cases where you'd actually need to provide full-fledged derived class, it is possible to implement some codegen facilities.

I couldn't find a way to generate **standalone** files that can be injected in nim object hierarchy (at least without some ugly hacks). In order to derive from C++ class I would generate actual C++ class via nim macros, similarly to nim by example [macros](https://nim-by-example.github.io/macros/).

```nim
cxxClass NewCxx of CppDerived:
  field: int
  proc newMethod(): NI
```

Will generate following C++ code:

```cpp
class NewCxx : public CppDerived {
  NI field;
  NI newMethod(){
    return newMethod_nimImpl(); // Actual implementation of nim method is
                                // declared in nim code.
  }
}
```

Actually generating C++ code also helps with Qt - I no longer need to reimplement MOC, and instead can just use it as-is.

---

After some testing with codegen I think it is the best solution overall, but there are some issues, like making nim-declared types available in generated C++ code. If `NewCxx` uses non-trivial nim type - how to make it available in generated header?

Also 'derived' clases still won't behave as proper OOP on the nim side - e.g. I'd need to provide additional overloads for all procs.
