const derivedHeader* = "cppderived.hpp"

{.compile: "cppderived.cpp".}

type
  CppBaseDerivedRaw* {.
    importcpp: "CppBaseDerived",
    header: derivedHeader
  .} = object

    derivedImpl*: pointer

    baseMethodWrap*: proc(derivedImpl: pointer, arg: cint,
                          closureEnv, closureProc: pointer) {.cdecl.}

    baseMethodProc*: pointer
    baseMethodEnv*: pointer


  CppBaseDerived*[T] = object
    ## Wrapper object might (in theory) also serve as a way to manage CPP
    ## objects using nim memory management. Destruction heap-allocated object
    ## will be performed on `destroy=` hook. Using composition instead of
    ## pointer to implementation is also possible.

    d*: ptr CppBaseDerivedRaw ## Pointer to raw object implementation
    derivedImpl*: T ## Custom user data

func closureToCdecl[T0, T1](
    cb: proc(a: var T0, b: T1) {.closure.}
  ): proc(a: var T0, b: T1, env: pointer) {.cdecl.} =

  discard


template setMethodImpl*(self, cb: typed, methodName: untyped): untyped =
  static: assert cb is proc
  type
    ClosImplType = typeof(closureToCdecl(cb))
    SelfType = typeof(self)

  # `{.cdecl.}` implementation callback that will be passed back to
  # raw derived class
  let wrap = proc(
    derivedImpl: pointer, arg: cint,
    cbEnv, cbImpl: pointer): void {.cdecl.} =

    # Uncast pointer to derived class
    var derived = cast[ptr SelfType](derivedImpl)

    # Call closure implementation, arguments and closure environment.
    cast[ClosImplType](cbImpl)(derived[], arg, cbEnv)


  self.d.`methodName Wrap` = wrap
  self.d.derivedImpl = addr self
  self.d.`methodName Env` = cb.rawEnv()
  self.d.`methodName Proc` = cb.rawProc()

proc setBaseMethod*[T](
    self: var CppBaseDerived[T],
    cb: proc(this: var CppBaseDerived[T], arg: cint) {.closure.}
  ) =

  static: assert cb is proc
  type
    ClosImplType = typeof(closureToCdecl(cb))
    SelfType = typeof(self)

  # `{.cdecl.}` implementation callback that will be passed back to
  # raw derived class
  let wrap = proc(
    derivedImpl: pointer, arg: cint,
    cbEnv, cbImpl: pointer): void {.cdecl.} =

    # Uncast pointer to derived class
    var derived = cast[ptr SelfType](derivedImpl)

    # Call closure implementation, arguments and closure environment.
    cast[ClosImplType](cbImpl)(derived[], arg, cbEnv)


  self.d.baseMethodWrap = wrap
  self.d.derivedImpl = addr self
  self.d.baseMethodEnv = cb.rawEnv()
  self.d.baseMethodProc = cb.rawProc()


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
    arg: int
  ): void {.importcpp: "#.baseMethod(@)", header: derivedHeader.}

  baseMethod(derived.d, arg)


