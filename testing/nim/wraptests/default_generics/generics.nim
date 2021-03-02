const cxheader = "generics.hpp"
# Even thought C++ struct has default template parameters,
# nim version overrides all of them.

type
  # Type for one of the default generic parameters
  Functor[T] {.importcpp: "Functor", header: cxheader.} = object

  # Base implementation with all necessary overloads in place
  GenBase[T, D] {.importcpp: "Gen", header: cxheader.} = object
    fld {.importcpp: "_fld".}: cint

  # Version with all default generic parameters filled in
  Gen[T] = GenBase[T, Functor[T]]

proc initFunctor[T](): Functor[T] {.importcpp: "Functor<'0>(@)".}

# Basic constructor proc allows to override only
# non-defaulted template parameters
proc initGen[T](arg: int): Gen[T] {.
    importcpp: "Gen<'0>(@)", constructor, header: cxheader.}

# Non-defaulted implementation, with support for passing all required generic
# parameters from nim side. In this case resulting type depends on argumments'
# types, it is easier to split implementation into two parts - non-defaulted
# one (this) and fully defaulted (defined down the code).
when true:
  # There are two ways of implementing this overload, and in some strange
  # edge cases you might get "invalid apostrophe type parameter index" error
  # when using simple `<'1, '2>` argument. Additionally, version with two
  # levels of indirection uses first and second parameter types, while direct
  # `{.importcpp.}` does not need this.
  proc initGenBase[T, D](arg: int, functor: D): GenBase[T, D] {.noinit.} =
    proc initGenBaseImpl(Ttype, DType: typedesc, arg: int):
      GenBase[T, D] {.importcpp: "Gen<'*1, '*2>(@)", header: cxheader.}

    return initGenBaseImpl(T, D, arg)

else:
  proc initGenBase[T, D](arg: int, functor: D): GenBase[T, D]
    {.importcpp: "Gen<'0, '1>(@)", constructor, header: cxheader.}



# Initialization of defaulted generic version follows definition of the original
# procedure. All default parameters resulted from analysing input source code.
proc initGenBase[T](
    arg: int, functor: Functor[T] = initFunctor[T]()
  ): GenBase[T, Functor[T]] =

  # Call 'base' implementation instead of providing additional `{.importcpp.}`
  # wrappers.
  initGenBase(arg, functor)


when false:
  # Originally error with importcpp apostrophe count was triggered by `new` wrapper,
  # so I decided to leave both implementations
  proc newGenBase[T, D](arg: int, arg: D): ptr GenBase[T, D] =
    proc newGenBaseImpl(T, D: typedesc, arg: int):
      ptr GenBase[T, D] {.importcpp: "new Gen<'*1, '*2>(@)", header: cxheader.}

    return newGenBaseImpl(T, D, arg)

else:
  proc newGenBase[T, D](arg: int, functor: D): ptr GenBase[T, D]
    {.importcpp: "new Gen<'0, '1>(@)", constructor, header: cxheader.}




proc main() =
  let val1 = initGen[string](5)
  let val2 = initGenBase[string, float](12, 1.2)
  let val3 = newGenBase[float, int](2, 2)
  echo val3.isNil()
  echo val3.fld

main()
