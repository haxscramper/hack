import std/[macros]

const cxheader = "header.hpp"
{.compile: "header.cpp".}

macro `//`(arg: string): untyped =
  let lit = newLit("/* " & arg.strVal() & " */")

  quote do:
    {.emit: `lit`.}

#==============================  Aggregate  ==============================#

type
  Aggregate {.importcpp, header: cxheader.} = object
    fld1: cint
    fld2: cchar

proc initAggregate(): Aggregate
    {.importcpp: "Aggregate(@)", header: cxheader.}

proc initAggregate(fld1: cint, fld2: cchar = '2'): Aggregate
 {.importcpp: "Aggregate({@})", header: cxheader.}

#==========================  Initializer list  ===========================#

type
  StdInitializerList*[T] {.
    importcpp: "std::initializer_list",
    header: "<initializer_list>"
  .} = object

proc cxxInitList*[T](args: T): StdInitializerList[T]
  {.importcpp: "{@}", varargs, constructor.}


#================================  List  =================================#

type
  List {.importcpp, header: cxheader.} = object

proc initList(args: cint): List
  {.importcpp: "List({@})", varargs, header: cxheader.}

proc initList(args: StdInitializerList[cint]): List
  {.importcpp: "List(@)", header: cxheader.}

#===============================  Functor  ===============================#

type
  Functor[T] {.importcpp, header: cxheader.} = object

  WhatFunctor[T] {.importcpp: "What<'0>::functor", header: cxheader} = object

  What[T] {.importcpp, header: cxheader.} = object
    f*: WhatFunctor[T]

proc initWhatFunctor[T](): WhatFunctor[T] {.noinit.} =
  proc aux(Targ: typedesc): WhatFunctor[T]
    {.importcpp: "What<'*1>::functor()", header: cxheader.}

  aux(T)

proc initWhat[T](functor: WhatFunctor[T] = initWhatFunctor[T]()): What[T] {.noinit.} =
  proc aux(Targ: typedesc, val: WhatFunctor[T]): What[T]
    {.importcpp: "What<'*1>(@)", header: cxheader.}

  // "Immediately assign to results, avoid copying"
  aux(T, functor)



#==========================  Overloaded procs  ===========================#

proc aggregateParam(arg: Aggregate = initAggregate(12, 'i')): void
  {.importcpp: "aggregateParam(@)", header: cxheader.}

proc initListParam(arg: List = initList(1,2,3,4,5)): void
  {.importcpp: "initListParam(@)", header: cxheader.}

proc mainFunc*() {.nimcall.} =
  block:
    // "Using default constructor"
    let val = initAggregate()

  block:
    // "Aggregate initialization constructor"
    let aggr2 = initAggregate(-90, 'z')
    aggregateParam(aggr2)

    // "Can also use only one argument for aggregate"
    // "And use name arguments"
    discard initAggregate(fld1 = 22)

    // "Function with aggregate-initialized default values"
    aggregateParam()



  block:
    // "Call constructor"
    let list1 = initList(12,3,4,5,56,6)

    // "Pass value of default parameter"
    initListParam(list1)

    // "Use default initalization values"
    initListParam()

    // "Create C++ initializer list literal"
    let list = initList(cxxInitList(1.cint))


  block:
    let val = initWhat[cint]()


mainFunc()
