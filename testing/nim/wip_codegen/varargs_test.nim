import std/[tables, lists]

template ceblock(str: static[string], body: untyped): untyped =
  block:
    echo str

    try:
      {.emit: "\n\n\n".}
      {.emit: ["/* ", str, "*/ /* 0 */"].}
      body
      {.emit: ["/* ", str, "*/ /* 1 */"].}
      {.emit: "\n\n\n".}

    except:
      echo "Failed at runtime with ", getCurrentException().name
      echo getCurrentExceptionMsg()

proc main() =
  type
    Base = object of RootObj
      fbase: int

    Derived = object of Base
      fderived: int

    CBase {.bycopy.} = object of RootObj
      fbase: int

    CDerived = object of CBase
      fderived: int


  ceblock "Derived to regular proc":
    proc impl(arg: Base) = echo arg
    impl(Derived())

  ceblock "Derived from bycopy to regular proc":
    proc impl(arg: CBase) = echo arg
    impl(CDerived())

  ceblock "Multiple derived to regular proc":
    proc impl(a1, a2, a3: Base) =
      echo a1
      echo a2
      echo a3

    impl(Derived(), Derived(), Base())

  ceblock "Multiple derived from bycopy to regular proc":
    proc impl(a1, a2, a3: CBase) =
      echo a1
      echo a2
      echo a3

    impl(CDerived(), CDerived(), CBase())

  ceblock "Multiple to varargs":
    proc impl(args: varargs[Base]) =
      for arg in args:
        echo arg

    ceblock "Single object":
      impl(Base())

    ceblock "Single derived":
      impl(Derived())

    ceblock "Multiple derived":
      impl(Derived(), Derived())

    ceblock "Derived and base":
      impl(Derived(), Base())


  ceblock "Multiple bycopy to varargs":
    proc impl(args: varargs[CBase]) =
      for arg in args:
        echo arg

    ceblock "Single object":
      impl(CBase())

    ceblock "Single derived":
      impl(CDerived())

    ceblock "Multiple Cderived":
      impl(CDerived(), CDerived())

    ceblock "CDerived and base":
      impl(CDerived(), CBase())

  ceblock "Add to seq":
    var s: seq[Base]
    ceblock "Add base":
      s.add Base()

    ceblock "Add derived":
      s.add Derived()

  ceblock "Caveman seq":
    type
      CaveSeq[T] = object
        data: array[10, T]
        len: int

    proc add[T](s: var CaveSeq[T], data: T) =
      s.data[s.len] = data
      inc s.len

    var s: CaveSeq[Base]
    ceblock "Add base":
      s.add Base()

    ceblock "Add derived":
      s.add Derived()

    echo s

  ceblock "Add to table":
    var t: Table[int, Base]
    t[0] = Base()
    t[1] = Derived()

    echo t

  ceblock "Lists":
    var l = initDoublyLinkedList[Base]()
    l.add Base()
    l.add Derived()

    echo l

  echo 1


main()