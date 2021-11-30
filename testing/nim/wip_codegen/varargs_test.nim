import std/[tables, lists]

var level = 0


proc get(): string =
  case level:
    of 0: ""
    of 1: "  "
    of 2: "    "
    else: "      "


type
  Base = object of RootObj
    fbase: int

  Derived = object of Base
    fderived: int

echo Derived().Base()
let z = Derived().Base()

proc print[T](s: T) =
  echo get(), "  = ", $s

template ceblock(str: static[string], body: untyped): untyped =
  block:
    inc level
    echo get(), "--- \e[32m", str, "\e[0m"

    try:
      {.emit: "\n\n\n".}
      {.emit: ["/* ", str, "*/ /* 0 */"].}
      body
      {.emit: ["/* ", str, "*/ /* 1 */"].}
      {.emit: "\n\n\n".}

    except:
      echo get(), "  > \e[31m", getCurrentException().name, "\e[0m ",  getCurrentExceptionMsg()

    dec level

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
    proc impl(arg: Base) = print arg
    impl(Derived())

  ceblock "Derived from bycopy to regular proc":
    proc impl(arg: CBase) = print arg
    impl(CDerived())

  ceblock "Multiple derived to regular proc":
    proc impl(a1, a2, a3: Base) =
      print a1
      print a2
      print a3

    impl(Derived(), Derived(), Base())

  ceblock "Multiple derived from bycopy to regular proc":
    proc impl(a1, a2, a3: CBase) =
      print a1
      print a2
      print a3

    impl(CDerived(), CDerived(), CBase())

  ceblock "Multiple to varargs":
    proc impl(args: varargs[Base]) =
      for arg in args:
        print arg

    ceblock "Single object":
      impl(Base())

    ceblock "Single derived":
      impl(Derived())

    ceblock "Multiple derived":
      impl(Derived(), Derived())

    ceblock "Derived and base":
      impl(Derived(), Base())

    ceblock "Base and derived":
      impl(Base(), Derived())


  ceblock "Multiple bycopy to varargs":
    proc impl(args: varargs[CBase]) =
      for arg in args:
        print arg

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

    print s

  ceblock "Assign to var":
    var s: Base
    s = Derived().Base()

  ceblock "Add to table":
    var t: Table[int, Base]
    t[0] = Base()
    t[1] = Derived()

    print t

  ceblock "Lists":
    var l = initDoublyLinkedList[Base]()
    l.add Base()
    l.add Derived()

    print l

  print 1


main()