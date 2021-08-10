import
  std/typetraits

type
  StdCharTraits[CharT] {.importcpp: "std::char_traits".} = object
  StdAllocator[Alloc] {.importcpp: "std::allocator", header: "<memory>".} = object

  StdBasicString[CharT, Traits, Alloc] {.
    importcpp: "std::basic_string", header: "<string>"} = object

  CxxBase {.inheritable, pure.} = object
  CxxNormalIteratorBase[It, Container] = object of CxxBase
  CxxPointerBase[P] = object of CxxBase

  StdBasicStringConstPointer[CharT, Traits, Alloc] {.
    importcpp: "std::basic_string<'0, '1, '2>::const_pointer",
    header: "<string>",
    byref
  .} = object of CxxPointerBase[CharT]

  StdBasicStringConstIterator[CharT, Traits, Alloc] {.
    importcpp: "std::basic_string<'0, '1, '2>::const_iterator",
    header: "<string>",
    byref
  .} = object of
    CxxNormalIteratorBase[
      StdBasicStringConstPointer[CharT, Traits, Alloc],
      StdBasicString[CharT, Traits, Alloc]
    ]

  StdString = StdBasicString[char, StdCharTraits[char], StdAllocator[char]]

proc cbegin[C, T, A](str: StdBasicString[C, T, A]):
  StdBasicStringConstIterator[C, T, A] {.importcpp: "#.cbegin()".}

proc `+=`(s: var StdString, other: cstring) {.importcpp: "#.operator+=(@)".}

proc `[]`[P](it: CxxPointerBase[P]): P {.importcpp: "*#".}
proc `[]`[It, Container](it: CxxNormalIteratorBase[It, Container]): auto =
  if false:
    {.emit: "\n#if false".}
    var tmp: It
    static:
      echo typeof tmp
      echo typeof tmp[]

    return tmp[]
    {.emit: "\n#endif".}

  else:
    {.emit: "return *`it`;".}

proc iostdAux() {.header: "<iostream>", importcpp: "//".}


proc test[C, T, A](it: StdBasicString[C, T, A]): StdBasicStringConstIterator[C, T, A] =
  it.cbegin()

proc main() =
  var str: StdString
  str += "01234".cstring
  var iter = str.test()

  {.emit: """
std::cout << *`iter` << "\n";
std::cout << *`iter` << "\n";
std::cout << *`str`.cbegin() << "\n";
""".}

  let ch: char = iter[]
  echo "[[", ch,  "]]"
  ioStdAux()

main()
