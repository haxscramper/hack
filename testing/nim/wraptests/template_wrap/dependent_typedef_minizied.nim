type
  CharTraits[CharT] = object
  Allocator[T] = object
    sizeType: cint

  BasicString[CharT, Traits, Alloc] = object
  String = BasicString[cchar, CharTraits[cchar], Allocator[cchar]]

proc sizeType[C, T, A](self: typedesc[BasicString[C, T, A]]): Allocator[C] =
  discard

proc npos[CharT, Traits, Alloc](self: BasicString[CharT, Traits, Alloc]): auto =
  if false:
    var tmp: typeof sizeType(BasicString[CharT, Traits, Alloc])
    return tmp

var test: String
echo typeof(test.npos())
echo test.npos()

