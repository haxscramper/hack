import f08_module

let hello = H()

type
  StrArr = object
    arr: cstringArray

proc `=destroy`(s: var StrArr): void =
  echo "deallocating cstring array"
  if s.arr != nil:
    deallocCStringArray(s.arr)

converter toCStringArray(s: StrArr): cstringArray = s.arr

converter toCStringArray(s: seq[string]): StrArr =
  StrArr(arr: allocCStringArray(s))

proc carr(cstr: StrArr): void =
  discard

carr(@["hello", "world"])
