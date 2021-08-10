import std/[macros, macrocache]

const
  implTable = CacheTable("impls")

type
  Data = ref object
    field1: int

macro test(): untyped =
  let data1 = Data(field1: 12)
  let d: int = cast[int](data1)
  # let n: Data = cast[Data](d)

  implTable["a"] = cast[NimNode](data1)

  let nodie: NimNode = implTable["a"]
  let data2: Data = cast[Data](nodie)

test()
