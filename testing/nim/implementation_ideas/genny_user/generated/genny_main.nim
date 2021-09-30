import bumpy, chroma, unicode, vmath

export bumpy, chroma, unicode, vmath

when defined(windows):
  const libName = "genny_main.dll"
elif defined(macosx):
  const libName = "libgenny_main.dylib"
else:
  const libName = "libgenny_main.so"

{.push dynlib: libName.}

type genny_mainError = object of ValueError

type Obj* = object
  field*: int

proc obj*(field: int): Obj =
  result.field = field

proc genny_main_obj_get_data(o: Obj): int {.importc: "genny_main_obj_get_data", cdecl.}

proc getData*(o: Obj): int {.inline.} =
  result = genny_main_obj_get_data(o)

