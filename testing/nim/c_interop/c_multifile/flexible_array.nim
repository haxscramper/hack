type
  ImGuiTable {.importc, header: "flexible_array.h".} = object
    field: cint

  ImSeq {.importc, header: "flexible_array.h".} = seq[ImGuiTable]

echo 1
var s: ImSeq = @[ImGuiTable()]
echo 2
