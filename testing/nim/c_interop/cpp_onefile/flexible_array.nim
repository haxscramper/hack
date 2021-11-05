type
  ImGuiTable {.importcpp, header: "flexible_array.hpp".} = object
    field: cint

  ImSeq {.importcpp, header: "flexible_array.hpp".} = seq[ImGuiTable]

var q: seq[int]

var s = @[ImGuiTable()]
s.add ImGuiTable()
s.add ImGuiTable()
s.add ImGuiTable(field: 12)
echo s
