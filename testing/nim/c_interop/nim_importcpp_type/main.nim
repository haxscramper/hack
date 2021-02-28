type
  Exported* {.header: "clib.hpp", importcpp: "Exported".} = object

proc acceptsExported(arg: Exported): void {.exportcpp.} =
  echo arg.val1
