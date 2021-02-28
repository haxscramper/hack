import os
{.push header: currentSourcePath().splitPath().head / "cxtype.hpp".}

type S* {.importcpp: "S".} = object

proc world*(s: S) {.importcpp: "world(@)".}
