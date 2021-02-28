import os
{.push header: currentSourcePath().splitPath().head / "cxtype.hpp".}

type S* {.importcpp: "S".} = object

proc hello*(): S {.importcpp: "hello".}
