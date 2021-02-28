import tables, macros

var routes* {.compiletime.}: Table[string, NimNode]
