import std/os
proc mcleanup() {.dynlib: "libc.so.6", importc: "_mcleanup", cdecl.}
putenv("GMON_OUT_PREFIX", "/tmp/gmon")

echo getCurrentProcessId()

mcleanup()
