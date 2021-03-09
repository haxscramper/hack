if paramStr(paramCount()) == "test8.nim":
  exec("nim r generate_semhack.nim")
  switch("path", "/home/test/.nimble/pkgs/compiler-1.4.0/compiler")
  # switch("d", "useNodeIds")
  # patchFile("compiler", "sem", "semhack")
