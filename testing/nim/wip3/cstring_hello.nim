proc something(a: string): string {.exportc.} =
  a & "test"

echo something("str")
