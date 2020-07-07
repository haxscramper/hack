proc split(a, b: string): seq[string] = discard
iterator split(a, b: string): string = discard

static:
  echo typeof(split("A", "B")) # < Should've printed `seq[string]`

assert split("A", "A").typeof(typeOfProc) is seq[string]
#      ^^^^^^^^^^^^^^^                       ^^^^^^^^^^^
#      |                                     | Return type for `proc`
#      |
#      | Should call `proc split(string, string): seq[string]`
#        but instead calls `iterator`
