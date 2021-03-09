import hmisc/other/oswrap
export oswrap
import compiler/[nimeval]

let stdlib* = ~".choosenim/toolchains/nim-1.4.0/lib"

converter toSeqString*(paths: seq[AbsDir]): seq[string] =
  for path in paths:
    result.add path.getStr()
