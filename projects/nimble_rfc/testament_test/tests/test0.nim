discard """
  cmd:      "nim c -r --styleCheck:hint --panics:on $options $file"
  matrix:   "--gc:arc; --gc:arc --d:release"
  targets:  "c"
  nimout:   ""
  action:   "run"
  exitcode: 0
  timeout:  60.0
"""

import std/compilesettings


var str = ""

str.add $querySettingSeq(searchPaths)
str.add $querySettingSeq(lazyPaths)
str.add $querySettingSeq(nimblePaths)

"/tmp/azz".writeFile(str)
