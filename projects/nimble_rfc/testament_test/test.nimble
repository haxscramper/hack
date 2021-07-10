version       = "0.11.16"
author        = "haxscramper"
description   = "Collection of helper utilities"
license       = "Apache-2.0"
srcDir        = "src"
packageName   = "test"


requires "hmisc >= 0.11.35"

task test, "run tests":
  exec("testament --print run test0.nim")