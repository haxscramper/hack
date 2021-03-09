# Package

version       = "0.1.2"
author        = "haxscramper"
description   = "Colorful alternative to echo"
license       = "BSD-3-Clause"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["colecho"]



# Dependencies

requires "nim >= 0.20.2"
requires "hargparse >= 1.0.0"
requires "hmisc >= 0.1.0"

after test:
  exec "./tests/test.sh"
