# Package

version       = "1.5.0"
author        = "haxscramper"
description   = "Simple filename pare utility"
license       = "BSD-3-Clause"
srcDir        = "src"
bin           = @["parse_path"]



# Dependencies

requires "nim >= 0.20.2"
requires "hargparse >= 0.1.0"
requires "colecho >= 1.0.0"


after install:
  exec "ln -sf ~/.nimble/bin/parse_path ~/.nimble/bin/parse-path"
