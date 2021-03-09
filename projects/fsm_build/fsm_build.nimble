# Package

version       = "0.2.0"
author        = "haxscramper"
description   = "A new awesome nimble package"
license       = "BSD-3-Clause"
srcDir        = "src"
bin           = @["fsm_build"]



# Dependencies

requires "nim >= 0.20.2"
requires "colecho >= 0.1.0"
requires "create_script >= 1.0.0"
requires "hmisc >= 1.0.0"
requires "hargparse >= 0.1.0"

after install:
  exec "ln -sf ~/.nimble/bin/fsm_build ~/.nimble/bin/fsm-build"

after test:
  exec "cd tests && nim t*.nims && bash t*.sh"
