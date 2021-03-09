# Package

version       = "1.4.0"
author        = "haxscramper"
description   = "Personal collection of simple utilities"
license       = "BSD-3-Clause"
srcDir        = "src"
bin           = @["get_daily_note", "format_comment"]



# Dependencies

requires "nim >= 0.20.2"
requires "hargparse >= 0.1.0"
requires "colecho"

after install:
  exec "ln -sf ~/.nimble/bin/get_daily_note ~/.nimble/bin/get-daily-note"

