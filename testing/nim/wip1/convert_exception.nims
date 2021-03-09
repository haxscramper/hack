#!/usr/bin/env nim

# NOTE does not work

type
  Ex = ref object of CatchableError
    show: proc(e: Ex)

let err: CatchableError = CatchableError()
let e: Ex = Ex(err)

try:
  raise Ex(show: proc(e: Ex) = echo "hello")
except Ex:
  # getCurrentException()
  echo "exception"
