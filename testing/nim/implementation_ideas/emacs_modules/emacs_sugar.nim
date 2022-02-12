import ./emacs_api
import std/[strutils, macros]

var env: EmEnv

proc `as`*[T](arg: EmValue, target: typedesc[T]): T =
  fromEmacs[T](env, result, arg)

proc setEnv*(inEnv: EmEnv) =
  echo "set environment"
  env = inEnv

proc getEnv*(): EmEnv = env


iterator items*(env: EmEnv, value: EmValue): EmValue =
  case env.getTypeName(value):
    of "cons":
      for item in env.consItems(value):
        yield item

    of "vector":
      for item in env.vecItems(value):
        yield item

    else:
      assert false, "$# cannot be iterated with `items`" % env.getTypeName(value)
