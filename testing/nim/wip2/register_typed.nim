import std/macros

func headSym(n: NimNode): NimNode =
  n[0]

macro globalRouter(): untyped =
  let
    router = genSym(nskVar, "router")
    reg = ident("reg")
    finish = ident("finishRouter")

  result = newStmtList()

  result.add quote do:
    var `router` {.compileTime.}: seq[NimNode]

    macro `reg`(pr: typed): untyped =
      `router`.add pr.headSym()
      result = pr

    macro `finish`() =
      for sym in `router`:
        echo sym.lispRepr()

globalRouter()

proc route1(a: string) {.reg.} = discard
proc route2(a: string) {.reg.} = discard
proc route1(a: string, q: float) {.reg.} = discard

finishRouter()


# Seems like you can't push custom pragma after all, or at least no in this confguration
# But it creates a local compiletime list of routed procs, allows the user to register it, and then provides a way to finalize implementation.
# So you can init router from other module, and then 

# proc initForModule(globalRouter: var Router) = 
#   finishRouter(globalRouter) # Register all local procs

# Module only exposes initForModule proc that fills in available routing procs
