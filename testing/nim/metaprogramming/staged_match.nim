import std/[macros, macrocache]

const
  cacheTable = CacheTable("table")
  id = "222222222222"

macro matchWithKind(head, headKind: typed, id: static[string]) =
  let arms = cacheTable[id]
  newCall("echo", newLit("matched with kind, total of "), newLit(arms.len), newLit(" arms"))

macro matchNoKind(head: typed, id: static[string]) =
  newCall("echo", newLit("matched without kind"))

macro match(head: typed, arms: varargs[untyped]) =
  cacheTable[id] = newStmtList(arms)
  let idLit = newLit(id)

  result = quote do:
    when compiles(`head`.kind):
      matchWithKind(`head`, `head`.kind, `idLit`)

    else:
      matchNoKind(`head`, `idLit`)




macro user(head: untyped) =
  match head:
  of @lhs + @rhs:
    echo lhs.repr
    echo rhs.repr


user(1 + 2)
