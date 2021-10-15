# https://stackoverflow.com/questions/69455064/iterate-over-multiple-tuples-in-nim

import std/[macros, typetraits, sequtils]

macro tuplePairsAux(
    lens: static[seq[int]],
    body: untyped,
    index: static[string],
    injectsOf: static[seq[string]],
    tuples: varargs[untyped]): untyped =

  let len0 = lens[0]
  if anyIt(lens, it != len0):
    error("'tuplePairs' requires to use tuples of equal lenght", tuples[0])

  if injectsOf.len != tuples.len:
    error("'tuplePairs' requires " & $tuples.len &
      " loop variables to be specified, but got " & $injectsOf.len, tuples[0])

  result = newStmtList()
  for item in 0 ..< len0:
    var declare = newStmtList()
    declare.add nnkConstSection.newTree(
      nnkConstDef.newTree(ident(index), newEmptyNode(), newLit(item)))

    for tup in 0 ..< len(tuples):
      let name = ident(injectsOf[tup])
      let inTup = tuples[tup]
      let itemIdx = newLit(item)
      declare.add quote do:
        let `name` = `inTup`[`itemIdx`]

    result.add nnkBlockStmt.newTree(
      newEmptyNode(), newStmtList(declare, body))

  echo result.repr()

macro tuplePairs*(x: ForLoopStmt): untyped =
  var lens = nnkBracket.newTree()

  for tup in x[^2][1..^1]:
    lens.add newCall(bindSym"tupleLen", tup)

  var call = newCall(
    bindSym"tuplePairsAux",
    nnkPrefix.newTree(ident"@", lens))

  call.add x[^1] # Pass body to aux call
  call.add newLit(x[0].strVal()) # Pass index variable to the list

  var injects = nnkBracket.newTree()
  for inj in x[1 ..^ 3]:
    injects.add newLit(inj.strVal())

  call.add nnkPrefix.newTree(ident"@", injects) # Pass names of the injected variables

  result = newStmtList()

  # Pass all argument tuples
  for tup in x[^2][1 ..^ 1]:
    # If passed tuple is an identifier it can be used directly
    if tup.kind in {nnkIdent, nnkSym}:
      call.add tup

    else:
      # Otherwise generate temporary variable in order to avoid multiple
      # evaluation of the expression
      let gen = genSym(nskLet)
      result.add nnkLetSection.newTree(
        nnkIdentDefs.newTree(gen, newEmptyNode(), tup))

      call.add gen

  result.add call
  result = nnkBlockStmt.newTree(newEmptyNode(), result)
  echo result.repr()


proc `+`(a: (int, int), b: (int, int)): (int, int, int) =
  for idx, t1, t2, t3 in tuplePairs(a, b, (1, 2)):
    result[idx] = t1 + t2 + t3
