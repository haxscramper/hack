import sequtils
import sugar
import hashes
import hmisc/[defensive, helpers]
import macros
import options
import strutils, strformat
import tables

initDefense()

type
  Failure = ref object of CatchableError

type
  # BlockId = distinct int
  ClauseId = distinct int
  # EnvId = distinct int
  ValueType = string
  Timestamp = int
  Symbol = string


  TermKind = enum
    tkConstant
    tkVariable
    tkFunctor

  Term = object
    case kind: TermKind
      of tkConstant:
        value: ValueType

      of tkVariable:
        name: Symbol
        creation: Timestamp
        genIdx: int

      of tkFunctor:
        symbol: Symbol
        arguments: seq[Term]
        copied: bool

  Variable = Term

  Environment = object
    # id: EnvId
    values: Table[Term, Term]

  Block = object
    idx: int
    current: int
    alts: seq[(ClauseId, Environment)]

type
  Clause = object
    id: ClauseId
    head: Term
    body: seq[Term]

  Program = object
    clauses: seq[ClauseId]

func `==`(c1, c2: ClauseId): bool =
  int(c1) == int(c2)

func `<`(c1, c2: ClauseId): bool =
  int(c1) < int(c2)

type
  ClauseStore = object
    data: OrderedTable[ClauseId, Clause]

  # EnvStore = object
  #   data: OrderedTable[EnvId, Environment]

type
  WorkspaceT = object
    clauseStore: ClauseStore
    # blockStore: BlockStore
    # envStore: EnvStore

    # controlStack: seq[BlockId] ## Contains choice and deterministic blocks.
    copyStack: seq[Term] ## Stores compound terms
    # postboundStack: seq[Variable] ## List of variables that need to be
    #                               ## additionally reset on
    #                               ## backtracking.

    # contBlock: BlockId ## Continuation point local block
    # contPoint: ClauseId ## Next clause to be proved. Negative
    #                             ## if all necessary proofs has been
    #                             ## completed

  Workspace = var WorkspaceT

proc `[]`(w: Workspace, cl: ClauseId): var Clause =
  w.clauseStore.data[cl]


proc hash(t: Term): Hash =
  var h: Hash = 0
  h = h !& int(t.kind)
  case t.kind:
    of tkVariable:
      h = h !& hash(t.name) !& hash(t.genIdx) !& hash(t.creation)
    of tkConstant:
      h = h !& hash(t.value)
    of tkFunctor:
      h = h !& hash(t.copied)
      for arg in t.arguments:
        h = h !& hash(arg)

  result = !$h

proc registerClause(w: Workspace, cl: Clause): void =
  w.clauseStore.data[cl.id] = cl

proc getClause(w: Workspace, id: ClauseId): Clause =
  w.clauseStore.data[id]

proc getTime(): Timestamp =
  var counter {.global.}: int
  inc counter
  return counter

func hasAlts(bl: Block): bool =
  bl.current < bl.alts.len

template anyOfIt(s: typed, pr: untyped): bool =
  var res = false
  for it {.inject.} in s:
    if pr:
      res = true
      break

  res

type
  ControlStack = seq[Block]

func hasAlts(bl: ControlStack): bool =
  bl.anyOfIt(it.hasAlts)

func findAlt(bl: ControlStack): int =
  bl.findIt(it.hasAlts)

func getAlt(bl: ControlStack): (ClauseId, Environment) =
  let choice = bl[bl.findAlt()]
  return choice.alts[choice.current]

func getAlt(bl: Block): (ClauseId, Environment) =
  return bl.alts[bl.current]

func nextAlt(bl: var Block): void = inc bl.current
func getEnv(bl: Block): Environment = bl.getAlt()[1]

func isFact(cl: Clause): bool = cl.body.len == 0
func isVar(term: Term): bool = term.kind == tkVariable
func isConstant(term: Term): bool = term.kind == tkConstant
func isFunctor(term: Term): bool = term.kind == tkFunctor
func isGenvar(term: Term): bool = term.isVar and (term.genIdx > 0)
func isCopied(term: Term): bool = term.isFunctor and term.copied
func sameName(cl: Clause, term: Term): bool = cl.head.symbol == term.symbol

func arity(term: Term): int =
  assert term.isFunctor()
  return term.arguments.len

func sameTerm(t1, t2: Term): bool =
  if t1.kind != t2.kind:
    return false

  case t1.kind:
    of tkConstant:
      return t1.value == t2.value
    of tkVariable:
      return t1.name == t2.name
    of tkFunctor:
      if t1.symbol == t2.symbol and t1.arity() == t2.arity():
        for (arg1, arg2) in zip(t1.arguments, t2.arguments):
          if not sameTerm(arg1, arg2):
            return false

        return true
      else:
        return false


func `==`(t1, t2: Term): bool = sameTerm(t1, t2)


proc makeEnvironment(values: seq[(Term, Term)] = @[]): Environment =
  var envIdx {.global.}: int
  result = Environment(
    # # id: EnvId((inc envIdx; envIdx)),
    # values: values.toTable()
  )

  for (key, val) in values:
    result.values[key] = val

proc makeFunctor(functorName: string, args: seq[Term]): Term =
  Term(
    kind: tkFunctor,
    symbol: functorName,
    arguments: args
  )

proc makeClause(head: Term, body: seq[Term]): Clause =
  assert head.isFunctor()
  for goal in body:
    assert goal.isFunctor()

  var idx {.global.}: int
  inc idx
  return Clause(
    id: ClauseId(idx),
    head: head,
    body: body
  )

proc makeStoreClause(head: Term, body: seq[Term], w: Workspace): ClauseId =
  let cl = makeClause(head, body)
  w.registerClause(cl)
  return cl.id

iterator iterateClauses(w: Workspace): ClauseId =
  for id, cl in w.clauseStore.data:
    yield id

proc makeVariable(varName: string): Term =
  Term(
    kind: tkVariable,
    name: varName,
    creation: getTime()
  )


proc makeFreeVar(): Term =
  var freeVarInst {.global.}: Term
  freeVarInst = makeVariable("#free#")
  freeVarInst

proc makeChoice(
  idx: int,
  alts: seq[(ClauseId, Environment)]): Block =
  Block(
    idx: idx,
    alts: alts,
    current: 0
  )

proc makeConstant(constValue: ValueType): Term =
  Term(
    kind: tkConstant,
    value: constValue
  )





iterator pairs(env: Environment): (Term, Term) =
  for key, val in env.values:
    yield (key, val)

iterator items(env: Environment): (Term, Term) =
  for key, val in env.values:
    yield (key, val)

func contains(env: Environment, variable: Variable): bool =
  for v, val in env:
    if sameTerm(v, variable):
      result = true

proc getVarList(term: Term): seq[Variable] =
  case term.kind:
    of tkConstant:
      return @[]
    of tkVariable:
      return @[term]
    of tkFunctor:
      concat(term.arguments.map(getVarList))


iterator iterateVars(term: Term): Variable =
  for v in term.getVarList():
    yield v

iterator iterateVars(cl: Clause): Variable =
  for arg in cl.head.iterateVars():
    yield arg

  for goal in cl.body:
    for v in goal.iterateVars():
      yield v

func getValue(env: Environment, term: Variable): Option[Term] =
  for key, val in env.values:
    if sameTerm(key, term):
      return some(val)

  return none(Term)

func `[]`(env: Environment, term: Variable): Term =
  env.values[term]

proc isBound(term: Variable, env: Environment): bool =
  let val = env.getValue(term)
  return (val.isSome() and # Present in environment
    not (
      sameTerm(val.get(), makeFreeVar()) or # Not explicitly bound to free var
      sameTerm(val.get(), term) or # Not bound to itself
      val.get().isVar() # NOTE ???
    ))


proc `$`(term: Term): string =
  case term.kind:
    of tkConstant:
      return term.value
    of tkVariable:
      return "_" & term.name & "'".repeat(term.genIdx)
    of tkFunctor:
      return term.symbol & "(" & term.arguments.mapIt($it).join(", ") & ")"

proc `$`(env: Environment): string =
  "{" & env.mapIt(
    &"({it[0]} " & (
      (it[0].isVar() and it[1].isVar()).tern("<->", "->")
    ) & &" {it[1]})"
  ).join(" ") & "}"

proc `$`(cl: Clause): string =
  if cl.isFact():
    $cl.head & "."
  else:
    $cl.head & " :- " & cl.body.mapIt($it).join(", ") & "."



proc push(
  self: var Environment, variable: Variable,
  value: Term, noOverride: bool = true): void =
  if noOverride:
    assert not isBound(variable, self),
       &"""Attempt to rebind already existing var '{variable}' to '{value}'
  (current binding is '{self.getValue(variable).get()}')"""
  else:
    discard
    # TODO drop values
    # if variable.isBound(self):
    #   self.values.delete(
    #     findIt(sameTerm(it[0], variable))
    #   )

  self.values[variable] = value

proc pushEnv(bl: var Block, v: Variable, val: Term, noOverride = true): void =
  bl.alts[bl.current][1].push(v, val, noOverride)

proc genVar(term: Variable): Variable =
  result = term
  inc result.genIdx

proc dereference(term: Term, env: Environment): Term =
  ## Traverse binding chain in environment `env` and return value of
  ## the `term`
  result = term

  while result.isBound(env):
    let value = env.getValue(result).get()
    if value.isConstant() or sameTerm(value, result):
      result = value
      break

    result = value

proc bindTerm(variable, value: Term, env: Environment): Environment

proc copy(term: Term, env: Environment): (Term, Environment) =
  let inputEnv = env
  case term.kind:
    of tkConstant:
      return (term, inputEnv)
    of tkVariable:
      let deref = term.dereference(env)
      if deref.isVar():
        let newVar = term.genVar()
        var resEnv = bindTerm(deref, newVar, env)
        return (newVar, resEnv)
      else:
        return (deref, inputEnv)

    of tkFunctor:
      var resEnv = env
      var resFunctor = makeFunctor(term.symbol, @[])
      for arg in term.arguments:
        let (tmpArg, tmpEnv) = arg.copy(resEnv)
        resEnv = tmpEnv
        resFunctor.arguments.add tmpArg

      assert resFunctor.arity() == term.arity()
      return (resFunctor, resEnv)

proc copy(cl: Clause): (ClauseId, Environment) =
  var resEnv: Environment
  for v in cl.iterateVars():
    if v notin resEnv:
      resEnv.push(v, makeFreeVar())

  return (cl.id, resEnv)


proc bindTerm(variable, value: Term, env: Environment): Environment =
  ## Create environment where `variable` is bound to `value`
  result = env
  case value.kind:
    of tkConstant, tkVariable:
      result.push(variable, value)
    of tkFunctor:
      if value.isCopied:
        result.push(variable, value)
      else:
        let (newTerm, newEnv) = value.copy(env)
        result = newEnv
        result.push(variable, newTerm)


converter toVariable(term: Term): Variable =
  assert term.kind == tkVariable
  return term



proc equalConstants(t1, t2: Term): bool =
  (t1.kind == tkConstant) and (t2.kind == tkConstant) and (t1.value == t2.value)

proc unif(t1, t2: Term, env: Environment = makeEnvironment()): Environment =
  ## If possible, return new environment where two terms `t1`, `t2` are equal
  let
    val1 = dereference(t1, env)
    val2 = dereference(t2, env)

  if val1.isConstant() and val2.isConstant():
    if equalConstants(val1, val2):
      return env
    else:
      raise Failure(msg: "Unification failed: different constants")
  elif val1.isVar():
    return bindTerm(val1, val2, env)
  elif val2.isVar():
    return bindTerm(val2, val1, env)
  else:
    result = env
    if val1.symbol != val2.symbol:
      raise Failure(
        msg: &"Cannot unify functors with different names '{t1}' and '{t2}'")

    for (arg1, arg2) in zip(val1.arguments, val2.arguments):
      result = unif(arg1, arg2, result)

iterator getUnified(w: Workspace, term: Term, env: Environment): (ClauseId, Environment) =
  ## Iterate over all clauses in workspace `w` that can be unified
  ## with term `t` under existing environment `env`.
  for cl in w.iterateClauses():
    if w[cl].sameName(term):
      try:
        let resEnv = unif(term, w[cl].head, env)
        yield (cl, resEnv)
      except Failure:
        showWarn "Clause", w[cl], ": ", getCurrentExceptionMsg()
        discard

template last[T](s: seq[T]): T = s[^1]

var tmp: int = 0

proc `$`(id: ClauseId): string = $int(id)

proc logBlock(bl: Block): void =
  for alt in bl.alts:
    showLog &"id: {alt[0]}, env: {alt[1]}"

proc revert(call, head: Term, env: Environment): Environment =
  let (cpTerm, cpEnv) = head.copy(env)
  let resEnv = unif(call, cpTerm, cpEnv)

  let exported = collect(newSeq):
    for v in call.iterateVars():
      (v, resEnv[v])

  return makeEnvironment(exported)


proc solve(w: Workspace, query: Term, topEnv: Environment): Option[Environment] =
  var control: ControlStack = @[makeChoice(idx = -1, alts = toSeq(w.getUnified(query, topEnv)))]
  showInfo "Solving ", query, "in env", topEnv
  while control.hasAlts():
    let topId = control[0].getAlt()[0]
    inc tmp
    if tmp > 8:
      quit 1

    # Iterate over all clauses that match toplevel query
    let (cl, env) = control.getAlt()
    let subMax = w[topId].body.len - 1
    if w[cl].isFact():
      showLog "Found fact with env", env
      return some(env)
    else:
      let subgoals = w[cl].body
      var idx = control.len - 1
      while idx < subgoals.len:
        let subgoal = subgoals[idx]
        let nowEnv = control.last.getEnv()

        showLog &"cl: {w[cl]}, env: {nowEnv}"
        control.add @[makeChoice(idx = idx, alts = toSeq(w.getUnified(subgoal, nowEnv)))]
        runIndentedLog:
          let resEnv = w.solve(subgoal, nowEnv)

        if resEnv.isSome():
          let revEnv = revert(subgoal, w[control.last.getAlt()[0]].head, resEnv.get())
          showLog "We can retrieve value", revEnv
          for v, val in revEnv:
            control.last.pushEnv(v, val, false)

          if idx == subMax:
            showInfo "Final goal in clause, returning result"
            return some(control.last.getEnv())
          else:
            inc idx
        else:
          showInfo "Subgoal solution failed"
          runIndentedLog:
            if not control.last.hasAlts():
              showLog "Last block has no more alternative solutions"
              # Remove choice point from the top of the stack: we tried everything
              discard control.pop
              # Advance choice point under current one
              control.last.nextAlt()
              # Break the loop - it is not possible to solve current
              # subgoal under this condition, we need to try earlier
              decreaseLogIndent()
              break
            else:
              showLog "Last block has alternative solutions"
              control.last.nextAlt()
              # discard control.pop
              # nowEnv = control.last.env


proc mf(s: string, a: varargs[Term]): Term = makeFunctor(s, toSeq(a))
proc mv(n: string): Term = makeVariable(n)
proc mc(s: string): Term = makeConstant(s)

proc main() =
  if false:
    echo unif(
      t1 = "t".mf(mv("X"), "f".mf(mv("U"), mv("Z"))),
      t2 = "t".mf(mc("12"), "f".mf(mc("--"), mv("I"))),
      env = makeEnvironment()
    )

    echo unif(
      t1 = "e".mf(mv "X", mv "Y"),
      t2 = "e".mf(mv "X", mv "X"),
      env = makeEnvironment()
    )

    echo dereference(mv("X"), makeEnvironment(
      values = @[(mv("X"), mc("*&&*"))]
    ))


    echo dereference(mv("X"), makeEnvironment(
      values = @[
        (mv("X"), mv("Y")),
        (mv("Y"), mc("*&&*"))]
    ))


  if true:
    echo copy("e".mf(mv "X", mv "Y"), makeEnvironment(@[(mv "X", mc "2")]))

    echo unif("e".mf(mc "2", mc "2"), "e".mf(mc "2", mv "U"))
    echo unif("e".mf(mv "X", mv "Y"), "e".mf(mc "2", mv "U"))
    echo unif("e".mf(mc "2", mv "U"), "e".mf(mv "X", mv "Y"))
    proc revertTest(t1, t2: Term, env: Environment): void =
      showInfo "Revert"
      runIndentedLog:
        showLog "Called as_________:", t1
        showLog "Unification head__: ", t2
        showLog "Clause res env____: ", env
        let rev = revert(t1, t2, env)
        showLog "Retrieved values__: ", rev

    revertTest(
      "e".mf(mv "X", mv "Y"),
      "e".mf(mv "I", mv "I"),
      makeEnvironment(@[(mv "I", mc"2")])
    )

    revertTest(
      "e".mf(mv "X", mv "Y"),
      "e".mf(mv "I", "g".mf(mv "I")),
      makeEnvironment(@[(mv "I", mc"2")])
    )

    revertTest(
      "e".mf(mv "X", "f".mf(mv "Y")),
      "e".mf(mv "I", mv "Z"),
      makeEnvironment(@[(mv "I", mc "2"), (mv "Z", mc "*")])
    )


    var w: WorkspaceT
    let prog = Program(
      clauses: @[
        # makeStoreClause("p".mf(mv "X"), @["q".mf(mv "X", mv "Y"), "r".mf(mv "Y")], w),
        # makeStoreClause("q".mf(mc "a", mc "b"), @[], w),
        # makeStoreClause("q".mf(mv "Z", mc "c"), @[], w),
        # makeStoreClause("r".mf(mc "c"), @[], w)
        makeStoreClause(
          "c".mf(mv "X", mv "Y"),
          @["c1".mf(mv "Y")# , "c2".mf(mv "Y")
          ], w
        ),
        makeStoreClause("c1".mf(mv "Z"), @["e".mf(mv "Z", mc "2")], w),
        makeStoreClause("c1".mf(mv "Z"), @["e".mf(mv "Z", mc "3")], w),
        makeStoreClause("c2".mf(mv "Z"), @["e".mf(mv "Z", mc "5")], w),
        makeStoreClause("c2".mf(mv "Z"), @["e".mf(mv "Z", mc "6")], w),
        makeStoreClause("e".mf(mv "I", mv "I"), @[], w)
      ]
    )

    # let query = mf("e", @[mc "2", mv "U"])
    # let query = mf("c", @[mc "2", mv "U"])
    # let res = w.solve(query, makeEnvironment())
    # echo res

pprintErr:
  main()
