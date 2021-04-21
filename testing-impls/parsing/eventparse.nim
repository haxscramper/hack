import std/[strformat, strutils, sequtils, tables]
import hmisc/[hdebug_misc, base_errors]
import hmisc/types/colorstring
import hmisc/algo/halgorithm

startHax()

type
  HandlerKind = enum
    hkPush
    hkWrap
    hkTear
    hkPop
    hkPopMany
    hkPopParallel
    hkNext
    hkNextMany
    hkAwait
    hkAwaitMany
    hkTrim
    hkLift
    hkDrop

    hkWhereGroup
    hkCloseAll

  Handler[Ast, AstKind, Event, EventKind, Flag] = ref object
    ## - @param{Ast} :: Ast type for stack
    ## - @param{AstKind} :: Stack content entry kind
    ## - @param{Event} :: Trigger input event type
    ## - @param{EventKind} :: Input event kind
    ## - @param{Flag} :: Flag stack content

    pendingTargetSet: bool ## Missing pop/await target
    case kind: HandlerKind
      of hkAwait, hkAwaitMany, hkNext, hkNextMany:
        waitKind: AstKind ## Filter element kinds
        pauseOn: set[Flag] ## Pause waiter when particular set of flags is
                           ## at the top
        waitTarget: ptr seq[Ast] ## Sequence to add elements to
        found: bool ## Waiter finished execution
        onComplete: Handler[Ast, AstKind, Event, EventKind, Flag] ## |
        ## Parent handler to execute if all elements have finished completion

      of hkPopParallel:
        ## Remove multiple elements from AST stack assigning to different
        ## targets
        popParallelTargets: seq[(AstKind, ptr seq[Ast])]

      of hkPopMany, hkPop:
        ## Remove elements from AST stack
        popKind: set[AstKind]
        popTarget: ptr seq[Ast]

      of hkLift, hkDrop:
        ## Add or remove flag from the flag stack
        flag: Flag

      of hkWhereGroup:
        ## Toplevel element grouping
        case guarded: bool ## Only trigger when particular flag is active
          of true:
            guardFlag: Flag

          else:
            discard

        startActions: seq[Handler[Ast, AstKind, Event, EventKind, Flag]] ## Actions
        ## executed immediately when group starts
        finishActions: seq[Handler[Ast, AstKind, Event, EventKind, Flag]] ## Actions
        ## executed after groups `where` completes
        whereBody: seq[Handler[Ast, AstKind, Event, EventKind, Flag]]

      of hkPush:
        ## Construct new AST and add it to stack using `argList` as
        ## subnodes
        newAstKind: AstKind
        argList: seq[Ast]

      of hkTrim:
        ## `trim` is the action to stop any other ongoing actions.
        trimKind: HandlerKind ## Action kind to trim
        trimTargetKind: AstKind ## Kind of the *action target*

      else:
        discard

  ExecContext[Ast, AstKind, Event, EventKind, Flag] = object
    ## Mutable context of parser execution
    stack: seq[tuple[ast: Ast, level: uint8]]
    waiters: array[
      AstKind,
      seq[Handler[Ast, AstKind, Event, EventKind, Flag]]]

    flags: seq[Flag]
    newAst: proc(kind: AstKind, subnodes: seq[Ast]): Ast ## |
    ## Callback to create new ast instance.
    finishEvent: Event ## Support event kind sent to `where` handler group by
    ## terminated action and to `await` event on termination.

proc isFinished[A, Ak, E, Ek, F](
    handler: Handler[A, Ak, E, Ek, F]): bool =
  ## Check whether handler has found required element
  case handler.kind:
    of hkAwait, hkAwaitMany, hkNext, hkNextMany:
      # Pending action must marked as `found`
      return handler.found

    of hkWhereGroup:
      # For where group all elements must be completed
      return allIt(handler.whereBody, isFinished(it))

    else:
      # Other elements do not have explicit notion of 'finished'
      # state and must be checked with 'isImmediate'
      raiseLogicError(
        &"Cannot check for finish in {handler.kind}")


proc isImmediate*[A, Ak, E, Ek, F](
    handler: Handler[A, Ak, E, Ek, F]): bool =
  ## Whether handler can be immediately executed
  case handler.kind:
    of hkWhereGroup:
      # All parts of the where group must be immediately
      # executable
      return allIt(handler.whereBody, isImmediate(it))

    of hkAwaitMany, hkAwait:
      false

    of hkPop, hkPopMany, hkPopParallel:
      true

    else:
      # TODO
      raiseImplementKindError(handler)

proc isWaiter*[A, Ak, E, Ek, F](
    handler: Handler[A, Ak, E, Ek, F]): bool =
  handler.kind in {hkAwait, hkAwaitMany, hkNext, hkNextMany}

proc canExecute*[A, Ak, E, Ek, F](
    handler: Handler[A, Ak, E, Ek, F]): bool =
  ## Whether action is immediately executable or it's waiting
  ## target has been finished.
  isImmediate(handler) or isFinished(handler)


proc lift[A, Ak, E, Ek, F](
    context: var ExecContext[A, Ak, E, Ek, F], flag: F) =
  ## Add context flag
  # This function implementation directly influences how parser works. Current
  # implementation maintains explicit stack of tags that makes it work like LR
  # automaton - switching is done based on the current stack head and input
  # token.
  #
  # There are other possible implementations for a flag handling, such as
  # unordered multiset that keeps track of *number of times* each flag has been
  # raised.
  context.flags.add flag

proc drop[A, Ak, E, Ek, F](
    context: var ExecContext[A, Ak, E, Ek, F], flag: F) =
  ## Drop flag
  # Stack-based implementation does not allow to remove any arbitrary flag from
  # lifted, and instead can only pop topmost entry.
  let val = context.flags.pop
  assert val == flag, &"{val} == {flag}"



proc nextColor[E](used: var set[E]): E =
  const all = { low(E) .. high(E) }
  for item in (all - used):
    result = item
    break

  used.incl result


proc execOn[A, Ak, E, Ek, F](
  handler: Handler[A, Ak, E, Ek, F], event: E,
  context: var ExecContext[A, Ak, E, Ek, F])

proc getName(kind: HandlerKind): string =
  const map = toMapArray({
    hkPush: "push",
    hkPop: "pop",
    hkPopMany: "pop*",
    hkPopParallel: "pop#",
    hkAwait: "await",
    hkAwaitMany: "await*",
    hkNext: "next",
    hkNextMany: "next*",
    hkLift: "lift",
    hkDrop: "drop",
    hkWhereGroup: "group"
  })

  return map[kind]

proc `$`*[A, Ak, E, Ek, F](
    handler: Handler[A, Ak, E, Ek, F]): string =
  ## String representation for `$`.
  ##
  ## - NOTE :: All generic parameters must also be string-convertible using `$` operator
  proc aux(h: Handler[A, Ak, E, Ek, F], level: int):
    seq[string] =

    let pref = "    ".repeat(level)
    case h.kind:
      of hkWhereGroup:
        result.add &"{pref}group"
        for act in h.startActions: result.add aux(act, level + 1)
        for act in h.finishActions: result.add aux(act, level + 1)

        if h.whereBody.len > 0:
          result.add pref & "    " & "where"
          for act in h.whereBody:
            result.add aux(act, level + 2)

      of hkPush:
        result.add &"{pref}push {h.newAstKind}(.. {h.argList.len})"

      of hkTrim:
        result.add &"{pref}trim {h.trimKind}/{h.trimTargetKind}"

      of hkAwait, hkAwaitMany, hkNext, hkNextMany:
        result.add &"{pref}{getName(h.kind)} {h.waitKind}"

      of hkLift, hkDrop:
        result.add &"{pref}{getName(h.kind)} {h.flag}"

      of hkPop, hkPopMany:
        result.add &"{pref}{getName(h.kind)} {h.popKind}"

      of hkPopParallel:
        for (kind, target) in h.popParallelTargets:
          result.add &"{pref}pop# {kind}"

      else:
        raiseImplementError("")

  return aux(handler, 0).join("\n")


proc putAst[A](target: var seq[A], ast: A) =
  target.add ast

proc putAst[A, Ak, E, Ek, F](
    context: var ExecContext[A, Ak, E, Ek, F], ast: A) =
  ## Execute stack addition. If no pending actions match for this event `ast` is
  ## added to context stack, otherwise it is appended to waiter target list.
  var found = false
  for waiter in mitems(context.waiters[ast.kind]):
    # TODO - only await(*) actions are currently implemented - next(*) is not
    # done yet
    if waiter.kind in {hkAwaitMany, hkAwait} and
       # If waiter matches expected AST
       waiter.waitKind == ast.kind and
       # And it is not set to pause on curren't context top flag
       (context.flags.len > 0 and context.flags[^1] notin waiter.pauseOn):

      waiter.waitTarget[].putAst ast # Add ast to target list
      waiter.found = true # Mark waiter as `found` (for single-item await)
      found = true
      # TODO execute trimming action for single-target await immediately when
      # found
      break

  if not found:
    context.stack.add (ast, context.flags.len.uint8)




proc execOn[A, Ak, E, Ek, F](
    handler: Handler[A, Ak, E, Ek, F], event: E,
    context: var ExecContext[A, Ak, E, Ek, F]) =
  case handler.kind:
    of hkWhereGroup:
      # There are two cases - `where` group must first be assigned to
      # corresponding waiters and then collected (group uses
      # `await`/`next`), *or* it has no forward waiters and can be executed
      # immediately
      if event.kind == context.finishEvent.kind:
        if handler.whereBody.allIt(isImmediate(it) or isFinished(it)):
          # Finishing single waiter from `where` clause does not necessarily
          # mean whole group can be executed.
          for action in handler.finishActions:
            action.execOn(event, context)

      else:
        for action in handler.startActions:
          # Start actions are always executed when encountered (mostly used to
          # raise flags)
          action.execOn(event, context)

        if canExecute(handler):
          # If handler can be immediately executed do it without adding trailing
          # actions to the waiter list.
          for action in handler.whereBody:
            action.execOn(event, context)

          for action in handler.finishActions:
            action.execOn(event, context)

        else:
          for action in mitems(handler.whereBody):
            # Add all waiter actions to the list.
            #
            # - TODO :: if `where` clause contains both `await*` and `pop#`
            #   actions - how should they be handled.
            if action.isWaiter():
              action.onComplete = handler
              context.waiters[action.waitKind].add action

    of hkLift:
      context.lift handler.flag

    of hkDrop:
      context.drop handler.flag

    of hkPopMany:
      if context.stack.len > 0:
        let level = context.stack[^1].level
        while len(context.stack) > 0 and
              level > context.flags.len.uint8 and
              context.stack[^1].ast.kind in handler.popKind:
          handler.popTarget[].putAst context.stack.pop().ast

    of hkPush:
      context.putAst context.newAst(
        handler.newAstKind, handler.argList)

      handler.argList = @[]

    of hkCloseAll:
      for group in mitems(context.waiters):
        for waiter in mitems(group):
          waiter.execOn(event, context)

    of hkAwaitMany:
      if event.kind == context.finishEvent.kind:
        handler.onComplete.execOn(event, context)

    of hkTrim:
      # Trim any ongoing waiter action. Used to explicitly closing pending
      # `await*` handlers
      var target = -1
      let kind = handler.trimTargetKind
      for idx, waiter in context.waiters[kind]:
        if waiter.kind == handler.trimKind:
          target = idx
          break

      if target == -1:
        raiseLogicError(
          &"Cannot trim {handler.trimKind}/{handler.trimTargetKind} - no ongoing action")

      # Trigger parent `where` group using provided finish event from context
      context.waiters[kind][target].onComplete.execOn(
        context.finishEvent, context)

      context.waiters[kind].delete(target)

    else:
      raiseImplementKindError(handler)




func updateTargets[A, Ak, E, Ek, F](handler: var Handler[A, Ak, E, Ek, F]) =
  var target: ptr seq[A]
  for action in handler.finishActions:
    if action.kind in {hkPush}:
      target = addr action.argList

  for handler in items(handler.whereBody):
    if handler.pendingTargetSet:
      case handler.kind:
        of hkPop, hkPopMany:
          handler.popTarget = target

        of hkAwait, hkAwaitMany, hkNext, hkNextMany:
          handler.waitTarget = target

        else:
          discard



func group[A, Ak, E, Ek, F](
    startActions: openarray[Handler[A, Ak, E, Ek, F]],
    finishActions: openarray[Handler[A, Ak, E, Ek, F]] = @[],
    whereBody: openarray[Handler[A, Ak, E, Ek, F]] = @[]
  ): Handler[A, Ak, E, Ek, F] =

  result = Handler[A, Ak, E, Ek, F](
    kind:          hkWhereGroup,
    startActions:  toSeq(startActions),
    finishActions: toSeq(finishActions),
    whereBody:     toSeq(whereBody)
  )

  updateTargets(result)


func group[A, Ak, E, Ek, F](
    flag: F,
    startActions:  openarray[Handler[A, Ak, E, Ek, F]],
    finishActions: openarray[Handler[A, Ak, E, Ek, F]] = @[],
    whereBody:     openarray[Handler[A, Ak, E, Ek, F]] = @[]
  ): Handler[A, Ak, E, Ek, F] =

  result = Handler[A, Ak, E, Ek, F](
    guarded:       true,
    guardFlag:     flag,
    kind:          hkWhereGroup,
    startActions:  toSeq(startActions),
    finishActions: toSeq(finishActions),
    whereBody:     toSeq(whereBody)
  )

  updateTargets(result)

func drop[A, Ak, E, Ek, F](H: typedesc[Handler[A, Ak, E, Ek, F]], flag: F): H =
  H(kind: hkDrop, flag: flag)


func lift[A, Ak, E, Ek, F](H: typedesc[Handler[A, Ak, E, Ek, F]], flag: F): H =
  H(kind: hkLift, flag: flag)

func trim[A, Ak, E, Ek, F](
  H: typedesc[Handler[A, Ak, E, Ek, F]], target: H): H =
  H(kind: hkTrim, trimTargetKind: target.waitTarget, trimKind: target.kind)

func push[A, Ak, E, Ek, F](
  H: typedesc[Handler[A, Ak, E, Ek, F]], target: Ak): H =
  H(kind: hkPush, newAstKind: target)


func trim[A, Ak, E, Ek, F](
  H: typedesc[Handler[A, Ak, E, Ek, F]], target: Ak, targetKind: HandlerKind): H =
  H(kind: hkTrim, trimTargetKind: target, trimKind: targetKind)


func closeALl[A, Ak, E, Ek, F](H: typedesc[Handler[A, Ak, E, Ek, F]]): H =
  H(kind: hkCloseALl)

func popMany[A, Ak, E, Ek, F](
    H: typedesc[Handler[A, Ak, E, Ek, F]],
    popKinds: set[Ak], popTarget: var H
  ): H =

  H(kind: hkPopMany, popKind: popKinds, popTarget: addr popTarget.argList)


func popMany[A, Ak, E, Ek, F](
    H: typedesc[Handler[A, Ak, E, Ek, F]], popKinds: set[Ak]): H =
  H(kind: hkPopMany, popKind: popKinds, pendingTargetSet: true)


func waitMany[A, Ak, E, Ek, F](
    H: typedesc[Handler[A, Ak, E, Ek, F]],
    waitKind: Ak, waitTarget: var H, pauseOn: set[F] = {}
  ): H =

  H(kind: hkAwaitMany, waitKind: waitKind,
    waitTarget: addr waitTarget.argList, pauseOn: pauseOn)


func waitMany[A, Ak, E, Ek, F](
    H: typedesc[Handler[A, Ak, E, Ek, F]],
    waitKind: Ak, pauseOn: set[F] = {}
  ): H =

  H(kind: hkAwaitMany, waitKind: waitKind,
    pendingTargetSet: true, pauseOn: pauseOn)

type
  Ast[Ak] = ref object
    kind*: Ak
    subnodes*: seq[Ast[Ak]]


proc treeRepr[Ak](ast: Ast[Ak]): string =
  proc aux(ast: Ast[Ak], level: int): string =
    let pref = "  ".repeat(level)
    result &= alignLeft(pref & $ast.kind, 20) & "\n"
    for node in ast.subnodes:
      result &= aux(node, level + 1)

  return aux(ast, 0)

proc treeRepr[T](s: seq[T]): string =
  for item in s:
    result = "-\n"
    for line in split(treeRepr(item), '\n'):
      result &= "  " & line & "\n"


type
  EventKind = enum
    evList
    evLBrace
    evRBrace
    evComma,
    evIdent

    evFinish ## Finish of the input stream. Special event kind
    evAwaitFinish ## `await*` or `next*` handler has finished execution

  Event = object
    case kind*: EventKind
      of evIdent:
        strVal*: string

      else:
        discard

  AstKind = enum
    akList
    akBrace

  Flag = enum
    fInList
    fInBrace




proc eventParse(events: seq[Event]): seq[Ast[AstKind]] =
  type HandlerT = Handler[Ast[AstKind], AstKind, Event, EventKind, Flag]

  var handlers: array[EventKind, seq[HandlerT]]
  var context: ExecContext[Ast[AstKind], AstKind, Event, EventKind, Flag]

  context.newAst =
    proc(kind: AstKind, subnodes: seq[Ast[AstKind]]): Ast =
      Ast[AstKind](kind: kind, subnodes: subnodes)

  handlers[evList].add group(
    [HandlerT.lift(fInList)],
    [HandlerT.push akList],
    [HandlerT.waitMany(akBrace, {fInBrace})]
  )

  handlers[evRBrace].add group(
    fInBrace,
    [ HandlerT.drop(fInBrace)             ],
    [ HandlerT.push akBrace               ],
    [ HandlerT.popMany({akList, akBrace}) ]
  )

  handlers[evLBrace] = @[group([ HandlerT.lift fInBrace ])]
  handlers[evFinish].add group([ HandlerT.closeAll() ])
  handlers[evRBrace].add group(
    fInList,
    [
      HandlerT.drop(fInList),
      HandlerT.trim(akBrace, hkAwaitMany)
    ]
  )


  for ev in events:
    for handler in handlers[ev.kind]:
      if not handler.guarded or (
        context.flags.len > 0 and context.flags[^1] == handler.guardFlag
      ):
        handler.execOn(ev, context)

  return context.stack.mapIt(it.ast)



proc lex(str: string): seq[Event] =
  for ch in str:
    case ch:
      of '!': result.add Event(kind: evList)
      of '[': result.add Event(kind: evLBrace)
      of ']': result.add Event(kind: evRBrace)
      of ',': result.add Event(kind: evComma)

      of 'a' .. 'z':
        result.add Event(kind: evIdent, strVal: $ch)

      else:
        discard

  result.add Event(kind: evFinish)

when isMainModule:
  let text = lex("![[[][]]]")

  let ast = eventParse(text)
  for ast in ast:
    echo ast.treeRepr()
