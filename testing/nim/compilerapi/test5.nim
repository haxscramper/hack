import common
import hnimast
import std/[tables, random, macros]
import compiler/[nimeval, llstream, ast, renderer, vmdef, vm]
import fusion/matching

type
  UsrImpl = object
    field: string
    value: float

  UsrPNode = distinct PNode


proc id(usr: UsrPNode): int =
  # AST representation of `Usr` type in vm has following form:
  #
  #```
  #     ObjConstr
  # [0]    Empty
  # [1]    ExprColonExpr
  # [1][0]    Ident id
  # [1][1]    IntLit 12
  #```
  #
  # `IntLit` node is a value of field `id`
  PNode(usr)[1][1].intVal.int

var usrPool: Table[int, UsrImpl]

macro exportVM*(procDecl: untyped): untyped =
  procDecl.assertMatch(
    ProcDef[
      # Match proc name in full form
      @name is ( # And get standalone `Ident`
        Postfix[_, @procIdent] | # Either in exported form
        (@procIdent is Ident()) # Or regular proc definition
      ),
      _, # Skip term rewriting template
      _, # Skip generic parameters
      [ # Match arguments/return types
        @returnType, # Get return type

        # Match full `IdentDefs` for first argument, and extract it's name
        # separately
        @firstArg is IdentDefs[@firstArgName, _, _],

        # Match all remaining arguments. Collect both `IdentDefs` into
        # sequence, and extract each argument separately
        all @trailArgs is IdentDefs[@trailArgsName, _, _]
      ],
      .._
    ]
  )

  result = newStmtList(procDecl)
  result.add nnkProcDef.newTree(
    name,
    newEmptyNode(),
    newEmptyNode(),
    nnkFormalParams.newTree(
      @[
        returnType,
        nnkIdentDefs.newTree(firstArgName, ident "UsrPNode", newEmptyNode()),
      ] & trailArgs),
    newEmptyNode(),
    newEmptyNode(),
    nnkCall.newTree(@[
      procIdent,
      nnkBracketExpr.newTree(
        ident "usrPool",
        nnkCall.newTree(ident "id", firstArgName)
      )
    ] & trailArgsName)
  )

proc backendAction*(usr: UsrImpl) {.exportVM.} =
  echo "Running action in C code"

proc backendAction*(usr: UsrImpl, arg: int) {.exportVM.} =
  echo "Running backend action with argument ", arg

let intr = createInterpreter(
  "scriptname.nims",
  toSeqString(@[stdlib, stdlib / "pure", stdlib / "core",]))

intr.implementRoutine(
  "*", "scriptname", "newUsr",
  # NOTE: I'm not sure how to handle access to external state correctly -
  # `usrPool` is a potentially-GC'ed object. It could potentially be
  # handled by wrapping each interpreter instance in single object that
  # also contains all necessary pools. This way all external objects will
  # be deallocated only together with interpreter instance itself.
  # Probably.
  proc(args: VmArgs) {.nimcall, gcsafe.} =
    var id: int = rand(high(int))
    while id in usrPool:
      id = rand(high(int))

    # Create actual nim object
    usrPool[id] = UsrImpl(field: "Usr world")

    args.setResult(nnkObjConstr.newPTree(
      newEmptyPNode(),
      nnkExprColonExpr.newPTree(
        # Pass id for opaque handle to nimscript side
        newPIdent("id"), newPLit(id)
      )
    ))
)

intr.implementRoutine("*", "scriptname", "backendAction_UsrImpl",
  proc(args: VmArgs) {.nimcall, gcsafe.} =
    backendAction(UsrPNode(args.getNode(0)))
)


intr.implementRoutine("*", "scriptname", "backendAction_UsrImpl_int",
  proc(args: VmArgs) {.nimcall, gcsafe.} =
    backendAction(UsrPNode(args.getNode(0)), args.getInt(1).int)
)


intr.evalScript(llStreamOpen("""
type
  Usr = object
    id: int

proc newUsr(): Usr = discard

proc backendAction_UsrImpl(usr: Usr) = assert false, "not reimplemented"
proc backendAction(usr: Usr) =
  backendAction_UsrImpl(usr)

proc backendAction_UsrImpl_int(usr: Usr, arg: int) = assert false, "not reimplemented"
proc backendAction(usr: Usr, arg: int) =
  backendAction_UsrImpl_int(usr, arg)

let test* = newUsr()

echo "Created new usr with id '", test.id, "' "

test.backendAction()

for i in [0, 1, 2, 3]:
  test.backendAction(i)
"""))

echo intr.getGlobalValue(intr.selectUniqueSymbol("test")).treeRepr(indexed = true)
