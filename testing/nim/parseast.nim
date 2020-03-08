import compiler/[
  modules,
  ast,
  astalgo,
  passes,
  llstream,
  modulegraphs,
  idents,
  options,

  typesrenderer
]

import sequtils
import strutils

# Create cache of unique identifiers
let cache: IdentCache = newIdentCache()
# Project compilation configuration (name of the project, linked
# libraries etc.)
let config: ConfigRef = newConfigRef()

proc dummyOpen(graph: ModuleGraph; module: PSym): PPassContext = discard

type
  ProcDefintion = object
    name: string
    args: seq[string]
    rett: string

  TypeDefinition = object
    name: string
    child: seq[tuple[name, typ: string]]

proc registerProc(n: PNode): void =
  # for idx, s in n.sons:
  #   echo idx
  #   debug s

  let prc = ProcDefintion(
    name: n[0].renderPlainSymbolName(),
    args: n[3].sons
      .filterIt(it.kind == nkIdentDefs)
      .mapIt(it[1].renderPlainSymbolName()),
    rett: n[3].sons
      .filterIt(it.kind == nkIdent)
      .mapIt(it.renderPlainSymbolName())
      .join("")
    )

  echo prc

func renderType(t: PNode): string =
  if t.kind == nkBracketExpr:
    "$1[$2]" % [t[0].renderType(), t[1].renderType()]
  else:
    t.renderPlainSymbolName()

proc registerType(n: PNode): void =
  let tdf = TypeDefinition(
    name: n[0].renderPlainSymbolName(),
    child: n[2].sons
      .filterIt(it.kind == nkRecList) # TODO replace with ident definition
      .mapIt(
        block:
          it.mapIt((name: it[0].renderType, typ: it[1].renderType))
      ).concat()
  )
  echo tdf

proc registerToplevel(n: PNode): void =
  case n.kind:
    of nkProcDef: registerProc(n)
    of nkStmtList:
      for s in n.sons: registerTopLevel(s)
    of nkTypeSection:
      for s in n.sons: registerTopLevel(s)
    of nkTypeDef:
      registerType(n)
    else:
      discard


proc logASTNode(context: PPassContext, n: PNode): PNode =
  result = n
  registerToplevel(n)

proc displayAST*(program: string) =
  let g: ModuleGraph = newModuleGraph(cache, config)
  var m: PSym = makeStdinModule(g)
  incl(m.flags, sfMainModule)
  registerPass(g, makePass(open = dummyOpen, process = logASTNode))
  processModule(g, m, llStreamOpen(program))

let thisSource = currentSourcePath().readFile().string()

# displayAST("""
# proc hi(rr: string = "12"): int =
#   ## Doc comment
#   echo "hi"
# """)

displayAST(thisSource)
