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

import db_sqlite

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

func registerProc(n: PNode): ProcDefintion =
  ProcDefintion(
    name: n[0].renderPlainSymbolName(),
    args: n[3].sons
      .filterIt(it.kind == nkIdentDefs)
      .mapIt(it[1].renderPlainSymbolName()),
    rett: n[3].sons
      .filterIt(it.kind == nkIdent)
      .mapIt(it.renderPlainSymbolName())
      .join("")
    )

func renderType(t: PNode): string =
  if t.kind == nkBracketExpr:
    "$1[$2]" % [t[0].renderType(), t[1].renderType()]
  else:
    t.renderPlainSymbolName()

func registerType(n: PNode): TypeDefinition =
  TypeDefinition(
    name: n[0].renderPlainSymbolName(),
    child: n[2].sons
      .filterIt(it.kind == nkRecList) # TODO replace with ident definition
      .mapIt(
        block:
          it.mapIt((name: it[0].renderType, typ: it[1].renderType))
      ).concat()
  )

var procs: seq[ProcDefintion]
var types: seq[TypeDefinition]

proc registerToplevel(n: PNode): void =
  case n.kind:
    of nkProcDef:
      procs.add registerProc(n)
    of nkStmtList:
      for s in n.sons: registerTopLevel(s)
    of nkTypeSection:
      for s in n.sons: registerTopLevel(s)
    of nkTypeDef:
      types.add registerType(n)
    else:
      discard


proc logASTNode(context: PPassContext, n: PNode): PNode =
  result = n
  registerToplevel(n)

proc registerAST*(program: string) =
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

registerAST(thisSource)

let db = open("database.tmp.db", "", "", "")

const procedure_args = "procedure_args"

db.exec(sql("DROP TABLE IF EXISTS $1" % procedure_args))

db.exec(sql("""
CREATE TABLE $1 (
    name VARCHAR(255) NOT NULL,
    arg VARCHAR(255)
)""" % procedure_args))

proc printResults(res: seq[seq[string]]): void =
  let colWidth = res.mapIt(it.mapIt(len(it)).max()).max()
  for row in res:
    # let sep = "+" & row.mapIt("-".repeat(colWidth)).join("+") & "+"
    let line = "| " & row.mapIt(it.alignLeft(colWidth)).join(" | ") & " |"
    echo line

for pr in procs:
  for arg in pr.args:
    let query = "INSERT INTO $1 (name, arg) VALUES (\"$2\", \"$3\")" % [
      procedure_args, pr.name, arg
    ]
    db.exec(sql(query))

printResults db.getAllRows sql("SELECT * FROM $1" % procedure_args)

db.close()
