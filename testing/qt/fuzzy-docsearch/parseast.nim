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
import strformat
import macros
import algorithm
import os
import re
import tables

# Create cache of unique identifiers
let cache: IdentCache = newIdentCache()
 # Project compilation configuration (name of the project, linked
 # libraries etc.)
let config: ConfigRef = newConfigRef()

proc dummyOpen(graph: ModuleGraph; module: PSym): PPassContext = discard

type
  ProcArg = object
    name: string
    vtype: string
    defval: string

  ProcKind = enum
    pkMacro
    pkFunc
    pkTemplate
    pkProc
    pkInterator

  ProcDefintion = object
    name: string
    args: seq[ProcArg]
    rett: string
    id: int
    docstr: string
    kind: ProcKind

  TypeDefinition = object
    name: string
    child: seq[tuple[name, typ: string]]

var procs: seq[ProcDefintion]
var types: seq[TypeDefinition]

proc registerProc(n: PNode): ProcDefintion =
  ## Register procedure or type declaration in the database
  ## Some procedures have
  ## MUltiple lines of documentation
  ## comments
  var docstr = ""
  if n.sons.len >= 6 and n[6].len > 0:
    let docComm = n[6][0]
    if docComm.kind == nkCommentStmt:
      docstr = docComm.comment

  ProcDefintion(
    name: n[0].renderPlainSymbolName(),
    args: n[3].sons
    .filterIt(it.kind == nkIdentDefs)
    .mapIt(ProcArg(
      vtype: it[1].renderPlainSymbolName(),
      name: it[0].renderPlainSymbolName(),
      defval: it[2].renderPlainSymbolName()
    )),
    rett: n[3].sons
    .filterIt(it.kind == nkIdent)
    .mapIt(it.renderPlainSymbolName())
    .join(""),
    docstr: docstr,
    kind:
    case n.kind:
      of nkProcDef: pkProc
      of nkTemplateDef: pkTemplate
      of nkMacroDef: pkMacro
      of nkFuncDef: pkMacro
      of nkIteratorDef: pkInterator
      else:
        raise newException(
          AssertionError,
          &"Invalid kind to ad as proc {n.kind}")
    )

func renderType(t: PNode): string =
  ## documentation for render type
  if t.kind == nkBracketExpr:
    "$1[$2]" % [t[0].renderType(), t[1].renderType()]
  else:
    t.renderPlainSymbolName()

func registerType(n: PNode): TypeDefinition =
  discard
  ## Register type declaration in database
  #  TypeDefinition(
  #    name: n[0].renderPlainSymbolName(),
  #    child: n[2].sons
  #      .filterIt(it.kind == nkRecList) # TODO replace with ident definition
  #      .mapIt(
  #        block:
  #          it.mapIt((name: it[0].renderType, typ: it[1].renderType))
  #      ).concat()
  #  )

proc registerToplevel(n: PNode): void =
  ## register AST node in the database
  case n.kind:
    of nkProcDef, nkFuncDef, nkIteratorDef, nkTemplateDef, nkMacroDef:
      procs.add registerProc(n)
    of nkStmtList:
      for s in n.sons: registerTopLevel(s)
    of nkTypeSection:
      for s in n.sons: registerTopLevel(s)
    of nkTypeDef:
      types.add registerType(n)
    else:
      discard


proc logASTNode(context: PPassContext; n: PNode): PNode =
  result = n
  registerToplevel(n)

proc registerAST*(program: string) =
  ## Register piece of code in the database
  let g: ModuleGraph = newModuleGraph(cache, config)
  var m: PSym = makeStdinModule(g)
  incl(m.flags, sfMainModule)
  registerPass(g, makePass(open = dummyOpen, process = logASTNode))
  processModule(g, m, llStreamOpen(program))


proc printResults(res: seq[seq[string]]; headers: seq[string] = @[]): void =
  # IDEA write transpose iterator for 2d seqs and generate widths for
  # all columns in the same way as I did for row-major matrix here.
  # = res.mapIt(it.mapIt(len(it)).max()).max()

  func clampStr(str: string; maxLen: int): string =
    if str.len > maxLen: str[0..maxLen]
    else: str

  var colWidths = newSeqWith(res[0].len, 0)
  for row in res & headers:
    for idx, cell in row:
      colWidths[idx] = max(colWidths[idx], cell.clampStr(20).len)

  for idx, row in headers & res:
    let line = "| " & toSeq(pairs(row)).mapIt(
      it[1].clampStr(20).alignLeft(colWidths[it[0]])
    ).join(" | ") & " |"

    if idx == 0:
      if headers.len != 0:
        let sep = "+" & toSeq(pairs(row)).mapIt(
          "-".repeat(colWidths[it[0]] + 2)
        ).join("+") & "+"
        echo ""
        echo sep
        echo line
        echo sep
    else:
      echo line

proc getHeaders(db: DbConn; tablename: string): seq[string] =
  db.getAllRows(sql(&"PRAGMA table_info({tablename})")).mapIt(it[1])

proc retecho(arg: string): string =
  echo arg
  arg

proc createProcTable(db: DbConn): void =
  for idx, pr in mpairs(procs):
    pr.id = idx

  db.exec(sql("DROP TABLE IF EXISTS arguments"))
  db.exec(sql("""
  CREATE TABLE arguments (
      procid INT NOT NULL,
      arg TEXT,
      type TEXT,
      idx INTEGER,
      defval TEXT
  )"""))

  for pr in procs:
    echo "registering arguments for ", pr.name
    for idx, arg in pr.args:
      let defval = if arg.defval.len == 0: "NULL" else: &"\"{arg.defval}\""
      let query = &"""
      INSERT INTO arguments
      (procid, arg, type, idx, defval)
      VALUES
      ("{pr.id}", "{arg.name}", "{arg.vtype}", "{idx}", {defval})
      """
      db.exec(sql(query))

  db.exec(sql("DROP TABLE IF EXISTS procs"))
  db.exec(sql("""
  CREATE TABLE procs (
      procid INT NOT NULL,
      procname TEXT,
      moduleid INT,
      docstring TEXT,
      rettype TEXT,
      kind TEXT
  )"""))

  for pr in procs:
    echo "registering ", pr.name
    let docstr = pr.docstr
    let query = &"""
    INSERT INTO procs
    (procid, procname, docstring, rettype, kind)
    VALUES
    ("{pr.id}", "{pr.name}", {docstr.dbQuote()}, '{pr.rett}', '{pr.kind}')
    """

    if not query.contains("?"):
      db.exec(sql(query))
    else:
      echo "query ", query
      echo "contains ?"

proc registerDirectoryRec(target: string): void =
  for file in target.walkDirRec():
    if file =~ re"(.*?)\.nim$":
      registerAST(file.readFile())

proc main() =
  let thisSource = currentSourcePath().readFile().string()
  var installed = toSeq("~/.choosenim/toolchains/".expandTilde().walkDir())
  installed = installed.sortedByIt(it.path)
  let target = installed[^1].path

  echo "Using toolchain at path", target


  registerDirectoryRec(target & "/lib")
  registerDirectoryRec("~/workspace/hax-nim".expandTilde())

  let db = open("database.tmp.db", "", "", "")
  db.createProcTable()
  db.close()

  echo "done"
  echo &"found {procs.len} procs in total"
  var cnt = @[(pkFunc, 0), (pkInterator, 0), (pkProc, 0), (pkMacro, 0), (
      pkTemplate, 0)].toTable()
  for pr in procs:
    inc cnt[pr.kind]

  for kind, num in cnt:
    echo kind, ": ", num


main()

