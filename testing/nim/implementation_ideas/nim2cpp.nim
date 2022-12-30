import compiler/[ast, parser, idents, options, renderer]
import hmisc/core/all
import std/[strutils, sequtils]
startHax()

type
  CodeContextKind = enum
    ccToplevel
    ccStandaloneStmt
    ccExpression
    ccFunctionName
    ccFunctionArg
    ccType
    ccField
    ccVar
    ccLet
    ccGeneric

  State = object
    context: seq[CodeContextKind]

func `+`*(s: sink State, k: CodeContextKind): State =
  result = s
  result.context.add k

func `of`(s: State, k: CodeContextKind | set[CodeContextKind]): bool =
  s.context.notEmpty() and s.context.last() of k

iterator items(n: PNode): PNode =
  for s in n.sons:
    yield s

iterator pairs(n: PNode): tuple[idx: int, sub: PNode] =
  for idx, s in n.sons:
    yield (idx, s)


func `[]`(n: PNode, s: HSlice[int, BackwardsIndex]): seq[PNode] =
  n.sons[s]

proc doc(node: PNode): string =
  if 0 < node.comment.len():
    result = "/*!"
    result.add node.comment
    result.add "\n*/"

proc treeRepr(node: PNode): string =
  proc aux(n: PNode, l: int): string =
    result.add repeat("  ", l)
    result.add $n.kind
    result.add " "
    template d() =
      if 0 < n.comment.len():
        for line in n.comment.splitLines():
          result.add "\n"
          result.add repeat("  ", l + 1)
          result.add "# "
          result.add line

    case n.kind:
      of nkIdent:
        result.add n.ident.s
        d()

      of nkSym:
        result.add n.sym.name.s
        d()

      of nkCharLit .. nkUInt64Lit:
        result.add $n.intVal
        d()

      of nkFloatLit .. nkFloat64Lit:
        result.add $n.floatVal
        d()

      of nkStrLit .. nkTripleStrLit:
        result.add "\"$#\"" % $n.strVal
        d()

      else:
        d()
        for sub in n:
          result.add "\n"
          result.add aux(sub, l + 1)

  return aux(node, 0)

func top(s: State): auto = s.context.top()

func escapeCxxStr(s: string, multi: bool = false): string =
  for c in s:
    result.add case c:
      of '\\': "\\\\"
      of '\n': (if multi: $c else: "\\n")
      else: $c


iterator flatIdentDefs(
    node: PNode, initIdx: int = 0): tuple[id, typ, expr: PNode, idx: int] =
  var idx = initIdx
  for name in node[0..^3]:
    yield (name, node[^2], node[^1], idx)
    inc idx

iterator flatIdentDefs(
    nodes: seq[PNode]): tuple[id, typ, expr: PNode, idx: int] =

  var idx = 0
  for node in nodes:
    for (id, typ, expr, resIdx) in flatIdentDefs(node, idx):
      yield (id, typ, expr, resIdx)
      idx = resIdx
    inc idx

proc toCpp(node: PNode, s: State): string =
  proc die(n: PNode) =
    raise newUnexpectedKindError(
      n,
      $s.context & "\n" &
        node.treeRepr() & "\n" &
        $node)

  case node.kind:
    of nkStmtList, nkTypeSection:
      for it in node:
        result.add "\n"
        result.add it.toCpp(s + ccStandaloneStmt)
        result.add ";"

    of nkStmtListExpr:
      result.add "/* FIXME STMT LIST EXPR */"
      for it in node:
        result.add "\n"
        result.add it.toCpp(s + ccStandaloneStmt)

    of nkIfStmt, nkWhenStmt, nkIfExpr:
      let note = tern(node of nkIfExpr, "/* FIXME expression */", "")
      for idx, branch in node:
        if branch of nkElifBranch:
          result.add "$else $const if ($expr) { $body $note }" % {
            "else": tern(idx == 0, "", "else"),
            "const": tern(node of nkWhenStmt, "constexpr", ""),
            "expr": toCpp(branch[0], s),
            "body": toCpp(branch[1], s),
            "note": note
          }

        else:
          result.add "else { $# }" % [
            toCpp(branch[0], s),
          ]

    of nkBreakStmt:
      result = "break;"

    of nkVarTuple:
      result = "auto [$#] $#" % [
        node[0..^2].mapIt(it.toCpp(s)).join(", "),
        toCpp(node[^1], s)
      ]

    of nkIntLit:
      result = $node.intVal

    of nkInt16Lit: result = "I16($#)" % $node.intVal
    of nkUInt16Lit: result = "U16($#)" % $node.intVal
    of nkUInt8Lit: result = "U8($#)" % $node.intVal
    of nkInt8Lit: result = "I8($#)" % $node.intVal
    of nkInt32Lit: result = "I32($#)" % $node.intVal
    of nkUInt32Lit: result = "U32($#)" % $node.intVal
    of nkInt64Lit: result = "I64($#)" % $node.intVal
    of nkUInt64Lit: result = "U64($#)" % $node.intVal
    of nkFloat64Lit: result = $node.floatVal
    of nkFloat32Lit: result = $node.floatVal

    of nkUIntLit:
      result = "UI($#)" % $node.intVal

    of nkFloatLit:
      result = $node.floatVal

    of nkCallStrLit:
      result = "$#$#" % [
        node[1].strVal.escapeCxxStr(),
        node[0].toCpp(s)
      ]

    of nkPragmaBlock:
      result = "/* PRAGMA BLOCK $# */\n$#" % [
        $node[0],
        node[1].toCpp(s)
      ]

    of nkTryStmt:
      result = "try {"
      result.add node[0].toCpp(s)
      result.add "}"
      for branch in node[1..^1]:
        if branch of nkExceptBranch:
          let body = branch[^1].toCpp(s)
          var name: string
          var varname: string
          if branch[0] of nkIdent:
            name = branch[0].toCpp(s)

          elif len(branch) == 1:
            name = "..."

          else:
            name = branch[0][1].toCpp(s)
            varname = branch[0][2].toCpp(s)

          name = case name:
            of "Exception": "std::exception"
            else: name

          result.add "except($name $varname) {$body}" % {
            "name": name,
            "varname": varname,
            "body": body
          }

        else:
          result.add "/* UNCONVERTIBLE $# */" % [
            $branch
          ]


    of nkStrLit, nkTripleStrLit, nkRStrLit:
      result = "R(\"$#\")" % node.strVal.escapeCxxStr(
        node of nkTripleStrLit)

    of nkIncludeStmt:
      result = "/* INCLUDE $# */" % $node

    of nkCharLit:
      result = "'$#'" % escapeCxxStr($node.intVal.char())

    of nkPrefix:
      let op = toCpp(node[0], s)
      let reop = case op:
        of "?": "notEmpty"
        of "$": "toStr"
        of "^": "backIdx"
        else: op

      if op == reop:
        result = "($#($#))" % [reop, toCpp(node[1], s)]

      else:
        result = "$#($#)" % [reop, toCpp(node[1], s)]

    of nkCaseStmt:
      result.add "switch ($#) {" % node[0].toCpp(s)
      for idx, branch in node[1..^1]:
        if branch of nkOfBranch:
          for item in branch[0..^2]:
            result.add "case $#:" % toCpp(item, s)

          result.add "{"
          result.add branch[^1].toCpp(s)
          result.add "break;"
          result.add "}"

        else:
          result.add "default: { $# }" % toCpp(branch[0], s)

      result.add "}"

    of nkReturnStmt:
      result = "return $#;" % toCpp(node[0], s)

    of nkRaiseStmt:
      result = "throw $#;" % toCpp(node[0], s)

    of nkRecList:
      for it in node:
        result.add it.toCpp(s + ccField)

    of nkTupleTy:
      result = "std::tuple<$#>" % [
        node.mapIt(it.toCpp(s + ccType)).join(", ")
      ]

    of nkBindStmt:
      result = "/* BIND STMT */"

    of nkRefTy:
      if len(node) == 0:
        result = "/* REF TYPECLASS */"

      else:
        result = "std::shared_ptr<$#>" % toCpp(node[0], s)

    of nkPtrTy:
      if len(node) == 0:
          result = "/* PTR TYPECLASS */"

      else:
        result = "$#*" % toCpp(node[0], s)

    of nkEnumTy:
      result = "/* ENUM TY */"

    of nkVarTy:
      result = "$#&" % toCpp(node[0], s)

    of nkRecCase:
      result.add node[0].toCpp(s)
      result.add "union {"
      for branch in node[1..^1]:
        result.add "/* IF $# */" % branch[0..^2].mapIt($it).join(", ")
        result.add "struct {"
        result.add toCpp(branch[^1], s)
        result.add "};"
      result.add "};"

    of nkUsingStmt, nkExportStmt:
      discard

    of nkConstSection:
      result = "/* TODO CONST SECTION */"

    of nkCast:
      result = "std::static_cast<$#>($#)" % [
        toCpp(node[0], s + ccType),
        toCpp(node[1], s)
      ]

    of nkNilLit:
      result = "/* NIL LIT */"

    of nkDistinctTy:
      if len(node) == 0:
        result = "/* DISTINCT TYPECLASS */"

      else:
        result = "/* distinct */ $#" % [
          toCpp(node[0], s)
        ]

    of nkExprColonExpr:
      result = ".$# = $#" % [
        toCpp(node[0], s),
        toCpp(node[1], s)
      ]

    of nkTypeDef:
      result.add "/* TYPE DEF $# */" % $node
      proc body(body: PNode): string = result.add body[2].toCpp(s)
      proc inh(body: PNode): string =
        if not(body[1] of nkEmpty):
          var base = body[1][0]
          if base of nkRefTy:
            base = base[0]

          result = ": public $#" % toCpp(base, s + ccType)

      case node[2].kind:
        of nkRefTy:
          if node[2][0] of nkObjectTy:
            result.add (
              "/* FIXME REF OBJ */$template struct $name $base {$body};" % {
                "template": node[1].toCpp(s),
                "name": node[0].toCpp(s + ccType),
                "body": node[2][0].body(),
                "base": node[2][0].inh(),
              })

          else:
            result.add "/* REF TYPEDEF */ $template using $old = $new" % {
              "template": node[1].toCpp(s),
              "old": node[0].toCpp(s + ccType),
              "new": node[2].toCpp(s + ccType)
            }

        of nkCall:
          result.add "$template using $old = decltype($new)" % {
            "template": node[1].toCpp(s),
            "old": node[0].toCpp(s + ccType),
            "new": node[2].toCpp(s + ccType)
          }

        of nkInfix:
          result.add "/* FIXME INFIX $# */" % $node

        of nkEnumTy:
          var fields: string
          for field in node[2][1..^1]:
            var fieldf = ""
            case field.kind:
              of nkIdent: fieldf = toCpp(field, s)
              of nkEnumFieldDef: fieldf = "$# = $#" % [
                toCpp(field[0], s),
                toCpp(field[1], s)
              ]
              else: die(field)

            fields.add "$name $value, $doc" % {
              "name": fieldf,
              "value": "",
              "doc": field.doc()
            }

          result.add "enum $name $doc {$body};" % {
            "doc": node.doc(),
            "name": node[0].toCpp(s + ccType),
            "body": fields
          }


        of nkObjectTy:
          result.add "$template struct $name $base {$body};" % {
            "template": node[1].toCpp(s),
            "name": node[0].toCpp(s + ccType),
            "body": node[2].body(),
            "base": node[2].inh(),
          }

        of nkTypeClassTy:
          discard

        of nkEmpty:
          result = "/* MAGIC TYPE */"

        else:
          result.add "$template using $old = $new;" % {
            "template": node[1].toCpp(s),
            "old": node[0].toCpp(s + ccType),
            "new": node[2].toCpp(s + ccType)
          }

    of nkRecWhen:
      result = "/* REC WHEN */"
      for branch in node:
        result.add "/* BRANCH $# */" % $branch[0..^2].mapIt($it).join(" ")
        result.add toCpp(branch[^1], s)

    of nkIdent:
      result = node.ident.s

      if s of ccType:
        case node.ident.s:
          of "seq": result = "Vec"
          of "string": result = "Str"
          of "int8": result = "I8"
          of "int16": result = "I16"
          of "int32": result = "I32"
          of "int64": result = "I64"
          of "unt8": result = "U8"
          of "unt16": result = "U16"
          of "unt32": result = "U32"
          of "unt64": result = "U64"

      else:
        case node.ident.s:
          of "or": result = "||"
          of "and": result = "&&"
          of "xor": result = "^"
          of "not": result = "!"


    of nkAsmStmt:
      result = "/* ASM $# */" % $node

    of nkIdentDefs:
      let typ = node[^2].toCpp(s + ccType)
      let expr = tern(
        node[^1] of nkEmpty,
        "",
        " = " & node[^1].toCpp(s + ccExpression))


      for idx, def in node[0..^3]:
        result.add "$const $type $name $expr $sep $doc" % {
          "const": tern(s of ccLet, "const", ""),
          "name": def.toCpp(s),
          "type": tern(
            s of ccFunctionArg and not typ.endsWith("&"),
            "const $#&" % typ, typ),
          "expr": expr,
          "sep": tern(
            s of {ccFunctionArg, ccGeneric, ccType},
            tern(0 < idx, ",", ""), ";"),
          "doc": node.doc()
        }

    of nkEmpty:
      case s.context.top():
        of ccFunctionArg: result = "void"
        of ccType: result = "auto"
        else: result = ""

    of nkDiscardStmt:
      result = node[0].toCpp(s)

    of nkMixinStmt:
      result = "/* MIXIN $# */" % $node

    of nkCurlyExpr:
      result = "/* TODO CURLY $# */" % $node

    of nkPragmaExpr:
      case s.top():
        of ccType:
          result = toCpp(node[1], s)

        else:
          result = "/* PRAGMA $# */ $#" % [
            $node[1],
            toCpp(node[0], s)
          ]

    of nkPostfix:
      result = node[1].toCpp(s)

    of nkAccQuoted:
      var id = ""
      for it in node:
        id.add it.toCpp(s)

      case s.top():
        of ccFunctionName: result = "operator $#" % id
        else: result = "/* ACC QUOTED */ $#" % id

    of nkAsgn:
      result = "$# = $#;" % [ toCpp(node[0], s), toCpp(node[1], s) ]

    of nkForStmt:
      if node[^2] of nkInfix and node.len() == 3:
        result = "for (int $idx = $lhs; $idx $op $rhs; ++i) {$body}" % {
          "idx": node[0].toCpp(s),
          "lhs": node[1][1].toCpp(s),
          "op": tern(node[1][0].ident.s == "..", "<=", "<"),
          "rhs": node[1][2].toCpp(s),
          "body": node[2].toCpp(s)
        }

      else:
        let args = node[0..^3]
        let init = tern(
          args.len() == 1,
          args[0].toCpp(s),
          "[" & args.mapIt(it.toCpp(s)).join(", ") & "]"
        )

        result = "for (const auto $init : $expr) { $body }" % {
          "init": init,
          "expr": node[^2].toCpp(s),
          "body": node[^1].toCpp(s)
        }

    of nkYieldStmt:
      result = "result.push_back($#)" % toCpp(node[0], s)

    of nkTemplateDef: result = "/* FIXME TEMPLATE */ /* \n\n$#\n*/" % $node
    of nkMacroDef: result = "/* FIXME MACRO */ /* \n\n$#\n*/" % $node
    of nkPragma: result = "/* FIXME PRAGMA */ /* \n\n$#\n*/" % $node

    of nkBlockStmt:
      result = "//$#\n{$#}" % [ toCpp(node[0], s), toCpp(node[1], s) ]

    of nkTupleConstr:
      result.add "{"
      if node[0] of nkExprColonExpr:
        for arg in node:
          result.add ".$# = $#," % [ toCpp(arg[0], s), toCpp(arg[1], s) ]

      else:
        for arg in node:
          result.add "$#," % toCpp(arg, s)

      result.add "}"

    of nkPar:
      assert node.len() == 1, node.treeRepr()
      result = "($#)" % toCpp(node[0], s)

    of nkStaticStmt:
      result = "/* FIXME STATIC */ /* $# */" % $node

    of nkIteratorTy:
      result = "generator<$#>" % node[0][0].toCpp(s + ccType)

    of nkExportExceptStmt:
      result = "/* EXPORT EXCEPT $# */" % $node

    of nkProcTy:
      if len(node) == 0:
        result = "/* PROC TYPECLASS */"

      else:
        result = "std::function<$#(" % node[0][0].toCpp(s + ccType)
        for (id, typ, expr, idx) in flatIdentDefs(node[0][1..^1]):
          if 0 < idx: result.add ", "
          result.add toCpp(typ, s + ccType)

    of nkObjConstr:
      result.add "$#{" % toCpp(node[0], s)
      for arg in node[1..^1]:
        result.add ".$# = $#," % [ toCpp(arg[0], s), toCpp(arg[1], s) ]
      result.add "}"

    of nkVarSection, nkLetSection:
      for sub in node:
        result.add "\n"
        result.add sub.toCpp(s + tern(node of nkVarSection, ccVar, ccLet))
        result.add ";"

    of nkLambda, nkDo:
      result = "[]($args){$body}" % {
        "args": node[3][1..^1].mapIt(it.toCpp(s + ccFunctionArg)).join(", "),
        "body": node[6].toCpp(s)
      }

    of nkProcDef, nkFuncDef, nkIteratorDef,
       nkMethodDef, nkConverterDef:
      var body = node[6].toCpp(s)
      var ret = node[3][0].toCpp(s + ccFunctionArg)
      if node of nkIteratorDef:
        body = "Vec<$#> result{}; $# return result;" % [ret, body]
        ret = "Vec<$#>" % ret

      else:
        if ret != "void":
          body = "$# result; $# return result;" % [ret, body]

      result.add "/* ORIG:\n\n$#\n$#\n\n*/" % [$node, $node.treeRepr()]
      result.add "$template\n$returnType $name($args) {$body}" % {
        "name": node[0].toCpp(s + ccFunctionName),
        "template": toCpp(node[2], s + ccGeneric),
        "returnType": ret,
        "body": body,
        "args": node[3][1..^1].mapIt(it.toCpp(s + ccFunctionArg)).join(", ")
      }

    of nkImportStmt:
      result = "/* IMPORT $# */" % $node

    of nkDotExpr:
      result = "$#.$#" % [ toCpp(node[0], s), toCpp(node[1], s) ]

    of nkGenericParams:
      result = "template<"
      var idx = 0
      for par in node:
        for id in par[0..^3]:
          if 0 < idx: result.add ", "
          inc idx
          result.add "typename $#" % id.toCpp(s)

      result.add ">"

    of nkInfix:
      let op = toCpp(node[0], s + ccExpression)
      let reop = case op:
        of "..": "rangeIncl"
        of "..<": "rangeExcl"
        else: op


      if op == reop:
        result = "(($#) $# ($#))" % [
          toCpp(node[1], s + ccExpression),
          reop,
          toCpp(node[2], s + ccExpression),
        ]

      else:
        result = "$#($#, $#)" % [
          reop,
          toCpp(node[1], s + ccExpression),
          toCpp(node[2], s + ccExpression),
        ]

    of nkObjectTy:
      result = "/* OBJECT TYPECLASS */"

    of nkCall, nkCommand:
      let name = node[0].toCpp(s + ccExpression)
      if name == "sink":
        result = "const $#&" % node[1].toCpp(s)

      elif name in ["inc", "dec"]:
        result = "$name $value" % {
          "name": tern(name == "inc", "+=", "-="),
          "value": tern(node.len() == 1, "1", node[1].toCpp(s)),
        }

      else:
        result = "$name($args)" % {
          "name": name,
          "args": mapIt(node[1..^1], toCpp(it, s + ccExpression)).join(", "),
        }

    of nkElse:
      result = "else {$#}" % $node[0].toCpp(s)

    of nkOfBranch:
      result = "of ($#) { $# }" % [
        node[0..^2].mapIt(it.toCpp(s)).join(", "),
        node[^1].toCpp(s)
      ]

    of nkCurly, nkBracket:
      result = "{$#}" % [
        node.mapIt(it.toCpp(s)).join(", ")
      ]

    of nkTableConstr:
      result = "{"
      var prev: seq[string]
      for item in node:
        if safeLen(item) == 0:
          prev.add toCpp(item, s)

        else:
          let rhs = toCpp(item[^1], s + ccExpression)
          for item in prev:
            result.add "{$#, $#}," % [ item, rhs ]

          prev = @[]

          for lhs in item[0..^2]:
            result.add "{$#, $#}," % [ lhs.toCpp(s + ccExpression), rhs ]

      result.add "}"

    of nkBracketExpr:
      if len(node) == 1:
        result = "(*($#))" % node[0].toCpp(s)

      else:
        result = "${name}${op}${args}${cl}" % {
          "name": node[0].toCpp(s),
          "args": mapIt(node[1..^1], it.toCpp(s)).join(", "),
          "op": tern(s of {ccGeneric, ccType}, "<", "["),
          "cl": tern(s of {ccGeneric, ccType}, ">", "]"),
        }

    of nkCommentStmt:
      result = doc(node)

    of nkExprEqExpr:
      result = "$# = $#" % [ toCpp(node[0], s), toCpp(node[1], s) ]

    of nkWhileStmt:
      result = "while($#){$#}" % [ toCpp(node[0], s), toCpp(node[1], s) ]

    of nkContinueStmt:
      result = "continue;"

    of nkFromStmt:
      result = "/* FROM $# */" % $node

    of nkTupleClassTy:
      result = "/* TUPLE CLASS TY */"

    of nkImportExceptStmt:
      result = "/* IMPORT EXCEPT $# */" % $node

    of nkDefer:
      result = "finally _defer{[](){$#}}" % toCpp(node[0], s)

    else:
      die(node)

import compiler/lineinfos

proc parse(s: string): PNode =
  var id = newIdentCache()
  var conf = newConfigRef()
  conf.notes.excl hintLineTooLong
  conf.structuredErrorHook = proc(
    config: ConfigRef, info: TLineInfo, msg: string, severity: Severity) =
    discard

  return parseString(s, id, conf)

import hmisc/other/oswrap

let root = AbsDir"/mnt/workspace/repos"
let res = AbsDir"/tmp"


func startswith(s: string, o: openarray[string]): bool =
  for i in o:
    if s.startsWith(i):
      return true

func contains(s: string, o: openarray[string]): bool =
  for i in o:
    if s.contains(i):
      return true

proc conv(ffrom, fto: AbsFile) =
  if ffrom.exists():
    echov "reading", ffrom
    let start = State(context: @[ccToplevel])
    mkDir fto.dir()
    writeFile(fto, ffrom.readFile().parse().toCpp(start))
    echov "wrote", fto

conv(ffrom = AbsFile"/tmp/test.nim", fto = AbsFile"/tmp/test.cpp")

for file in walkDir(
    root,
    RelFile,
    exts = @["nim"],
    recurse = true
  ):
  if file.string.startsWith("Nim") or
     file.string.contains([
       "tests",
       "compiler",
       "i386",
       "mysql",
       "pubgrub",
       "extract_cp1251_strings_from_dll",
       "cursor_jump",
       "direct_ast_matching",
       "astrepr_positional_names"
     ]):
    continue

  if "hlex_base" notin file.string:
    continue

  conv(root / file, withExt(res / file, "cpp"))

echo "done"
