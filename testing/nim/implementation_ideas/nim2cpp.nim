import compiler/[ast, parser, idents, options]
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

func `[]`(n: PNode, s: HSlice[int, BackwardsIndex]): seq[PNode] =
  n.sons[s]

proc doc(node: PNode): string =
  if 0 < node.comment.len():
    result = "/*!"
    for line in node.comment.splitLines():
      result.add "\n" & line

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

func escapeCxxStr(s: string): string =
  result = "R\"("
  for c in s:
    result.add c
  result.add ")\""


proc toCpp(node: PNode, s: State): string =
  proc die(n: PNode) =
    raise newUnexpectedKindError(
      n, $s.context & "\n" & node.treeRepr())

  case node.kind:
    of nkStmtList, nkTypeSection:
      for it in node:
        result.add "\n"
        result.add it.toCpp(s + ccStandaloneStmt)

    of nkIfStmt:
      for idx, branch in node:
        if branch of nkElifBranch:
          result.add "$# if ($#) { $# }" % [
            tern(idx == 0, "", "else"),
            toCpp(branch[0], s),
            toCpp(branch[1], s)
          ]

        else:
          result.add "else { $# }" % [
            toCpp(branch[0], s),
          ]


    of nkIntLit:
      result = $node.intVal

    of nkStrLit:
      result = "$#" % node.strVal.escapeCxxStr()

    of nkPrefix:
      result = "$#$#" % [
        toCpp(node[0], s),
        toCpp(node[1], s)
      ]

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
        result.add "\n"
        result.add it.toCpp(s + ccField)
        result.add ";"

    of nkTupleTy:
      result = "std::tuple<$#>" % [
        node.mapIt(it.toCpp(s + ccType)).join(", ")
      ]

    of nkRefTy:
      result = "$#*" % toCpp(node[0], s)

    of nkRecCase:
      result = "/* TODO REC CASE */"

    of nkUsingStmt, nkExportStmt:
      discard

    of nkConstSection:
      result = "/* TODO CONST SECTION */"

    of nkTypeDef:
      case node[2].kind:
        of nkIdent, nkTupleTy:
          result = "$template using $old = $new" % {
            "template": node[1].toCpp(s),
            "old": node[0].toCpp(s),
            "new": node[2].toCpp(s)
          }

        of nkEnumTy:
          var fields: string
          for field in node[2][1..^1]:
            fields.add "$name $value, $doc" % {
              "name": field.toCpp(s),
              "value": "",
              "doc": field.doc()
            }

          result = "enum $name $doc {$body};" % {
            "doc": node.doc(),
            "name": node[0].toCpp(s),
            "body": fields
          }

        of nkObjectTy:
          result = "$template struct $name {$body};" % {
            "template": node[1].toCpp(s),
            "name": node[0].toCpp(s + ccType),
            "body": node[2][2].toCpp(s)
          }

        else:
          die(node[2])

    of nkIdent:
      result = node.ident.s

      if s of ccType:
        case node.ident.s:
          of "seq": result = "std::vector"

    of nkIdentDefs:
      let typ = node[^2].toCpp(s + ccType)
      let expr = node[^1].toCpp(s + ccExpression)
      for idx, def in node[0..^3]:
        result.add "$const $type $name $expr $sep" % {
          "const": tern(s of ccLet, "const", ""),
          "name": def.toCpp(s),
          "type": typ,
          "expr": expr,
          "sep": tern(0 < idx, ",", "")
        }

    of nkEmpty:
      case s.context.top():
        of ccFunctionArg: result = "void"
        of ccType: result = "auto"
        else: result = ""

    of nkDiscardStmt:
      result = node[0].toCpp(s)

    of nkPostfix:
      result = node[1].toCpp(s)

    of nkAccQuoted:
      var id = ""
      for it in node:
        id.add it.toCpp(s)

      case s.top():
        of ccFunctionName:
          result = "operator $#" % id

        else:
          die(node)

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

    of nkTemplateDef:
      result = "/* TEMPLATE */"

    of nkObjConstr:
      result.add "$#()" % toCpp(node[0], s)
      for arg in node[1..^1]:
        result.add ".$#($#)" % [ toCpp(arg[0], s), toCpp(arg[1], s) ]

    of nkVarSection, nkLetSection:
      for sub in node:
        result.add "\n"
        result.add sub.toCpp(s + tern(node of nkVarSection, ccVar, ccLet))
        result.add ";"

    of nkProcDef, nkFuncDef, nkIteratorDef:
      var body = node[6].toCpp(s)
      var ret = node[3][0].toCpp(s + ccType)
      if node of nkIteratorDef:
        body = "std::vector<$#> result{}; $# return result;" % [ret, body]
        ret = "std::vector<$#>" % ret

      else:
        body = "$# result; $# return result;" % [ret, body]

      result = "$template\n$returnType $name($args) {$body}" % {
        "name": node[0].toCpp(s + ccFunctionName),
        "template": toCpp(node[2], s + ccGeneric),
        "returnType": ret,
        "body": body,
        "args": node[3][1..^1].mapIt(it.toCpp(s + ccFunctionArg)).join(", ")
      }

    of nkImportStmt:
      discard

    of nkDotExpr:
      result = "$#.$#" % [ toCpp(node[0], s), toCpp(node[1], s) ]

    of nkInfix:
      result = "$# $# $#" % [
        toCpp(node[1], s + ccExpression),
        toCpp(node[0], s + ccExpression),
        toCpp(node[2], s + ccExpression),
      ]

    of nkCall, nkCommand:
      let name = node[0].toCpp(s + ccExpression)
      if name == "sink":
        result = "const $#&" % node[1].toCpp(s)

      else:
        result = "$name($args)$end" % {
          "name": name,
          "args": mapIt(node[1..^1], toCpp(it, s + ccExpression)).join(", "),
          "end": tern(s of ccStandaloneStmt, ";", "")
        }


    of nkCurly, nkBracket:
      result = "{$#}" % [
        node.mapIt(it.toCpp(s)).join(", ")
      ]

    of nkTableConstr:
      result = "{"
      for item in node:
        let rhs = toCpp(item[^1], s + ccExpression)
        for lhs in item[0..^2]:
          result.add "{$#, $#}," % [ lhs.toCpp(s + ccExpression), rhs ]

      result.add "}"

    of nkBracketExpr:
      result = "${name}${op}${args}${cl}" % {
        "name": node[0].toCpp(s),
        "args": mapIt(node[1..^1], it.toCpp(s)).join(", "),
        "op": tern(s of {ccGeneric, ccType}, "<", "["),
        "cl": tern(s of {ccGeneric, ccType}, ">", "]"),
      }

    of nkCommentStmt:
      result = doc(node)

    else:
      die(node)

proc parse(s: string): PNode =
  var id = newIdentCache()
  var conf = newConfigRef()
  return parseString(s, id, conf)

let start = State(context: @[ccToplevel])
writeFile(
  "/tmp/res.cpp",
  parse(readFile(
    "/mnt/workspace/repos/hmisc/src/hmisc/algo/hlex_base.nim")).toCpp(start))
