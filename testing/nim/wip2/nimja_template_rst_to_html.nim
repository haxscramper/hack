{.define(dumpNwtMacro).}

import pkg/nimja

import packages/docutils/[rst, rstast]

import
  hmisc/other/oswrap,
  hmisc/core/all

import std/[strformat]


proc parseRstString*(s: string): PRstNode =
  const filen = "input"
  var dummyHasToc = false
  let findFile = proc(path: string): string =
    echo "find file", path

  result = rstParse(
    s, filen, 0, 1, dummyHasToc, {},
    findFile = findFile,
    msgHandler = (
      proc(filename: string, line, col: int,
           msgkind: MsgKind, arg: string) =

        let mc = msgkind.whichMsgClass
        if mc == mcError:
          var message: string
          message.add &" {mc}: {arg}"
          raise newException(EParseError, message)))



template t(str: string): untyped =
  compileTemplateStr(str)

proc `[]`*(rst: PRstNode, idx: int): PRstNode = rst.sons[idx]

import hmisc/other/hpprint


proc toHtml*(rst: PRstNode): string =
  case rst.kind:
    of rnStrongEmphasis: t(
      "<b>{%for sub in rst.sons%}{{sub.toHtml()}}{%endfor%}</b>")

    of rnLeaf:
      result.add rst.text

    of rnInterpretedText:
      result.add "<code>"
      for inner in rst.sons:
        result.add toHtml(inner)

      result.add "</code>"

    of rnInner:
      for inner in rst.sons:
        result.add toHtml(inner)

    else:
      pprint rst
      raise newImplementKindError(rst)

type
  DocEntryKind = enum
    dekProc
    dekArg
    dekType
    dekModule


  DocEntry = ref object
    name: string
    docs: PRstNode
    case kind: DocEntryKind
      of dekModule:
        entries: seq[DocEntry]

      of dekProc:
        arguments: seq[DocEntry]

      of dekArg:
        argtype: DocEntry

      of dekType:
        discard

proc docProc(name: string, doc: PRstNode, args: openarray[DocEntry]): DocEntry =
  DocEntry(name: name, kind: dekProc, docs: doc, arguments: @args)

proc docArg(name: string, argtype: DocEntry, doc: PRstNode): DocEntry =
  DocEntry(name: name, kind: dekArg, docs: doc, argType: argType)

proc docType(name: string): DocEntry =
  DocEntry(name: name, kind: dekType)

proc docModule(name: string, doc: PRstNode, entries: openarray[DocEntry]): DocEntry =
  DocEntry(name: name, kind: dekModule, docs: doc, entries: @entries)

proc doc(str: string): PRstNode = parseRstString(str)

proc toHtml(entry: DocEntry): string =
  case entry.kind:
    of dekModule:
      t("""
<html>
<head>
  <title>{{entry.name}}</title>
</head>
<body>

<h1>{{entry.name}}</h1>

{{entry.docs.toHtml()}}

{% for sub in entry.entries %}
{{toHtml(sub)}}
{% endfor %}

</body>
</html>""")

    of dekProc:
      t("""
<h2>{{entry.name}}</h2>

<code>
<pre>
proc(
{% for arg in entry.arguments %}
    {{arg.name}}: {{arg.argtype.toHtml}} #[ {{arg.docs.toHtml()}} ]#
{% endfor %}
  ) =

</pre>
</code>

{{entry.docs.toHtmL()}}


""")

    of dekType:
      result = entry.name

    else:
      raise newImplementKindError(entry)

let module = docModule(
  "htree_mapping",
  doc(lit3"""
    Generic implementations of algorithms for working with tree-like data
    structures. Iterative and recursive DFS/BFS with order control, recursive
    tree mapping etc."""), [
      docProc(
        "concat",
        doc(lit3"""
          Do BFS iteration on recursive data type and map results into sequence"""),[
            docArg("topNode", docType"untyped", doc"First node in tree"),
            docArg("subNode", docType"untyped", doc"Expression to get subnodes"),
            docArg(
              "op",
              docType"untyped",
              doc"Expression to get result. If `typeof(op)` is `Option` `none` results are discarded")])])

let text = toHtml(module)
echo text

"/tmp/a.html".writeFile(text)
