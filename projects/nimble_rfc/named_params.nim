import
  hnimast

import
  hmisc/core/[all, code_errors, colored],
  hmisc/types/colorstring

import
  std/[sequtils, tables, strformat, sets]

import
  hmisc/other/oswrap

type
  ProcInfo = object
    file: RelFile
    info: TLineInfo
    args: seq[string]

var
  cnt = 0
  overloaded = 0


proc registerDir(dir: AbsDir) =
  var
    table: Table[string, Table[seq[string], seq[ProcInfo]]]
    sigSet: OrderedSet[(string, seq[string])]


  proc register(node: PNode, file: RelFile) =
    case node.kind:
      of nkStmtList:
        for sub in node:
          register(sub, file)

      of nkProcDeclKinds:
        let decl = parseProc(node)

        if not isEmptyNode(decl.impl):
          let signames = mapit(decl.argumentTypes().mapIt(it.skip()), $it)
          if decl.name notin table:
            table[decl.name] = initTable[seq[string], seq[ProcInfo]]()

          if signames notin table[decl.name]:
            table[decl.name][signames] = @[]

          table[decl.name][signames].add(ProcInfo(
            file: file,
            info: decl.declNode.get().getInfo(),
            args: decl.argumentNames()))

          if table[decl.name][signames].len > 1:
            let info = table[decl.name][signames]


            var namegroup = info.groupByIt(it.args)
            if namegroup.len > 1:
              if (decl.name, signames) in sigSet:
                echo " ! already started overload group".toRed()

              else:
                sigSet.incl (decl.name, signames)
                echo " @ new overload group".toGreen()


              echo " > ", decl.name.toCyan(), " ", signames
              for group in mitems(namegroup):
                for item in mitems(group):
                  echo "   > ", item.args, " :: ", toLink(
                    (getStr(cwd() /. item.file), item.info.line.int, item.info.col.int),
                    &"{item.file}:{item.info.line}:{item.info.col}")

          inc cnt


      else:
        discard

  for file in walkDir(dir, RelFile, recurse = true, exts = @["nim"]):
    if globalTick() > 10_000:
      break

    elif "tests/" in file:
      discard

    else:
      let node = parsePNodeStr(readFile(dir / file), doRaise = false)
      if not isNil(node):
        try:
          register(node, file)

        except Exception as e:
          pprintStacktrace(e)

  if sigSet.len() > 0:
    echo " ## ", AbsFile(dir.string).splitFile().name.toRed()
    for (name, args) in sigSet:
      inc overloaded
      echo " + ", name.toCyan(), " ", args


for dir in walkDir(cwd() / "main/packages", AbsDir):
  if dir.name() notin @["bluu", "netwatch", "gcplat"]:
    echo " ~ ", dir.name()
    registerDir(dir)


echo &"""
Total procedures processed:  {cnt}
With name-based overloading: {overloaded}
"""
