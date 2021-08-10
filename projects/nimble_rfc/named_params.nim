import
  hnimast

import
  hmisc/core/[all, code_errors, colored],
  hmisc/types/colorstring,
  hmisc/algo/clformat

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

const
  maxFiles = 10_000

type
  SigTable = Table[string, Table[seq[string], seq[ProcInfo]]]
  SigSet = OrderedTable[(string, seq[string]), ColoredText]


proc print(dir: AbsDir, table: SigTable, sigSet: SigSet) =
  if sigSet.len() > 0:
    echo " ## ", dir.name().toRed()
    for key, buf in sigSet:
      inc overloaded
      echo buf

proc registerDir(dir: AbsDir) =
  proc register(
      node: PNode, file: RelFile,
      table: var SigTable, sigSet: var SigSet) =
    case node.kind:
      of nkStmtList:
        for sub in node:
          register(sub, file, table, sigSet)

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
              var buf: ColoredText
              # if (decl.name, signames) in sigSet:
              #   buf.add "   ! already started overload group\n".toRed()

              # else:
              #   buf.add "   @ new overload group\n".toGreen()


              buf.add "   > "
              buf.add decl.name.toCyan()
              buf.add " "
              buf.add signames.hshow()
              buf.add "\n"
              for group in mitems(namegroup):
                for item in mitems(group):
                  buf.add "     > "
                  buf.add item.args.hshow()
                  buf.add " :: "
                  buf.add &"{item.file}:{item.info.line}:{item.info.col}\n"

              sigSet[(decl.name, signames)] = buf


          inc cnt


      else:
        discard

  if false: # As package
    var
      table: SigTable
      sigSet: SigSet

    for file in walkDir(dir, RelFile, recurse = true, exts = @["nim"]):
      if globalTick() > maxFiles:
        break

      elif "tests/" in file:
        discard

      else:
        let node = parsePNodeStr(readFile(dir / file), doRaise = false)
        if not isNil(node):
          try:
            register(node, file, table, sigSet)

          except Exception as e:
            pprintStacktrace(e)

    print(dir, table, sigSet)

  else:
    for file in walkDir(dir, RelFile, recurse = true, exts = @["nim"]):
      if globalTick() > maxFiles:
        break

      elif "tests/" in file:
        discard

      else:
        let node = parsePNodeStr(readFile(dir / file), doRaise = false)
        if not isNil(node):
          try:
            var
              table: SigTable
              sigSet: SigSet

            register(node, file, table, sigSet)
            print(dir, table, sigSet)

          except Exception as e:
            discard
            # pprintStacktrace(e)


import std/terminal

for dir in walkDir(cwd() / "main/packages", AbsDir):
  if dir.name() notin @["bluu", "netwatch", "gcplat"]:
    registerDir(dir)

registerDir(cwd() / "main/packages/nim-sys")



echo &"""
Total procedures processed:  {cnt}
With name-based overloading: {overloaded}
"""
