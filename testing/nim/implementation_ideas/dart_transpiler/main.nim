import
  htsparse/dart/dart,
  hmisc/other/oswrap,
  hnimast,
  fusion/matching

{.experimental: "caseStmtMacros".}


proc conv(n: DartNode, s: string): PNode =
  result = newPStmtList()
  case n.kind:
    of dartProgram:
      for sub in n:
        result.add conv(sub, s)

    of dartComment, dartDocumentationComment:
      discard

    of dartImportOrExport:
      discard

    of dartMethodSignature:
      result = conv(n[0], s)

    of dartFunctionSignature:
      result = nnkProcDef.newPTree()

    of dartClassDefinition:
      let name = s[n[0]]
      let n = n[1]

      var
        head: DartNode
        body: DartNode

      for idx, sub in n:
        if sub.kind in {dartMethodSignature}:
          head = sub

        elif sub.kind in {dartFunctionBody}:
          body = sub

          let
            main = head.conv(s)
            impl = body[0].conv(s)

        else:
          discard


    else:
      echo n.treeRepr(s)

proc convert(inFile, outFile: AbsFile) =
  let str = inFile.readFile()
  let node = parseDartString(str)

  outFile.writeFile($conv(node, str))

when isMainModule:
  convert AbsFile("/tmp/term.dart"), AbsFile("/tmp/term.nim")
