import
  htsparse/java/java,
  hmisc/other/oswrap,
  hmisc/wrappers/treesitter,
  hmisc/hdebug_misc,
  hnimast

import
  std/[sequtils, strutils, options, sugar]

proc toNType(node: JavaNode, str: string): PNtype =
  case node.kind:
    of javaIdentifier, javaTypeIdentifier:
      result = newPType(str[node])

    of javaGenericType:
      let head = str[node[0]]

      case head:
        of "ArrayList":
          result = newPType("seq")

        else:
          result = newPType(head)


      for arg in node[1]:
        result.add toNType(arg, str)

    of javaPrimitiveTypes:
      result = newPType(
        case node.kind:
          of javaIntegralType: "int"
          of javaFloatingPointType: "float"
          of javaVoidType: "void"
          of javaBooleanType: "bool"
          else:
            raise newUnexpectedKindError(node)
      )

    of javaArrayType:
      let
        size = str[node["dimensions"]]
        element: PNType = toNType(node["element"], str)


      if size == "[]":
        result = newPType("seq", [element])

      elif size == "[][]":
        result = newPType("seq", [newPType("seq", [element])])

      elif size == "[][][]":
        result = newPType(
          "seq", [newPType(
            "seq", [newPType(
              "seq", [
                element])])])

      else:
        raise newImplementError(size)

    else:
      raise newImplementKindError(node, node.treeRepr(str, pathIndexed = true))

proc conv(
    node: JavaNode, str: string,
    parent: Option[PNType] = none(PNType)
  ): PNode =

  case node.kind:
    of javaComment, javaPackageDeclaration, javaImportDeclaration:
      result = newEmptyPNode()

    of javaIdentifier, javaThis:
      result = newPident(str[node])

    of javaFalse:
      result = newPIdent("false")

    of javaTrue:
      result = newPIdent("true")

    of javaPrimitiveTypes, javaTypeIdentifier:
      result = toNType(node, str).toNNode()

    of javaProgram, javaBlock, javaConstructorBody:
      result = newPStmtList()
      for sub in items(node):
        result.add conv(sub, str)

    of javaClassDeclaration:
      var
        res = newPObjectDecl(str[node["name"]])
        tmp = newPStmtList()

      for entry in node["body"]:
        case entry.kind:
          of javaFieldDeclaration:
            res.addField newObjectField(
              str[entry[2][0]],
              toNType(entry[1], str),
              value = if entry.has(2) and entry[2].has(1):
                        some conv(entry[2][1], str)

                      else:
                        none(PNode)
            )

          else:
            tmp.add conv(entry, str, some res.name)

      result = newPStmtList()
      result.add toNNode(res)
      result.add tmp

    of javaMethodDeclaration:
      var decl = newPProcDecl(
        name = str[node[2]],
        args = node[3].mapIt((str[it[1]], it[0].toNType(str))),
        impl = node[4].conv(str),
        returnType = some node[1].toNType(str))

      if parent.isSome():
        decl.addArgument("this", parent.get())

      result = decl.toNNode()

    of javaConstructorDeclaration:
      assert parent.isSome()
      var decl = newPProcDecl(
        name = "new" & str[node[1]],
        args = node[2].mapIt((str[it[1]], it[0].toNType(str))),
        impl = node[3].conv(str),
        returnType = parent
      )

      result = decl.toNNode()


    of javaLocalVariableDeclaration:
      let vartype = toNType(node[0], str)
      result = newPStmtList()
      for decl in node[1 ..^ 1]:
        assertKind(decl, javaVariableDeclarator)
        result = newVar(
          str[decl[0, javaIdentifier]],
          vartype,
          if decl.has(1):
            conv(decl[1], str)

          else:
            newEmptyPNode())

    of javaObjectCreationExpression:
      case node[0].kind:
        of javaIdentifier, javaTypeIdentifier:
          result = newPCall("new" & str[node[0]])

        of javaGenericType:
          let head = newXCall newBracketExpr(
            newPident(
              "new" & str[node[
                0, {javaGenericType}][
                  0, {javaTypeIdentifier}]]),
            node[0][1, {javaTypeArguments}].mapIt(conv(it, str)))

        else:
          raise newImplementError(node.treeRepr(str))

      for arg in node[1]:
        result.add conv(arg, str)



    of javaDecimalFloatingPointLiteral:
      result = str[node][0 .. ^2].parseFloat().newPLit()

    of javaDecimalIntegerLiteral:
      result = str[node].parseInt().newPLit()

    of javaNullLiteral:
      result = newPIdent("nil")

    of javaStringLiteral:
      result = newPLit(str[node])

    of javaExpressionStatement:
      assert node.len == 1
      result = conv(node[0], str)

    of javaMethodInvocation:
      if "object" in node:
        result = newPCall(str[node[1]])
        result.add conv(node[0], str)
        for arg in node[2]:
          result.add conv(arg, str)

      else:
        result = newPCall(str[node[0]])
        for arg in node[1]:
          result.add conv(arg, str)

    of javaEnhancedForStatement:
      result = nnkForStmt.newPTree(
        node[1].conv(str),
        node[2].conv(str),
        node[3].conv(str))

    of javaForStatement:
      result = newBlock(
        node["init"].conv(str),
        newWhile(
          node["condition"].conv(str),
          node["body"].conv(str),
          node["update"].conv(str)))

    of javaWhileStatement:
      result = newWhile(
        node["condition"].conv(str),
        node["body"].conv(str))


    of javaParenthesizedExpression:
      assert node.len() == 1
      result = conv(node[0], str)

    of javaAssignmentExpression:
      result = newAsgn(node[0].conv(str), node[1].conv(str))

    of javaFieldAccess:
      result = newDot(node[0].conv(str), node[1].conv(str))

    of javaArrayAccess:
      result = newPCall("[]", conv(node[0], str), conv(node[1], str))

    of javaBinaryExpression:
      result = newPCall(
        str[node{1}],
        conv(node{0}, str),
        conv(node{2}, str))

    of javaUnaryExpression:
      result = newPCall(str[node{0}], conv(node{1}, str))

    of javaReturnStatement:
      result = nnkReturnStmt.newPTree(conv(node[0], str))

    of javaArrayCreationExpression:
      case str[node["dimensions"]]:
        of "[]":
          result = nnkBracket.newTree():
            collect(newSeq):
              for sub in node[2]:
                conv(sub, str)

        else:
          result = newPCall(
            "newSeqWith",
            conv(node[1][0], str),
            newPCall("default", conv(node[0], str)))

    of javaCastExpression:
      result = nnkCast.newPTree(
        conv(node[0], str),
        conv(node[1], str))

    of javaArrayInitializer:
      result = nnkBracket.newPTree()
      for sub in node:
        result.add conv(sub, str)

    of javaTernaryExpression:
      result = newPar newPar newIfStmt(
        conv(node[0], str),
        conv(node[1], str),
        conv(node[2], str))

    of javaUpdateExpression:
      result = newPCall(
        case str[node{1}]:
          of "++": "inc"
          of "--": "dec"
          else: raise newUnexpectedKindError(str[node{1}]),
        conv(node{0}, str)
      )

    of javaIfStatement:
      result = newIfPStmt()
      case node.len:
        of 3:
          result.addBranch(conv(node[0], str), conv(node[1], str))
          result.addBranch(conv(node[2], str))

        of 2:
          result.addBranch(conv(node[0], str), conv(node[1], str))

        else:
          raise newImplementError(
            str[node] & "\n" & node.treeRepr(str, indexed = true))


    of javaSwitchExpression:
      result = newCaseStmt(conv(node[0], str))
      for label in node[1]:
        var sub: seq[PNode]
        for stmt in label[1 .. ^1]:
          sub.add conv(stmt, str)

        if label[0].len == 0:
          result.addBranch(sub)

        else:
          result.addBranch(
            conv(label[0, {javaSwitchLabel}][0], str),
            sub)

    else:
      raise newImplementKindError(
        node, "\n" & str[node], node.treeRepr(
          str, pathIndexed = true, maxDepth = 5,
          unnamed = false))



let str = "tmp.java".readFile()

startHax()
echo str.parseJavaString().conv(str)
