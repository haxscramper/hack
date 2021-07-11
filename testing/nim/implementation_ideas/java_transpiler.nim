import
  htsparse/java/java,
  hmisc/other/oswrap,
  hmisc/wrappers/treesitter,
  hnimast

import
  std/[sequtils, strutils, options]

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

    of javaIntegralType:
      result = newPType("int")

    of javaFloatingPointType:
      result = newPType("float")

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

    of javaIntegralType:
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
              value = if isNil(entry[2][1]):
                        none(PNode)
                      else:
                        some conv(entry[2][1], str)
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
      result = newVar(
        str[node[1][0]],
        toNType(node[0], str),
        conv(node[1][1], str)
      )

    of javaObjectCreationExpression:
      result = newPCall("new" & str[node[0]])
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
      echo node.treeRepr(str)
      echo str[node]
      result = newPCall(str[node[1]])
      result.add conv(node[0], str)
      for arg in node[2]:
        result.add conv(arg, str)

    of javaEnhancedForStatement:
      result = nnkForStmt.newPTree(
        node[1].conv(str),
        node[2].conv(str),
        node[3].conv(str)
      )


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

    of javaReturnStatement:
      result = nnkReturnStmt.newPTree(conv(node[0], str))

    of javaArrayCreationExpression:
      result = newPCall(
        "newSeqWith",
        conv(node[1][0], str),
        newPCall("default", conv(node[0], str)))

    of javaIfStatement:
      case node.len():
        of 1:
          result = nnkIfStmt.newPTree(
            nnkElifBranch.newPTree(
              conv(node[0], str),
              conv(node[1], str)))

        of 2:
          result = nnkIfStmt.newPTree(
            nnkElifBranch.newPTree(
              conv(node[0], str),
              conv(node[1], str)))

        else:
          raise newImplementError(str[node] & "\n" & node.treeRepr(str, indexed = true))



    else:
      raise newImplementKindError(
        node, "\n" & str[node], node.treeRepr(str, indexed = true))



let str = "tmp.java".readFile()

echo str.parseJavaString().conv(str)
