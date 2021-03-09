import macros, strformat

func typedArgs*(call: NimNode): seq[NimNode] =
  for arg in call[1..^1]:
    case arg.kind:
      of nnkHiddenStdConv:
        case arg[1].kind:
          of nnkBracket:
            for elem in arg[1]:
              case elem.kind:
                of nnkHiddenCallConv:
                  result.add elem[1]
                else:
                  result.add elem
          else:
            raiseAssert(&"#[ IMPLEMENT for kind {arg[1].kind} ]#")
      else:
        raiseAssert(&"#[ IMPLEMENT for kind {arg.kind} ]#")


macro logcall*(inCall: typed): untyped =
  var args = newCall("echo")
  args.add inCall[0].toStrLit()
  args.add newLit("(")

  for arg in inCall.typedArgs():
    if args.len > 3:
      args.add newLit(", ")

    case arg.kind:
      of nnkIdent, nnkSym:
        args.add newLit(arg.toStrLit().strVal() & " = ")
        args.add nnkPrefix.newTree(ident "$", arg)
      else:
        args.add arg

  args.add newLit(")")

  result = quote do:
    `args`
    `inCall`

when isMainModule:
  let val = 12
  logcall echo("12", val)
  logcall 12.echo
  logcall val.echo
  logcall [1,2,3,3].echo
