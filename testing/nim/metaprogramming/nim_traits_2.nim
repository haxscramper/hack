import macros

dumpTree:
  type
    TraitParam*[T] = object
      whatever: void

    NimTrait = object
      name: string
      params: seq[TraitParam]

when false:
  dumpTree:
    proc parseTraitParam(nodes: seq[NimNode]): seq[TraitParam] = #[ IMPLEMENT ]# discard

    proc parseTraits(node: NimNode): seq[NimTrait] =
      assert node.kind == nnkBracket
      for element in node.children:
        let trait: NimTrait =
          convertAst(NimTrait):
            rule:
              patt: Ident
              oupt: NimTrait(name: $captured)
            rule:
              patt: Call([[callName, kind: Ident]], [[*arguments]])
              outp: NimTrait(name: $callName, params: arguments.parseTraitParam())

    proc generateImplFor_Hash(obj: TypeDecl): ProcDecl =
      let traits = obj.pragma.parseTraits()
      # Do some code generation, return `ProcDecl`
