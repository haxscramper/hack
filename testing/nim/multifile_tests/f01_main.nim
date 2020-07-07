import f01_for_loop_macro

when false:
  # does not work without enabling for loop macros in imported module.
  # {.experimental: "forLoopMacros".}

  # without wrapping the macro in a block, we'd need to choose different
  # names for `a` and `b` here to avoid redefinition errors
  for a, b in enumerate([1, 2, 3, 5]):
    echo a, " ", b

when true:
  template loopMacro(body: untyped): untyped =
    {.push experimental("forLoopMacros").}
    body
    {.pop experimental("forLoopMacros").}

  loopMacro:
    for a, b in enumerate([1, 2, 3, 5]):
      echo a, " ", b
