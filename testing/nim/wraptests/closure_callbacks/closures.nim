proc main() =
  let val = 15
  let cb = proc(a: int): int =
    echo val

  block:
    let raw = cast[
      proc(a: int, env: pointer): int {.cdecl.}](cb.rawProc())

    let env = cb.rawEnv()

    discard raw(12, env)

  block:
    let raw = cast[proc(a: int): int {.cdecl.}](cb.rawProc())

    let env = cb.rawEnv()

    discard raw(12)


main()
