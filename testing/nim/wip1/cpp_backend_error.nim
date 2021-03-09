type
  LytConsole = ref object of RootObj
    text*: string



proc main() =
  let closure = proc(zzz: var LytConsole) =
    echo zzz[]

  var hello = LytConsole()

  closure(hello)


main()
