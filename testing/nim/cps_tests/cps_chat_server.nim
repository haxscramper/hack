import
  cps,
  std/[net],
  sys/ioqueue

type
  State = ref object
    server: Socket
    clients: seq[Socket]

proc accept(server: Socket): Socket = new(result); server.accept(result)

proc process(state: State) {.cps: Continuation.} =
  let client = state.server.accept()
  state.clients.add client

  while true:
    let line = client.recvLine()
    if line.len == 0:
      break

    for c in state.clients:
      c.send("> " & line & "\n")

proc main(state: State) {.cps: Continuation.} =
  state.server = newSocket()
  state.server.setSockOpt(OptReuseAddr, true)
  state.server.bindAddr(Port(12345))
  state.server.listen()
  process(state)


var cont = whelp main(State())
cont = trampoline cont
