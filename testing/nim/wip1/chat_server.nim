import asyncnet, asyncdispatch, strutils

var clients {.threadvar.}: seq[AsyncSocket]

proc processClient(client: AsyncSocket) {.async.} =
  # This function does not return anything. When launched
  # asyncrhonously it's return type is
  while true:
    # Read one line of data from socket
    let line = await client.recv(1024 * 32)
    # If line is empty stop processing client
    if line.len == 0:
      break

    echo "Recieved line: ", line

    let doDisconnect = line.contains("/disconnect")

    # Index of _current_ client to differentiate between various
    # users.
    var senderIdx: int
    for idx, c in clients:
      if c == client:
        senderIdx = idx

    # For each connected client
    for c in clients:
      # Unless it is the same client
      if c != client:
        if doDisconnect:
          await c.send("| - client: " & $senderIdx & " has disconnect")
        else:
          # Send line to it
          await c.send("| <" & $senderIdx & "> " & line & "\c\L")

    if doDisconnect:
      break

proc serve() {.async.} =
  clients = @[]
  # Initialize new socket
  var server = newAsyncSocket()
  server.setSockOpt(OptReuseAddr, true)
  # Connect to port
  server.bindAddr(Port(12345))
  server.listen()
  echo "Started server"

  while true:
    let client = await server.accept()
    echo "New connection"
    clients.add client

    # Raise error if processClient returns future with error
    asyncCheck processClient(client)
    echo "Done connecting"

asyncCheck serve()
echo "Launched serve"
runForever()
