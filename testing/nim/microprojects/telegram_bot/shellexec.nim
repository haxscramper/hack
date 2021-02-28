import asynchttpserver, asyncdispatch
import shell

var server = newAsyncHttpServer()
proc cb(req: Request) {.async.} =
  # # let isOk = safeSh
  # shell:
  #   echo "test"

  await req.respond(Http200, "Hello World")

waitFor server.serve(Port(8080), cb)
