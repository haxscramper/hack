import hargparse
import macros
import options
import colechopkg/lib
import net

parseArgs:
  opt:
    name: "fsm-socket"
    opt: ["--socket", "+takes_value"]
    parseto: int
    help: "Specify socket for fsm-build"
  opt:
    name: "add-relative-file"
    opt: ["--add-relative-file", "+takes_value"]
    help: "Add file by it's relative path to fsm-build"

if not "fsm-socket".kp:
  ceUserError0("fsm-socket is missing")
  quit(1)

if "add-relative-file".kp:
  let file: string = "add-relative-file".k.toStr()
  ceUserInfo0("Adding relative file")
  var socket = newSocket()
  socket.connect("127.0.0.1", Port("fsm-socket".k.toInt()))
  ceUserInfo0("Sending file " & file)
  #socket.send(file)
  socket.close()
