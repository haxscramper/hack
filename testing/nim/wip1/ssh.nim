import shell
import bitops
import strutils
import sequtils
import net
import posix
import streams
import osproc
import os

import libssh2

type
  SSHError* = ref object of CatchableError
    rc*: int

  SSHConnection* = ref object
    session*: Session
    socket*: Socket
    channel*: Channel

    outbuf: string
    errbuf: string

  RemoteProcess = object
    connection*: SSHConnection
    exitStatus*: cint
    inStream*, outStream*, errStream*: Stream

  RemoteStream* = ref object of Stream
    conn*: SSHConnection

  ShellProcKind* = enum
    spkRemote
    spkLocal

  ShellProcess* = object
    case kind*: ShellProcKind
    of spkRemote:
      rproc*: RemoteProcess
    of spkLocal:
      lproc*: Process

  ShellCommand* = object
    cmdString*: string
    case kind*: ShellProcKind
    of spkLocal:
      nil
    of spkRemote:
      user*: string
      passwd*: string
      # TODO add support for different types of credentials

# proc startPorcess(cmd: ShellCommand): ShellProces =
#   case cmd.kind:
#     of spkLocal:
#       ShellPorcess(
#         kind: spkLocal,
#         lproc: startProcess(cmd.cmdString)
#       )
#     of spkRemote:


proc waitsocket(socket_fd: SocketHandle, s: Session): int =
  var timeout: Timeval
  var fd: TFdSet
  var writefd: TFdSet
  var readfd: TFdSet
  var dir: int

  timeout.tv_sec = 10.Time
  timeout.tv_usec = 0

  FD_ZERO(fd)
  FD_SET(socket_fd, fd)

  dir = s.sessionBlockDirections()

  if((dir and LIBSSH2_SESSION_BLOCK_INBOUND) == LIBSSH2_SESSION_BLOCK_INBOUND):
    readfd = fd

  if((dir and LIBSSH2_SESSION_BLOCK_OUTBOUND) == LIBSSH2_SESSION_BLOCK_OUTBOUND):
    writefd = fd

  var sfd  = cast[cint](socket_fd) + 1

  result = select(sfd, addr readfd, addr writefd, nil, addr timeout);


proc noMoreData(
  conn: var SSHConnection,
  isErr: static[bool] = false): bool =
  ## Return true if no more data can be read from a connection
  ## channel. This might be due to several reasons: ssh error or
  ## process has terminated. Testing is done separately on `stderr`
  ## and `stdout` for a channel.
  when isErr:
    if conn.errbuf.len > 0: return false
  else:
    if conn.outbuf.len > 0: return false

  while true:
    var buf: array[1024, char]
    let rc =
      when isErr:
        conn.channel.channelReadStderr(addr buf, sizeof buf)
      else:
        conn.channel.channelRead(addr buf, sizeof buf)

    if rc == 0:
      # echo "\tssh no more data to read from ", (if isErr: "err" else: "out")
      return true
    elif rc == LIBSSH2_ERROR_EAGAIN:
      # echo "\tssh reading again, rc: " & $rc
      discard waitsocket(conn.socket.getFd(), conn.session)
    elif rc > 0:
      # echo "\tssh has data to read, appending test buffer, rc: ",
      #    rc, " recieved size: "

      when isErr:
        conn.errbuf &= buf[0 ..< rc].join()
      else:
        conn.outbuf &= buf[0 ..< rc].join()

      return false
    else:
      # echo "\tssh read returned error " & $rc
      return true

proc processOutput(rproc: var RemoteProcess, err: static[bool] = false): RemoteStream =
  new(result)
  result.conn = rproc.connection

  result.atEndImpl =
    proc(s: Stream): bool =
      var conn = (cast[RemoteStream](s)).conn
      conn.noMoreData()

  result.readDataImpl =
    proc(s: Stream, buffer: pointer, buflen: int): int =
      var conn = (cast[RemoteStream](s)).conn

      when err:
        if conn.errbuf.len > 0:
          echo "\tcan read stderr from buffer"
      else:
        # echo "\treading from stdout"
        if conn.outbuf.len == 0 and conn.noMoreData():
          echo "buffer is empty and no data can be read"
          return 0

        let toRead = min(conn.outbuf.len, buflen)
        copymem(buffer, conn.outbuf.cstring, toRead)
        conn.outbuf = conn.outbuf[toRead..^1]
        return toRead

  result.readLineImpl =
    proc(s: Stream, line: var TaintedString): bool =
      var conn = (cast[RemoteStream](s)).conn
      when err:
        discard

      else:
        while not conn.noMoreData():
          let eol = conn.outbuf.find({'\0', '\L', '\c', '\n'})
          # echo conn.outbuf.mapIt(it)
          # echo "eol is: ", eol
          if eol != -1:
            line = conn.outbuf[0 ..< eol]
            conn.outbuf = conn.outbuf[eol + 1 .. ^1]
            return true

        line = conn.outbuf
        conn.outbuf = ""

proc readLine(s: RemoteStream): string =
  discard s.readLine(result)

proc outputStream(rproc: var RemoteProcess): RemoteStream =
  rproc.processOutput(false)

proc errorStream(rproc: var RemoteProcess): RemoteStream =
  rproc.processOutput(true)


proc shutdown(c: SSHConnection) =
  discard c.session.sessionDisconnect("Normal shutdown, thank you for playing")
  discard c.session.sessionFree()
  c.socket.close()
  libssh2.exit()
  quit()


proc sshInit(hostname: string, port: int = 22): SSHConnection =
  ## Init ssh library, create new socket, init new ssh session
  var rc = init(0)
  if rc != 0:
    raise SSHError(
      msg: "libssh2 initialization failed",
      rc: rc
    )

  result = SSHConnection(
    socket: newSocket(),
    session: sessionInit()
  )

  result.socket.connect(hostname, Port(port))
  result.session.sessionSetBlocking(0)


proc sshHandshake(c: var SSHConnection): void =
  var rc = 0
  while true:
    rc = c.session.sessionHandshake(c.socket.getFd())
    if rc != LIBSSH2_ERROR_EAGAIN:
      break

  if rc != 0:
    raise SSHError(
      msg: "failure establing ssh connection",
      rc: rc
    )

proc sshKnownHosts(ssc: var SSHConnection, hostname: string): void =
  var knownHosts = ssc.session.knownHostInit()
  if knownHosts.isNil:
    ssc.shutdown()

  var rc = knownHosts.knownHostReadfile(
    joinpath(getHomeDir(), ".ssh/known_hosts"),
    LIBSSH2_KNOWNHOST_FILE_OPENSSH)

  if rc < 0:
    raise SSHError(
      msg: "Read knownhost error: " & $rc,
      rc: rc
    )

  var length: int
  var typ: int

  var fingerprint = ssc.session.sessionHostkey(length, typ)
  if fingerprint.isNil:
    ssc.shutdown()
    raise SSHError(msg: "Unable to fetch hostkey")

  var host: knownhost_st
  let check = knownHosts.knownHostCheckP(
    hostname,
    22,
    fingerprint,
    length,
    LIBSSH2_KNOWNHOST_TYPE_PLAIN or LIBSSH2_KNOWNHOST_KEYENC_RAW or LIBSSH2_KNOWNHOST_KEY_SSHRSA,
    addr host
  )

  # echo "Host check: ",
  #     check, " key: ",
  #     if check <= LIBSSH2_KNOWNHOST_CHECK_MISMATCH: host.key else: "<none>"

  rc = knownHosts.knownHostAddC(
    hostname,
    nil,
    fingerprint,
    length,
    nil,
    0,
    LIBSSH2_KNOWNHOST_TYPE_PLAIN or LIBSSH2_KNOWNHOST_KEYENC_RAW or LIBSSH2_KNOWNHOST_KEY_SSHRSA,
    nil)

  if rc != 0:
    raise SSHError(
      msg: "Failed to add knownhost: " & $rc,
      rc: rc
    )

  knownHosts.knownHostWritefile("dummy_known_hosts", LIBSSH2_KNOWNHOST_FILE_OPENSSH)
  knownHosts.knownHostFree()

# TODO separate into two functions: public key OR password. Do not
# implicitly mix two different authentification methods.
proc sshAuth(
  ssc: var SSHConnection,
  username: string,
  pubkeyFile: string = "~/.ssh/id_rsa.pub",
  privkeyFile = "~/.ssh/id_rsa",
  password: string = "",
     ): void =
  var rc = 0
  if password.len > 0:
    while true:
      rc = ssc.session.userauthPassword(username, password, nil)
      if rc != LIBSSH2_ERROR_EAGAIN:
        break

    if rc != 0:
      ssc.shutdown()

      raise SSHError(
        msg: "Authentication by password failed!",
        rc: rc
      )

  else:
    while true:
      rc = ssc.session.userauthPublickeyFromFile(username, pubkeyFile, privkeyFile, password)
      if rc != LIBSSH2_ERROR_EAGAIN:
        break

    if rc != 0:
      ssc.shutdown()
      raise SSHError(
        msg: "Authentication by public key failed!",
        rc: rc
      )

proc sshOpenChannel(ssc: var SSHConnection): void =
  var rc = 0
  # var channel: Channel
  while true:
    ssc.channel = ssc.session.channelOpenSession()
    if ssc.channel.isNil and ssc.session.sessionLastError(nil, nil, 0) == LIBSSH2_ERROR_EAGAIN:
      discard waitsocket(ssc.socket.getFd(), ssc.session)
    else:
      break

  if ssc.channel.isNil:
    echo "Unable to open a session"
    ssc.shutdown()

proc sshExecCommand(ssc: var SSHConnection, command: string): void =
  var rc = 0
  while true:
    rc = ssc.channel.channelExec(command)
    if rc != LIBSSH2_ERROR_EAGAIN:
      break

  if rc != 0:
    # TODO Report error command and other necessary things to process
    # them on higher levels.
    ssc.shutdown()
    raise SSHError(
      msg: "SSH Failed to execute command",
      rc: rc
    )


# iterator sshResultStdout(ssc: var SSHConnection): string {.closure.} =

iterator sshResultStderr(ssc: var SSHConnection): string {.closure.} =
  var rc = 0
  while true:
    var buffer: array[0..1024, char]
    rc = ssc.channel.channelReadStderr(addr buffer, buffer.len)
    if rc > 0:
      yield buffer[0 ..< rc].join()
    if rc == LIBSSH2_ERROR_EAGAIN:
      discard waitsocket(ssc.socket.getFd(), ssc.session)
    else:
      break




proc sshCommandGetExit(
  ssc: var SSHConnection): tuple[code: int, signal: cstring] =
  var rc = 0
  var  exitcode = 127
  while true:
    rc = ssc.channel.channelClose()
    if rc == LIBSSH2_ERROR_EAGAIN:
      discard waitsocket(ssc.socket.getFd(), ssc.session)
    else:
      break

  var exitsignal: cstring

  if rc == 0:
    exitcode = ssc.channel.channelGetExitStatus()
    discard ssc.channel.channelGetExitSignal(exitSignal, 0, nil, 0, nil, 0)


  discard ssc.channel.channelFree()


proc startProcess*(ssc: var SSHConnection, command: string): RemoteProcess =
  ssc.sshExecCommand(command)

  return RemoteProcess(
    connection: ssc
  )





proc main() =
  let
    username = "ssh-test-user"
    password = "ssh-password"
    hostname = "127.0.0.1"

  var ssc = sshInit(hostname = hostname)
  ssc.sshHandshake()

  ssc.sshKnownHosts(hostname = hostname)
  ssc.sshAuth(
    username = username,
    password = password
  )

  ssc.sshOpenChannel()



  var rproc = ssc.startProcess("/bin/ls /")

  while not rproc.outputStream().atEnd():
    echo "-> ", rproc.outputStream().readLine()


main()

echo "done"
