import shell
import strutils
import net
import posix

import libssh2

type
  SSHError = ref object of CatchableError
    rc: int

  SSHConnection = object
    session: Session
    socket: Socket
    channel: Channel

proc shutdown(c: SSHConnection) =
  discard c.session.sessionDisconnect("Normal shutdown, thank you for playing")
  discard c.session.sessionFree()
  c.socket.close()
  libssh2.exit()
  quit()

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

  var rc = knownHosts.knownHostReadfile("dummy_known_hosts", LIBSSH2_KNOWNHOST_FILE_OPENSSH)

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


iterator sshResultStdout(ssc: var SSHConnection): string =
  var rc = 0
  while true:
    var buffer: array[0..1024, char]
    rc = ssc.channel.channelRead(addr buffer, buffer.len)
    if rc > 0:
      yield buffer[0 ..< rc].join()
    if rc == LIBSSH2_ERROR_EAGAIN:
      discard waitsocket(ssc.socket.getFd(), ssc.session)
    else:
      break

iterator sshResultStderr(ssc: var SSHConnection): string =
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


proc main() =
  let
    username = "test"
    password = "test"
    hostname = "127.0.0.1"

  "/tmp/test.sh".writeFile """#!/usr/bin/env bash
echo 'Script stdout'
echo 'Script stderr' 1>&2
"""

  shell:
    chmod +x "/tmp/test.sh"

  var ssc = sshInit(hostname = hostname)

  ssc.sshHandshake()

  ssc.sshKnownHosts(hostname = hostname)
  ssc.sshAuth(
    username = username,
    password = password
  )

  ssc.sshOpenChannel()
  ssc.sshExecCommand("/tmp/test.sh")

  echo "--- stdout ---"
  for buf in ssc.sshResultStdout():
    echo buf

  echo "--- stderr ---"
  for buf in ssc.sshResultStderr():
    echo buf

  let (exitcode, exitsignal) = ssc.sshCommandGetExit()

  if not exitSignal.isNil:
    echo "Got sinal: ", exitSignal
  else:
    echo "EXIT: ", exitcode


main()

echo "done"
