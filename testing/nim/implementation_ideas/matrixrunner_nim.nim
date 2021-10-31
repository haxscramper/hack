import hmisc/preludes/cli_app

type
  NimRunFlag = enum
    nrfStrictFuncs = "strictFuncs"

  NimGc = enum
    ngcArc = "arc"
    ngcOrc = "orc"
    ngcRefc = "refc"

  NimBackend = enum
    nbC = "c"
    nbJs = "js"
    nbCpp = "cpp"

  NimRun = object
    flags: set[NimRunFlag]
    gc: NimGc
    backend: NimBackend
    file: AbsFile
    outFile: AbsFile
    tmpDir: AbsDir

proc makeCmd(conf: NimRun): ShellCmd =
  var cmd = shellCmd(nim)

  cmd.arg "compile"
  for flag in conf.flags:
    cmd.flag $flag

  with cmd:
    opt("gc", $conf.gc)
    opt("backend", $conf.backend)
    opt("out", $conf.outfile)
    opt("unitsep", "on")

  cmd.arg conf.file


  return cmd

let dir = getAppTempDir()

mkDir dir

var runs: seq[(ShellCmd, NimRun)]

for idx in 0 .. 2:
  let file = dir /. &"file{idx}.nim"
  let run = NimRun(
    file: file,
    outFile: file.withExt("out")
  )

  writeFile(file, """
static:
  echo "test"

proc gen1[A](a: A) = a + 123
proc gen2[A](a: A) = gen1(1)
proc gen3[A](a: A) = gen2(a)

gen3("1231")
""")

  runs.add(makeCmd(run), run)

startHax()

func hshow(run: NimRun, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  with result:
    add hshow(run.gc)
    add ", "
    add hshow(run.backend)
    add " "
    add hshow(run.flags)

func `$`*(run: NimRun): string = $hshow(run)



proc reportFail(res: ShellResult, run: NimRun) =
  echov run
  echov res.cmd.prettyShellCmd()
  for part in res.getStderr().split("\31"):
    echo "[", part, "]"

for (res, run) in runShellResult(runs):
  if res.isOk():
    echov run, "ok"

  else:
    reportFail(res, run)
