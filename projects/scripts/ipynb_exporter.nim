import hargparse
import hashes
import re
import strutils
import shell
import hmisc/[helpers, defensive, hjson]
import colechopkg/lib
import strformat
import os
import parsetoml
import sequtils
import json
import base64
import md5
import options
import pandoc_parser


initDefense()

const
  testing = true
  defaultTemp = "/tmp/ipynb_exporter"
  defaultInput =
    when testing: defaultTemp
    else: "input"


var nocode = false
var hideerror = false

type
  NBCellKind = enum
    nbcMarkdown
    nbcCode

  NBCellDataKind = enum
    nboTextPlain
    nboImagePng
    nboTextHtml

  NBCellOutput = object
    dtype: NBCellDataKind
    content: string

  NBCell = object
    case kind*: NBCellKind
    of nbcMarkdown:
      text*: string
    of nbcCode:
      source*: string
      outputs*: seq[NBCellOutput]

  NBDocument = object
    cells*: seq[NBCell]


type
  LatexNodeKind = enum
    lnkPlaintext
    lnkMacroUse
    lnkEnviron
    lnkComment
    lnkNodeList

  LatexNode = object
    case kind*: LatexNodeKind
    of lnkNodeList:
      nodes: seq[LatexNode]
    of lnkPlaintext, lnkComment:
      text: string ## Plaintext string
    of lnkMacroUse, lnkEnviron:
      name: string ## Macro name
      optargs: seq[LatexNode] ## Optional arguments
      gargs: seq[LatexNode] ## Braced arguments
      body: seq[LatexNode] ## Body of the environment. Not used for macro

  LatexDocument = object
    preamble*: seq[LatexNode]
    content*: seq[LatexNode]

func wrapBrace(str: string, brace: char, padding: string = ""): string =
  let closeb: char =
    case brace:
      of '[': ']'
      of '{': '}'
      of '(': ')'
      of '<': '>'
      else: brace

  return brace & padding & str & padding & closeb

func toString(node: LatexNode): string =
  case node.kind:
    of lnkPlainText: node.text
    of lnkComment: "% " & node.text
    of lnkNodeList: node.nodes.map(toString).join("\n")
    of lnkEnviron, lnkMacroUse:
      let optargs = (node.optargs.len > 0).tern(
        node.optargs.mapIt(it.toString()).join(",").wrapBrace('['),
        "")

      let gargs = (node.gargs.len > 0).tern(
        node.gargs.mapIt(it.toString().wrapBrace('{')).join(""),
        "")

      if node.kind == lnkEnviron:
        let body = node.body.mapIt(it.tostring()).join("\n")
        """
  \begin{{{node.name}}}{optargs}{gargs}
  {body}
  \end{{{node.name}}}
  """
      else:
        """\{node.name}{optargs}{gargs}""""

func toString(doc: LatexDocument): string =
  result &= doc.preamble.mapIt(it.toString()).join("\n")
  result &= "\\begin{document}\n"
  result &= doc.preamble.mapIt(it.toString()).join("\n")
  result &= "\\end{document}\n"

func makeLtxPlaintext(text: string): LatexNode =
  LatexNode(
    kind: lnkPlaintext,
    text: text
  )

func makeLtxMacro(name: string, gargs, optargs: seq[string] = @[]): LatexNode =
  LatexNode(
    kind: lnkMacroUse,
    name: name,
    gargs: gargs.map(makeLtxPlaintext),
    optargs: optargs.map(makeLtxPlaintext)
  )

func makeLtxNodeList(nodes: seq[LatexNode]): LatexNode =
  LatexNode(
    kind: lnkNodeList,
    nodes: nodes
  )

func makeLtxEnviron(
  name: string,
  body: seq[LatexNode],
  gargs, optargs: seq[string] = @[]): LatexNode =

  LatexNode(
    kind: lnkEnviron,
    name: name,
    body: body,
    gargs: gargs.map(makeLtxPlaintext),
    optargs: optargs.map(makeLtxPlaintext)
  )

func markdownStringToLatex(str: string): LatexNode = makeLtxPlaintext(str)
func htmlStringToLatex(str: string): LatexNode = makeLtxPlaintext(str)

type Base64ImageSpec = tuple[path, content: string]

func toLatex(outp: NBCellOutput): tuple[
  node: LatexNode,
  encoded: Option[Base64ImageSpec]] =
  ## Convert single cell output to latex node. If output contains
  ## `image/png` it will be exported as base-64 string.
  case outp.dtype:
    of nboTextPlain:
      (node: makeLtxEnviron(
        name = "verbatim",
        body = @[makeLtxPlaintext(outp.content)]),
       encoded: none(Base64ImageSpec)
      )
    of nboTextHtml: (
      node: htmlStringToLatex(outp.content),
      encoded: none(Base64ImageSpec)
    )
    of nboImagePng:
      let path = $hash(outp.content) & ".png"
      let inclgr = makeLtxMacro(
        name = "includegraphics",
        optargs = @["width=0.5\\textwidth"],
        gargs = @[path]
      )

      (
        node: makeLtxEnviron(name = "center", body = @[inclgr]),
        encoded: some((path, outp.content))
      )

func toLatex(cell: NBCell): (LatexNode, seq[Base64ImageSpec]) =
  ## Convert notebook cell to latex node. All images will be expored
  ## as base64 with required paths.
  case cell.kind:
    of nbcMarkdown: (markdownStringToLatex(cell.text), @[])
    of nbcCode:
      let outputs = cell.outputs.map(toLatex)
      let res1 = makeLtxNodeList(@[
        makeLtxEnviron(
          name = "minted",
          gargs = @["python"],
          body = @[makeLtxPlaintext(cell.source)])
        ] &
          outputs.mapIt(it[0])
      )

      (res1, outputs.filterIt(it[1].isSome).mapIt(it[1].get()))

func toLatexDocument(notebook: NBDocument, title, author: string): LatexDocument =
  ## Conter notebook document to latex document. Required images will
  ## be exported seprateely as base64-encoded strings.
  result.preamble.add makeLtxMacro("documentclass", gargs = @["article"])

  let packages: seq[(seq[string], string)] = @[
    (@["T1", "T2A"], "fontenc"),
    (@["utf8"], "inputenc"),
    (@["english", "russian"], "babel"),
    (@[], "amsmath"),
    (@[], "amssymb"),
    (@[], "minted"),
    (@[], "graphicx"),
    (@[], "grffile"),
    (@[], "longtable"),
    (@[], "fancyhdr"),
    (@["margin=2cm"], "geometry")
  ]

  result.preamble.add packages.mapIt(
    makeLtxMacro(
      name = "usepackage",
      gargs = @[it[1]],
      optargs = it[0]
    ))

  let styleconf: seq[(string, string)] = @[
    ("pagestyle", "fancy"),
    ("fancyhf", ""),
    ("lhead", &"{title} {author}"),
    ("rhead", "\\today"),
    ("rfoot", "\\thepage"),
    ("author", &"{title} {author}"),
    ("title", &"{author}")
  ]

  result.preamble.add styleconf.mapIt(
    makeLtxMacro(it[0], gargs = @[it[1]])
  )

  result.content.add makeLtxMacro("maketitle")

  let cells = notebook.cells.map(toLatex)
  let images = cells.mapIt(it[1])

  result.content.add cells.mapIt(it[0])



proc getConfOrDie(conf: TomlValueRef, name: string): string =
  if not conf.hasKey(name):
    ceUserError0(&"Configuration file is missing {name}")
    die()
  else:
    conf[name].getStr()



proc anything(input: string, argument: var string, start: int): int =
  let diff = input.len - start
  argument = input[start..^1]
  return diff



  # if node.kind == JString:
  #   return node.getStr()
  # else:
  #   raise newException(ValueError, &"Json node of kind {node.kind} cannot be converter to string")


proc exportCellOutput(outp, cell: JsonNode): string =
  let outtype = outp["output_type"].asStr()
  let execcount = cell["execution_count"].asInt()

  defer:
    if result.contains("\\cfrac"):
      showWarn("Code cell contains \\cfrac. execcount:", execcount)

  if outtype == "stream":
    showLog("Found text stream")
    result = outp["text"].joinArr()
  elif outtype == "execute_result" or outtype == "display_data":
    let data = outp["data"]
    if data.hasKey("image/png"):
      let base64 = data["image/png"].asStr()
      let hash = toMD5(base64)
      let outf = "sfdf" & $hash & ".png"
      let file = outf.open(fmWrite)
      file.write(decode(base64))
      file.close()

      let (res, _, code) = shellVerboseErr:
        identify ($outf)

      var
        height: int
        width: int

      if res =~ re""".+ .+ (\d+)x(\d+).*""":
        width = matches[0].parseInt()
        height = matches[1].parseInt()
      else:
        echo res
        echo "not ok"
        die()


      result = """
\begin{center}
\includegraphics[width=$1in]{$2}
\end{center}
""" % [$(width / 100), getCurrentDir().joinPath(outf)]
    elif data.hasKey("text/latex"):
      let body = data["text/latex"].joinArr()
      result = body
    elif data.hasKey("text/html"):
      showLog("Found html table, converting to latex ...")
      let body = data["text/html"].joinArr()
      # "tmp.html".writeFile(body)
      # let (latex, err, code) = shellVerboseErr {dokCommand}:
      #   pandoc -f html -t latex "tmp.html" -o "table.tex"
      let latextable = htmlTableToLatex(body)

      result = """
% ---
\begin{center}
$1
\end{center}
% ---
""" % [latextable]
    else:
      result = """
% ---
\begin{verbatim}
$1
\end{verbatim}
% --- ---

""" % [outp["data"]["text/plain"].joinArr()] #TODO export html tables?

proc exportCodeCell(cell: JsonNode): string =
  if not nocode:
    let body = cell["source"].asSeq().mapIt(it.asStr()).join("")
    result.add """
  % ---
  \begin{minted}{python}[breaklines]
  $1
  \end{minted}
  % --- ---

  """ % [body]

  for res in cell["outputs"].asSeq():
    result.add "\n"
    result.add res.exportCellOutput(cell)

proc shellConfig(): set[DebugOutputKind] = {}


proc exportMarkdownCell(cell: JsonNode): string =
  let body = cell["source"].asSeq().mapIt(it.asStr()).join("")
  "file.md".writeFile(body)
  let (res, err, code) = shellVerboseErr shellConfig():
    pandoc -f markdown -t latex -o "file.tex" "file.md"

  if code != 0:
    ceUserError0("Error while converting to markdown")
    echo err
    die()

  result = "file.tex".readFile().string()

func getLatexHeader(author, group, task: string): string =
  return """
  % Created 2020-01-29 Wed 18:16
  % Intended LaTeX compiler: pdflatex
  \documentclass{article}

  \usepackage{hyperref}

  \hypersetup{colorlinks=true,linkcolor=blue}
  \usepackage[T1, T2A]{fontenc}
  \usepackage[utf8]{inputenc}
  \usepackage[english,russian]{babel}

  % \usepackage{cmap}

  \usepackage{amsmath}
  \usepackage{amssymb}
  \usepackage{minted}
  \usepackage{graphicx}
  \usepackage{grffile}
  \usepackage{longtable}

  \usepackage{fancyhdr}

  \usepackage[margin=2cm]{geometry}
  \pagestyle{fancy}
  \fancyhf{}
  \lhead{$1 $2}
  \rhead{\today}
  \rfoot{\thepage}

  \author{$1 $2}
  \title{$3}

  \begin{document}

  \maketitle
  """ % [author, group, task]

proc cdUp(): void =
  setCurrentDir(parentDir(getCurrentDir()))

proc logDir(): void =
  ceUserLog0(&"Current location is {getCurrentDir()}")

proc logCopyFile(src, dest: string): void =
  ceUserLog0(&"Copying {src} to {dest}")
  copyFile(src, dest)

proc convertFile(nbJson: JsonNode, author, group, task: string): void =
  let outName = "result_tex.tex"
  ceUserLog0(&"Writing result to file {outName}")
  let outFile = outName.open(fmWrite)

  outFile.write getLatexHeader(author, group, task)


  runIndentedLog():
    for cell in nbJson["cells"].asSeq():
      if cell["cell_type"].asStr() == "code":
        outFile.write exportCodeCell(cell)
      elif cell["cell_type"].asStr() == "markdown":
        outfile.write exportMarkdownCell(cell)


  outFile.write """
  % ---
  \end{document}
  """

  outFile.close()

  let namepref = &"{author}_{group}_{task}"

  var genFiles = "list.txt".open(fmWrite)

  block:
    showInfo("Compiling latex to pdf ...")
    showLog("Latex file:", outName.absolutePath())
    var isOk = safeRunCommand("pdf compilation", noShellMsg, true):
      echo "running compilation"
      latexmk "-pdf -latexoption=-shell-escape --interaction=nonstopmode" ($outName)
      echo "finished compilation"

    logCopyFile("result_tex.pdf", &"{namepref}.pdf")
    genFiles.write(&"{namepref}.pdf\n")


  block:
    var isOk = safeRunCommand("latex to odt conversion", noShellMsg, hideerror):
      pandoc "result_tex.tex" "-o" "result_tex.odt"

    if not isOk:
      die()


    logCopyFile("result_tex.odt", &"{namepref}.odt")

    isOk = safeRunCommand("odt to docx conversion", noShellMsg, hideerror):
      soffice "--headless" "--convert-to" docx "result_tex.odt"

    if not isOk:
      die()

    logCopyFile("result_tex.docx", &"{namepref}.docx")
    genFiles.write(&"{namepref}.docx\n")

  genFiles.close()


proc extractConfig(nbJson: JsonNode): Option[string] =
  for cell in nbJson["cells"].asSeq():
    if cell["cell_type"].asStr() == "raw":
      let src = cell["source"].asSeq().mapIt(it.asStr())
      if src[0].startsWith("#+begin_config") and
         src[^1].startsWith("#+end_config"):
        return some(src[1..^2].joinl())



proc processNotebook*(filepath: string, tmpdir: string, fromZip: bool): void =
  let (_, file) = filepath.splitPath()
  let targetfile = &"{tmpdir}/{file}"
  ceUserInfo0(&"Processing path {filepath}")
  ceUserLog0(&"File name is {file}")
  logDir()
  if targetfile == filepath:
    showLog("Target file is located in temporary directory")
  else:
    showLog("Temp directory is:", tmpDir)
    showLog("Cleaning up temporary directory")
    if existsDir(tmpDir):
      removeDir(tmpdir)

    createDir(tmpdir)
    logCopyFile(filepath, targetfile)

  setCurrentDir(tmpdir)
  logDir()

  if existsFile(file):
    ceUserInfo0(&"File copy ok")
  else:
    ceUserError0(&"Could not find {file}")

  if fromZip:
    shell:
      unzip ($file)

    ceUserLog0("Opened file")

  let notebook = findFirstFile("*.ipynb", "notebook")
  ceUserInfo0(&"Input notebook: {notebook}")

  showInfo("Parsing notebook file at ", notebook.absolutePath())
  let nbJson = json.parseFile(notebook)

  let config =
    if fromZip:
      let configuration =  findFirstFile("*.toml", "configuration file")
      ceUserInfo0(&"Input configuration: {configuration}")
      parsetoml.parseFile(configuration)
    else:
      iflet (conf = extractConfig(nbJson)):
        ceUserInfo0("Found configuration string")
        parsetoml.parseString(conf)
      else:
        ceUserError0("Could not find configuration in the notebook")
        die()

  let author = config.getConfOrDie("name")
  let group = config.getConfOrDie("group")
  let task = config.getConfOrDie("task")
  let namepref = &"{author}_{group}_{task}"

  convertFile(nbJson, author, group, task)
  logCopyFile(notebook, &"{namepref}.ipynb")

  cdUp()

  let resultfile = tmpdir & ".zip"
  shell:
    zip ($resultfile) -r ($tmpdir)


  # logCopyFile(&"{tmpdir}/{resultfile}", &"{resultfile}")

proc processWholeDir(inputDirectory: string, pref: string): void =
  ## Find all zipped notebooks in directory and process them
  ## one-by-one. Pack results back into zip archives

  ceUserLog0(&"Input directory '{inputDirectory}'")

  if not dirExists(inputDirectory):
    ceUserError0("Input directory does not exist")
    die()
  else:
    ceUserInfo0("Input directory exists")
    setCurrentDir(inputDirectory)

  logDir()
  for idx, zip in toSeq(walkPattern("*.zip")):
    ceUserInfo0(&"Found zip file {zip}")
    if not zip.contains("result"):
      processNotebook(zip, &"{pref}result_zip_{idx}", fromZip = true)

  for idx, notebook in toSeq(walkPattern("*.ipynb")):
    ceUserInfo0(&"Found notebook file {notebook}")
    processNotebook(notebook, &"{pref}result_notebook_{idx}", fromZip = false)

parseArgs:
  opt:
    name: "input-dir"
    opt: ["--in-dir", "--input", "+takes_values"]
    help: """Specify input directory. By default {defaultInput}
             is used"""
  opt:
    name: "nocode"
    opt: ["--nocode"]
    help: "Do not add code blocks to generated document"

  opt:
    name: "pref"
    opt: ["--pref", "+takes_values"]
    help: "result direcoty prefix"

  opt:
    name: "input-file"
    opt: ["--in-file", "--file", "+takes_values"]
    help: """Specify input file. Run only on it"""
  opt:
    name: "hideerror"
    opt: ["--hideerror"]
    help: "Do not show error messages from failed shell commands"
  opt:
    name: "temp-dir"
    opt: ["--temp-dir", "+takes_values"]
    help: "Directory to use for temporary files in single-file compilation"


if "hideerror".kp():
  ceUserWarn("""Error supression is enabled - failed shell
  commands will not show `stderr`""")
  hideerror = true

if "nocode".kp():
  showInfo "'--nocode' is selected: python code will not be included"
  nocode = true



if "input-dir".kp() or (not "input-file".kp()):
  # Process directory
  let inputDir = "input-dir".kp().tern(
    "input-dir".k.toStr(), defaultInput
  ).absolutePath()

  processWholeDir(inputDir, "pref".kp().tern("pref".k().toStr(), ""))
else:
  let inputFile =
    block:
      let tmp = "input-file".k.toStr().expandTilde()
      if tmp.isAbsolute(): tmp
      else: tmp.absolutePath()

  if not inputFile.existsFile():
    ceUserError0("No such file or directory: " & inputFile)
    die()
  else:
    let (_, filename) = inputFile.splitPath()
    ceUserInfo0(&"Found file {inputFile}, generating report ...")
    ceUserLog0(&"Output files will be places into 'res_{filename}.d'")
    processNotebook(
      inputFile,
      "temp-dir".kp().tern(
        "temp-dir".k().toStr(),
        &"res_{filename}.d"
      ),
      fromZip = false)
