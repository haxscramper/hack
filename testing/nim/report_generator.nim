import parsetoml
import colechopkg/lib
import strformat
import strutils
import sequtils
import hmisc/helpers
import hargparse, macros, tables
import shell
import os
import colechopkg/lib

var doDebug = true

let startdir = getcurrentdir()

proc decho(args: varargs[string, `$`], ind = 0) =
  if doDebug:
    ceUserLog0(args.join(" "), ind)

proc compileOrgFile(targetFileName: string) =
  decho "Compiling file " & targetFileName
  let (user, exit) = execShell("id -un")
  echo user

  block:
    let command = "emacs -u " & user &
      " --batch " &
      " --eval '(load user-init-file)' " &
      &"\"{targetFileName}\"" &
      " -f org-latex-export-to-pdf"

    let (outp, code) = execShell(command)
    if code != 0:
      echo outp

  # block:
  #   let texFile = targetFileName.splitFile.name & ".tex"
  #   decho "Compilinng latex file " & texFile
  #   let command = "latexmk -latexoption=\"-shell-escape\" -pdflua " &
  #     " --interaction=nonstopmode " & &"\"{texFile}\""

  #   let (outp, code) =  execShell(command)
  #   if code != 0:
  #     ceUserWarn "Error occurred while compiling " & texFile
  #     echo outp




proc checkForKey(table: TomlValueRef, key: string, tableName: string = "table") =
  if not table.hasKey(key):
    ceUserError0(&"Table '{tableName}' is missing {key}")
    quit 1

proc processReport(report: TomlValueRef, header: string, runCompile = false) =
  report.checkForKey("name");

  let name = report["name"].getStr()

  ceUserInfo2 "Processing report " & name
  decho name

  report.checkForKey("files_globs", name)
  report.checkForKey("image_globs", name)
  report.checkForKey("description", name)
  report.checkForKey("flowchart_globs", name)
  report.checkForKey("file", name)

  var imageWidth = 400
  let description = report["description"].getStr()

  var hasImages = false
  decho "program images", ind = 2
  let images: string = report["image_globs"]
    .getElems().mapIt(it.getstr).mapIt(
      toseq(it.walkPattern()).mapIt(
        block:
          hasImages = true
          decho it, ind = 4
          &"""
#+attr_latex: :width 0.9\textwidth :height 0.9\textheight :options keepaspectratio
[[../{it}]]
""")).concat().join("\n\n")

  if not hasImages:
    ceUserWarn &"Report {name} has no program images"

  hasImages = false
  decho "flowchart images", ind = 2
  let flowcharts = report["flowchart_globs"]
    .getelems().mapit(it.getstr).mapit(
      toseq(it.walkpattern()).mapit(
        block:
          hasImages = true
          decho it, ind = 4
          &"""
** {it.splitpath().tail}

#+attr_latex: :width 0.9\textwidth :height 0.9\textheight :options keepaspectratio
[[../{it}]]
""")).concat().join("\n\n")

  if not hasImages:
    ceUserWarn &"Report {name} has no flowchart images"

  decho "source code files:", ind = 2
  let sources = report["files_globs"]
    .getelems().mapit(it.getstr).mapit(
      toseq(it.walkpattern()).mapit(
        block:
          decho it, ind = 4
          &"""
** {it.splitpath().tail}

#+HEADERS: :noeval
#+ATTR_LATEX: :float nil
#+BEGIN_SRC txt
{it.readfile().string()}
#+END_SRC
""")).concat().join("\n\n")

  let outText = fmt"""
{header}

* Постановка задачи

{description}

#+Begin_Latex
\pagebreak
#+End_Latex

* Примеры работы программы

{images}

#+Begin_Latex
\pagebreak
#+End_Latex

* Блок-схемы алгоритмов

{flowcharts}

#+Begin_Latex
\pagebreak
#+End_Latex

* Исходный код

{sources}

"""


  let fileName = report["file"].getstr()
  let workDir = filename & "out.d"
  createdir(workdir)
  setcurrentdir(workdir)

  decho "file name is", filename, ind = 4
  let file = (fileName).open(fmWrite)
  file.write(outText)
  file.close()

  if runCompile:
    decho "compiling file", filename, ind = 4
    compileOrgFile(fileName)


  setcurrentdir(startdir)



var imageSize = 300
proc processFile(file: TomlValueRef): string =
  proc gs(item: string): string = file[item].getStr()
  proc kp(item: string): bool = file.hasKey(item)

  let name = tern("name".kp(), "name".gs(), "MISSING NAME !!!")
  let path = tern("path".kp(), "path".gs(), "MISSING PATH !!!")

  let image_glob = tern("images".kp(), "images".gs(), "")

  let description = tern(
    "description".kp(),
    "description".gs(),
    "MISSING DESCRIPTION !!!")

  let source_code =
    try:
      "#+ATTR_LATEX: :float nil\n#+BEGIN_SRC txt\n" & path.readFile().string() & "\n#+END_SRC"
    except:
      "MISSING SOURCE CODE !!!"

  var example_images =
    block:
      var tmp = ""
      for img in walkPattern(image_glob):
        tmp &= &"#+attr_latex: :width {imageSize}px :placement [!h]\n"
        tmp &= "[[./" & img & "]]\n"
      tmp

  result &= &"""

#+Begin_Latex
\pagebreak
#+End_Latex

** {name}

*** Постановка задачи

{description}

*** Исходный код программы

{source_code}
"""

  result &= tern(
    example_images.len > 0,
    "*** Примеры работы программы\n" & example_images & "\n",
    ""
  )

parseArgs:
  opt:
    name: "out-file"
    opt: ["--out-file", "-o", "--output", "+takes_value"]
    help: "File to write generated org file"
  opt:
    name: "in-dir"
    opt: ["--input", "--in", "+takes_value"]
    help: "Source file for configuration."
  opt:
    name: "compile-pdf"
    opt: ["--compile"]
    help: "Run emacs to compile pdf"
  opt:
    name: "image-size"
    opt: ["--imsize", "+takes_value"]
    help: "Image width"

if "image-size".kp:
  imageSize = "image-size".k.toInt()

echo &"Image size is {imageSize}"

var sourceDir =
  if "in-dir".kp:
    "in-dir".k.tostr()
  else:
    echo "Missing input directory"
    quit(1)

setCurrentDir(sourceDir);

var targetFileName =
  if "out-file".kp:
    "out-file".k.toStr()
  else:
    "report.tmp.org"

var targetFile = targetFileName.open(fmWrite)
let conf = parseFile("input.toml")

proc output(text: string) =
#  echo text
  targetFile.write(text)

let header =
  if conf.hasKey("org_header"):
    conf["org_header"].getStr() & "\n\n"
  else: """
#+LATEX_CLASS_OPTIONS: [a4paper,12pt]
#+LATEX_HEADER: \usepackage[left=1.5cm,right=2cm,top=3cm,bottom=3cm]{geometry}
#+LATEX_HEADER: \usepackage[pdfborder={0,0,0}]{hyperref}
#+LATEX_HEADER: \hypersetup{colorlinks=true,linkcolor=blue}

#+LATEX_HEADER: \usepackage[T2A]{fontenc}
#+LATEX_HEADER: \usepackage[utf8]{inputenc}
#+LATEX_HEADER: \usepackage[russian]{babel}
#+LATEX_HEADER: \usepackage{adjustbox}

#+LATEX_HEADER: \addto\captionsenglish{\renewcommand{\contentsname}{Оглавление}}

#+OPTIONS: toc:1

"""

output header

if conf.hasKey("report"):
  for report in conf["report"].getElems():
    processReport(report, runCompile = "compile-pdf".kp, header = header)

if conf.hasKey("file"):
  for file in conf["file"].getElems():
    output processFile(file)

  if "compile-pdf".kp:
    compileOrgFile(targetFileName)


targetFile.close()




#[

TODO automatically use emacs to genrate pdf and odt files on export

]#
