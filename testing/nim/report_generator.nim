import parsetoml
import colechopkg/lib
import strformat
import strutils
import sequtils
import hmisc/helpers
import hargparse, macros, tables
import shell
import os


#[
proc setName(name: string): string =
  result = "* " & name

proc insertCode(filesDir: string, extension: string): string =
  result = filesDir

proc insertFlowchars(flowchartDir: string): string =
  result &= "* Блок-схемы\n"
  result &= flowchartDir
  for file in @[flowchartDir]: # TODO walk files
    result &= "[[./" & file & "]]\n"

proc insertImages(imageDir: string): string =
  result &= "* Примеры работы программы\n"
  for file in @[imageDir]: # TODO walk files
    result &= "[[./" & file & "]]\n"


proc processReport(report: TomlValueRef): string =
  let name = report["name"].getStr()
  result &= setName(name)

  try:
    result &= insertCode(
      report["file_dir"].getStr(),
      report["file_ext"].getStr()
    )
  except:
    ceUserError0("Missing file_dir " & name)

]#

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
      "#+BEGIN_SRC txt\n" & path.readFile().string() & "\n#+END_SRC"
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
  else:
"""
#+LATEX_CLASS_OPTIONS: [a4paper,12pt]
#+LATEX_HEADER: \\usepackage[left=1.5cm,right=2cm,top=3cm,bottom=3cm]{geometry}
#+LATEX_HEADER: \\usepackage[pdfborder={0,0,0}]{hyperref}
#+LATEX_HEADER: \\hypersetup{colorlinks=true,linkcolor=blue}

#+LATEX_HEADER: \\usepackage[T2A]{fontenc}
#+LATEX_HEADER: \\usepackage[utf8]{inputenc}
#+LATEX_HEADER: \\usepackage[russian]{babel}

#+LATEX_HEADER: \\addto\\captionsenglish{\\renewcommand{\\contentsname}{Оглавление}}

#+OPTIONS: toc:1

"""

output header

if conf.hasKey("report"):
  for report in conf["report"].getElems():
    discard
    # echo processReport(report)

if conf.hasKey("file"):
  for file in conf["file"].getElems():
    output processFile(file)

targetFile.close()



if "compile-pdf".kp:
  let (user, exit) = execShell("id -un")
  echo user
  let command = "emacs -u " & user &
    " --batch " & &"\"{targetFileName}\"" &
    " --eval '(load user-init-file)' " &
    " -f org-latex-export-to-pdf"

  let (outp, code) = execShell(command)
  echo outp


#[

TODO automatically use emacs to genrate pdf and odt files on export

]#
