import parsetoml
import colechopkg/lib
import strformat
import strutils
import sequtils
import hmisc/helpers
import hargparse, macros, tables
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
      "#+BEGIN_SRC prolog\n" & path.readFile().string() & "#+END_SRC"
    except:
      "MISSING SOURCE CODE !!!"

  var example_images =
    block:
      var tmp = ""
      for img in walkPattern(image_glob):
        tmp &= "#+attr_latex: :width 400px :placement [!h]\n"
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
    name: "in-file"
    opt: ["--input", "--in", "+takes_value"]
    help: "Source file for configuration."



var targetFile =
  if "out-file".kp:
    "out-file".k.toStr().open(fmWrite)
  else:
    "report.tmp.org".open(fmWrite)

let conf =
  parseFile(
    tern("in-file".kp, "in-file".k.toStr(), "input.toml")
  )


proc output(text: string) =
#  echo text
  targetFile.write(text)

let header = conf["org_header"].getStr() & "\n\n"

output header

if conf.hasKey("report"):
  for report in conf["report"].getElems():
    discard
    # echo processReport(report)

if conf.hasKey("file"):
  for file in conf["file"].getElems():
    output processFile(file)

targetFile.close()
echo "done"

#[

TODO automatically use emacs to genrate pdf and odt files on export

]#
