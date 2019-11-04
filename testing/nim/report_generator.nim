import parsetoml
import colechopkg/lib
import strformat
import strutils
import sequtils
import os

let conf = parseFile("input.toml")

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

  let name = "name".gs()
  let path = "path".gs()
  let image_glob = "images".gs()

  let description = "description".gs()

  let source_code =
    "#+BEGIN_SRC prolog\n" & path.readFile().string() & "#+END_SRC"

  var example_images =
    block:
      var tmp = ""
      for img in walkPattern(image_glob):
        tmp &= "#+attr_latex: :width 400px :placement [!h]\n"
        tmp &= "[[./" & img & "]]\n"
      tmp

  result &= &"""
** {name}

*** Постановка задачи

{description}

*** Исходный код программы

{source_code}

*** Примеры работы программы

{example_images}
"""

var targetFile = "report.tmp.org".open(fmWrite)

proc output(text: string) =
  echo text
  targetFile.write(text)

let header = conf["org_header"].getStr() & "\n\n"

output header

if conf["type"].getStr() == "reports":
  for report in conf["report"].getElems():
    #echo processReport(report)
    discard
elif conf["type"].getStr() == "files":
  for file in conf["file"].getElems():
    output processFile(file)


targetFile.close()
