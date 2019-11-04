import parsetoml
import colechopkg/lib

let conf = parseFile("input.toml")

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


for report in conf["report"].getElems():
  var outBody = ""
  let name = report["name"].getStr()
  outBody &= setName(name)

  try:
    outBody &= insertCode(
      report["file_dir"].getStr(),
      report["file_ext"].getStr()
    )
  except:
    ceUserError0("Missing file_dir " & name)

  echo outBody
