import shell
import json
import math
import strformat
import sequtils
import xmltree
import colechopkg/lib
import os

import svg_generation
import keyboard
import scad_generation
import geom_operations

let kbd = readFile("keyboard.json").
  parseJson().
  to(Keyboard).
  toRadianAngles()

let test = kbd.blocks[0]

proc generateSCAD(kbd: Keyboard, outFile: string): void =
  let scadBody = kbd.toSCAD().addSCADImports().toString()
  outFile.writeFile(scadBody)
  let clangRes = shellVerbose:
    "clang-format" -i ($outFile)

  if clangRes.code != 0:
    ceUserError0 "Error when formatting keyboard scad file"
  else:
    ceUserLog0 "Formatting ok"


proc generateSVG(blc: Block, outFile: string): void =
  let imgWidth = svgMulti * 40
  let imgHeight = svgMulti * 30

  let svgBody = @[test.toSVG()].toSVGImage(
    width = imgWidth,
    height = imgHeight
  )

  let fileName = "out.tmp.svg"
  let tmpFile = "out.tmp.png"
  fileName.writeFile(xmlHeader & $svgBody)

  let convertRes = shellVerbose:
    inkscape -z -e ($tmpFile) -w ($imgWidth) -h ($imgHeight) ($fileName)

  if convertRes[1] == 0:
    ceUserLog0("Conversion ok")
    copyFile(tmpFile, outFile)

# test.generateSVG("res.tmp.png")
kbd.generateSCAD("res.tmp.scad")
