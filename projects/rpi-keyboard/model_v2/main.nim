import shell
import math
import strformat
import sequtils
import xmltree
import colechopkg/lib
import os

import svg_generation
import keyboard
import scad_generation

let test = Block(
  rows: @[
    (Row(keys: @[
      (Key(width: 1.5, length: 2.0, height: 1.0), 0.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0)
    ]), 0.0),
    (Row(keys: @[
      (Key(width: 1.5, length: 2.0, height: 1.0), 3.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
    ]), 1.0),
    (Row(keys: @[
      (Key(width: 1.5, length: 2.0, height: 1.0), -1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
      (Key(width: 1.5, length: 2.0, height: 1.0), 1.0),
    ]), 1.0),
],
  angles: (PI/2 + PI/18, PI/2 - PI/18),
  offsets: (0.2, 0.2),
  dimensions: (
    width: 10.0,
    lowerLen: 22.0
  )
)


proc generateSCAD(blc: Block, outFile: string): void =
  let scadBody = blc.toSCAD()
  outFile.writeFile(scadBody)
  let clangRes = shellVerbose:
    "clang-format" -i ($outFile)

  if clangRes[1] == 0:
    ceUserLog0("Formatting ok")

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

test.generateSVG("res.tmp.png")
test.generateSCAD("res.tmp.scad")
