#!/usr/bin/env nimcr
#nimcr-args c --verbosity:0 --hints:off

import shell
import strscans
import hargparse
import colechopkg/lib
import sequtils
import strutils
import os
import re
import random
import hmisc/hfile
import hmisc/helpers
import strformat

const testing = false

proc shellConfig(): set[DebugOutputKind] = {}

parseArgs:
  opt:
    name: "debug"
    opt: ["--debug"]
    help: "Use debug printing"
  opt:
    name: "circuit-dir"
    opt: ["--circuit-dir"]
    help: "Directory containing Circuit_macros distribution"
  opt:
    name: "png-using"
    opt: ["--png-using"]
    takes: @["latex", "svg"]
    help: "Choose way of creating png images (intermediate steps)"



let doDebug = testing or "debug".kp
proc dlog(args: varargs[string, `$`]): void =
  if doDebug:
    ceUserLog0 args.join(" ")

dlog ">>>> ========"

if doDebug:
  dlog "Debug enabled"
  echo "Arguments"
  for k, v in optParsed:
    echo k, " ", v

let (sourceFile, targetFile, circuitDir, targetExt) =
  block:
    let (sourceFile, targetFile) =
      when testing:
        ("input.tmp.m4", "output.tmp.png")
      else:
        if argParsed.len < 2:
          ceUserError0 "Missing input and output files (last two arguments for commang)"
          quit 1
        else:
          (argParsed[0], argParsed[1])

    let circuitDir =
      if "circuit-dir".kp: "circuit-dir".k.toStr
      else: "~/.config/hax-software/m4circuit".expandTilde()

    let targetExt =
      block:
        let (_, _, ext) = targetFile.splitFile()
        if ext.len > 0:
          ext[1..^1]
        else:
          ceUserError0(&"Target file {targetFile} has no extension")
          quit 1

    (sourceFile, targetFile, circuitDir, targetExt)

dlog "Input file:", sourceFile
dlog "Output file:", targetFile
dlog "Circuit dir:", circuitDir
dlog "Target ext:", targetExt

proc gettmp(): string =
  mktemp(
    templ = "circuit.tmp.XXXXXXXXX",
    dir = "/tmp/circuit/"
  )



proc createPngUsingSVG(inFile, outFile: string): bool =
  dlog "Using svg"
  let macroResult = gettmp()
  var width = 400
  var height = 400
  let
    picfile = macroResult & ".pic"
    svgfile = macroResult & ".svg"

  block:
    dlog "Running m4"
    dlog &"Outputting into {picfile}"
    let (res, err, code) = shellVerboseErr shellConfig():
      m4 -I ($circuitDir) "svg.m4" ($inFile) > ($picfile)

    if code != 0:
      dlog "Error duing m4 execution"
      dlog err
      return false
    else:
      dlog "M4 ok"
      dlog res

  block:
    let (res, err, code) = shellVerboseErr shellConfig():
      dpic -v ($picfile) > ($svgfile)

    if code != 0:
      dlog "Error duing dpic execution"
      dlog err
    else:
      dlog "pic generation ok"
      dlog res
      dlog err

  block:
    let (dim, code) = shellVerbose shellConfig():
      grep "\"<!-- width=\"" ($macroResult".svg")

    if not scanf(dim, "<!-- width=\"$i\" height=\"$i\" -->", width, height):
      dlog dim, "does not match"
    else:
      dlog &"width: {height} height: {width}"

  width *= 3
  height *= 3

  block:
    let (res, code) = shellVerbose shellConfig():
      inkscape -z -e ($outFile) -w ($width) -h ($height) ($macroResult".svg")

    if code != 0:
      dlog "Error durink inkscape execution"
      dlog res
    else:
      dlog "Inkscape ok"


proc createTexTikzpicture(inFile: string): string =
  let picfile =  gettmp() & ".pic"
  dlog &"M4 -> tikz ({inFile} -> {picfile})"
  let (m4res, m4err, m4code) = shellVerboseErr shellConfig():
    m4 -I ($circuitDir) pgf.m4 ($inFile) > ($picfile)

  if m4code != 0 and doDebug:
    dlog "Error during m4 execution"
    echo m4res
    echo m4err

  let (picRes, picErr, picCode) = shellVerboseErr shellConfig():
    dpic -g ($picfile)


  if picCode != 0 and doDebug:
    dlog "Error during pic execution"
    echo picErr
    dlog "Input file:"
    if picfile.existsFile():
      var idx = 1
      for line in picfile.lines():
        echo &"{idx:>3}| {line}"
        inc idx
    else:
      echo &"{picfile} does not exist"

  result = picRes


proc createTexStandalone(inFile, outFile: string) =
  outFile.writeFile(fmt"""
\documentclass[convert = false]{{standalone}}
\usepackage{{tikz}}

\begin{{document}}

{createTexTikzpicture(inFile)}

\end{{document}}
""")

proc createPdfUsingTexTikz(inFile, outFile: string): bool =
  let macroResult = gettmp()
  let texfile = macroResult & ".tex"
  let pdffile = macroResult & ".pdf"
  dlog "Creating png using latex, outputting into", texfile
  dlog "Output file is", outFile
  createTexStandalone(inFile, texfile)
  let (res, err, code) = shellVerboseErr shellConfig():
    # latexmk -cd -C ($texfile)
    latexmk -cd -pdf "-latexoption='-shell-escape'" "--interaction='nonstopmode'" ($texfile)

  if code != 0:
    dlog "Error duing latexmk execution"
    # TODO parse error into something more usable
    # dlog err
    return false
  else:
    shell:
      cp ($pdffile) ($outFile)

    return true



proc createEpsUsingTex(inFile, outFile: string) =
  let macroResult = gettmp()
  let pdfFile = macroResult & ".pdf"
  if not createPdfUsingTexTikz(inFile, pdfFile):
    dlog "Pdf creation using tikz failed"
  else:
    let (res, err, code) = shellVerboseErr shellConfig():
      gm convert ($inFile) ($outFile)

    if code != 0:
      dlog "gm convert failed"
      dlog err


proc createPngUsingTex(inFile, outFile: string) =
  let macroResult = gettmp()
  let pdfFile = macroResult & ".pdf"
  if not createPdfUsingTexTikz(inFile, pdfFile):
    dlog "Pdf creation using tikz failed"
  else:
    let (res, code) = shellVerbose shellConfig():
      gm convert -density 300 ($pdfFile) -quality 150 ($outFile)

    dlog "Output code is", code


block:
  let inFile = sourceFile
  let file = inFile.open(fmRead)
  var line: string
  if file.readLine(line):
    if line.startsWith(".PS"):
      dlog "full-sized pic file"
    else:
      dlog "shortened pic file"
      let fileString = inFile.readFile().string()
      inFile.writeFile(&"""
.PS
cct_init
{fileString}
.PE
""")

      dlog &"Added wrapping for file {inFile}"
      if doDebug:
        echo inFile.readFile()
  else:
    dlog "cannot read first line from file", inFile

  file.close()



case targetExt:
  of "png":
    dlog "Creating png target"
    when testing:
      createPngUsingTex(sourceFile, targetFile)
    else:
      if not "png-using".kp:
        createPngUsingTex(sourceFile, targetFile)
      else:
        case "png-using".k.tostr:
          of "latex": createPngUsingTex(sourceFile, targetFile)
          of "svg":
            let ok = createPngUsingSVG(sourceFile, targetFile)
            if not ok:
              dlog "Error generating png"


  of "tex":
    dlog "Creating tex target"
    if "tex-tikzpicture".kp:
      targetFile.writeFile(createTexTikzpicture(sourceFile))
    else:
      createTexStandalone(sourceFile, targetFile)

  of "eps":
    dlog "Creating eps target"
    createEpsUsingTex(sourceFile, targetFile)
  else:
    ceUserError0 "Unknown extension " & targetExt


dlog "<<<< ========"
