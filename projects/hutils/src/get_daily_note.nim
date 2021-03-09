import strformat, strutils
import hargparse
import macros

import re
import os
import times
import math

const newDayAfter = 5
const logMinDelay = 8

const testing = true
proc dlog(args: varargs[string, `$`]): void =
  when testing: echo args.join(" ")
  else: discard

type
  NoteType = enum
    ntDaily
    ntWeekly
    ntMonthly

proc getWeekNum(time: DateTime): int =
  ceil(
    (
      getDayOfYear(
        monthday = time.monthday,
        month = time.month,
        year = time.year) +
      getDayOfWeek(1, mJan, time.year).ord
    ) / 7
  ).toInt()


proc getCurrentNote(fileDirectory: string, ntype: NoteType = ntDaily): string =
  ## Return path to current daily note
  var time = now()
  if time.hour < newDayAfter:
    time.monthday = time.monthday - 1


  result = fileDirectory.joinPath(
    case ntype:
      of ntDaily: time.format("yyyy-MM-dd") & ".org"
      of ntWeekly: time.format("yyyy") &
        "-W" &
        ($time.getWeekNum()).align(2, '0') & ".org"
      of ntMonthly: time.format("yyyy-MM") & ".org"
  )

proc noteAppendRequired(note: string): bool =
  var lastHour = 0
  var lastMinute = 0
  for line in note.readFile().split("\n"):
    if line =~ re"^\*\* @time:(\d\d):(\d\d);":
      lastHour = matches[0].parseInt()
      lastMinute = matches[1].parseInt()

  proc getSince0000(hour, minute: int): int =
    if hour < newDayAfter:
      minute + (24 + hour) * 60
    else:
      minute + hour * 60

  let now0000 = getSince0000(now().hour, now().minute)
  let file0000 = getSince0000(lastHour, lastMinute)

  return now0000 - file0000 > logMinDelay or (
    lastHour == 0 and lastMinute == 0)

proc getTimeStampNow(): string =
  "@time:" & now().format("HH:mm") & ";"

proc thisWeekDays(): seq[string] =
  let curr = now()
  for day in WeekDay:
    let nowIdx = cast[int](curr.weekday)
    let dayIdx = cast[int](day)
    let diff = dayIdx - nowIdx
    let interval = TimeInterval(days: abs(diff))
    let res =
      if diff > 0: curr + interval
      else: curr - interval

    result.add res.format("yyyy-MM-dd dddd")

proc addNewLog(note: string): void =
  let file = note.open(fmAppend)
  file.write("\n** " & getTimeStampNow() & "\n\n\n")
  file.close()

proc createNewNote(note: string, ntype: NoteType): void =
  let body =
    case ntype:
      of ntDaily:
        let head = "#+TITLE: @date:" &
        now().format("yyyy-MM-dd") &
        "; " & getTimeStampNow() & "\n\n"

        let org_time = now().format("yyyy-MM-dd ddd") & " 23:55"
        let tail = &"""
* TODO Tasks [/]
  DEADLINE: <{org_time}>
** TODO <++>

* Logs

""" & "** " & getTimeStampNow() & "\n\n"
        head & tail
      of ntWeekly:
        &"""
#+title: Weekly note N{now().getWeekNum()}

{thisWeekDays().join("\n")}

"""
      of ntMonthly:
        &"""
#+title: Monthly note for {now().format("MMMM yyyy")}
"""

  let file = note.open(fmWrite)
  file.write(body)
  file.close()

proc fileIsEmpty(note: string): bool =
  note.readFile().len == 0


parseArgs:
  opt:
    name: "modify-file"
    opt: ["--mod-file"]
    help: "Whether or not to append new log or create missing file"
  opt:
    name: "file-dir"
    opt: ["--file-dir", "+takes_values"]
    help: "Directory for note file"
  opt:
    name: "update-symlink"
    opt: ["--update-symlink"]
    help: "Udate symbolic link for 'today' note"
  opt:
    name: "period"
    opt: ["--period"]
    help: "Get daily, weekly or mothly note"
    takes: @["week", "day", "month"]



let ntype =
  if not "period".kp or "period".k.toStr() == "day":
    ntDaily
  elif "period".k.toStr() == "week":
    ntWeekly
  else:
    ntMonthly

let fileDirectory =
  if "file-dir".kp:
    let dir = "file-dir".k.toStr()
    if dir.endsWith("/"): dir[0..^2]
    else: dir
  else:
    getHomeDir() &
      ".config/hax-local/dirs/personal/notes/" &
      (
        case ntype:
          of ntDaily: "daily/"
          of ntWeekly: "weekly/"
          of ntMonthly: "monthly/"
      )

let note = getCurrentNote(fileDirectory, ntype)

if "modify-file".kp:
  if not fileExists(note) or note.fileIsEmpty():
    createNewNote(note, ntype)

  if ntype == ntDaily and noteAppendRequired(note):
    addNewLog(note)

if "update-symlink".kp:
  removeFile(fileDirectory & "/today.org")
  createSymlink(src = note, dest = fileDirectory & "/today.org")

echo note
