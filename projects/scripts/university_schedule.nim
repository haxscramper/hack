import parsecsv, strutils, sequtils, strformat, parseutils, times,
       math

import hmisc/other/hjson
from streams import newFileStream
import hmisc/hdebug_misc
import hpprint, tables
import hmisc/helpers

var buf: seq[seq[string]]

startHax()

var s = newFileStream("in.csv", fmRead)
var x: CsvParser
open(x, s, "in.csv", separator = '\t')
while readRow(x):
  buf.add @[]
  for val in items(x.row):
    buf[^1].add val

close(x)

type
  Class = object
    cType: string
    room: string
    name: string
    idx: int

  Day = object
    classes: seq[Class]

var
  curr = 0
  idx = 0

var evenwk, oddwk: seq[Day]

evenwk.add Day()
oddwk.add Day()

while idx < buf.len:
  if idx div 12 != curr:
    evenwk.add Day()
    oddwk.add Day()
    curr = idx div 12

  let class = Class(
    name: buf[idx][0],
    cType: buf[idx][1],
    room: buf[idx][3],
    idx: (idx mod 12) div 2
  )

  if idx mod 2 == 0:
    evenwk[^1].classes.add class
  else:
    oddwk[^1].classes.add class

  inc idx

if evenwk.len < 6:
  evenwk.add Day()

if oddwk.len < 6:
  oddwk.add Day()



type
  WkId = int
  DayId = int
  Tbl = Table[WkId, Table[DayId, Table[int, Class]]]

var schTable: Tbl

func `[]=`(tbl: var Tbl, wk, day, idx: int, class: Class): void =
  if class.room == "":
    return

  if wk notin tbl:
    tbl[wk] = {day : {idx : class}.toTable()}.toTable()

  if day notin tbl[wk]:
    tbl[wk][day] = {idx : class}.toTable()

  # if idx notin tbl[wk][day]:
  tbl[wk][day][idx] = class


func parseUntil*(str, fin: string): string =
  discard parseUntil(str, result, fin)

func parseAfter*(str, fin: string): string =
  var tmp: string
  let pos = str.find(fin)
  if pos == -1:
    result = str
  else:
    # echov str
    # echov pos
    # echov pos + len(fin)
    result = str[pos + len(fin) ..< str.len]

for wk in 0 .. 16:
  for idx in 0 .. 5:
    let day =
      if wk mod 2 == 0:
        evenwk[idx]
      else:
        oddwk[idx]

    for class in day.classes:
      let name = class.name

      if name.len > 0 and name[0] in {'0' .. '9'}:
        for (headId, head) in name.split("\n").enumerate():
          let weeks = head.parseUntil("н.").
            split(",").
            mapIt(it.strip())

          if weeks[0].len > 0 and weeks.len > 1:
            # echo weeks, " ",
            let name = head.parseAfter("н.")
            for week in weeks:
              schTable[week.parseInt() - 1, idx, class.idx] =
                  class.withIt do:
                    it.name = name.strip()
                    # it.room = class.room.split("\n")[headId].strip()
                    it.cType = class.cType.split("\n")[headId].strip()
              # schTable.mgetOrPut(week.parseInt(), idx).
              #          mgetOrPut(idx, class)

      else:
        # echov wk, idx
        schTable[wk, idx, class.idx] = class

# pprint schTable[0]

var res: string

let start = parse("2021-02-09", "yyyy-MM-dd")

func toICS(d: DateTime): string =
  d.format("yyyyMMdd") & "T" & d.format("HHmmss") & "Z"

func toGCAL(d: DateTime): string =
  d.format("dd/MM/yyyy") & "," & d.format("HH:mm:ss")

func breakShift(idx: int): int =
  [0, 10, 10, 30, 10, 10, 30][0 .. idx].sum()

var evs: seq[JsonNode]

func toColor(cl: Class): string =
  # 1 blue
  # 2 green
  # 3 purple
  # 4 red
  # 5 yellow
  # 6 orange
  # 7 turquoise
  # 8 gray
  # 9 bold blue
  # 10 bold green
  # 11 bold red
  if cl.name.startsWith("Физическая культура"):
    "2"
  else:
    case cl.cType:
      of "лк": "8"
      of "пр": "5"
      of "лаб": "4"
      else: "1"

for wkId, days in schTable:
  for dayId, classes in days:
    for clId, class in classes:
      # echov clId
      let tBegin = start + wkId.weeks + (dayId - 1).days +
          9.hours + (90 * clId).minutes +
          breakShift(clId).minutes

      # echov class.name, class.cType, tBegin.weekDay, tBegin.format("HH:mm")

      let tEnd = tBegin + 90.minutes

      if tBegin.hour < 4:
        echov tBegin, wkId, dayId, clId
        pprint class
        dieHere()

      evs.add %{
        "name" : %(&"{class.name} {class.cType}, {class.room}"),
        "room" : %class.room,
        "begin": %($tBegin),
        "end": %($tEnd),
        "color": %class.toColor()
      }

      res.add &"""
"{class.name} {class.cType}, {class.room}",{tBegin.toGCAL()},{tEnd.toGCAl()}
"""

res = &"""
Subject,Start Date,Start Time,End Date,End Time
{res}
"""


"res.json".writeFile($(%evs))
"res.csv".writeFile(res)
