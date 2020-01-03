import parsetoml
import sequtils
import options
import keyboard
import common
import strutils
import geometry
import hmisc/helpers

type
  Toml = TomlValueRef
  TomlVal = TomlValueRef

proc getF(t: Toml, key: string): float =
  if t.hasKey(key): t[key].getFloat()
  else: raise newException(ValueError, "Table does not have field " & key)


proc getI(t: Toml, key: string): int =
  if t.hasKey(key): t[key].getInt()
  else: raise newException(ValueError, "Table does not have field " & key)

proc getS(t: Toml, key: string): string =
  if t.hasKey(key): t[key].getStr()
  else: raise newException(ValueError, "Table does not have field " & key)

proc parseLockConf(table: Toml): void =
  dlog "Parsing lock configuration"
  globalLockConf.depth = table.getF("depth")
  globalLockConf.sectionWidth = table.getF("sectionWidth")
  globalLockConf.lockWidth = table.getF("lockWidth")
  globalLockConf.offsetSize = table.getF("offsetSize")
  globalLockConf.widthMultiplier = table.getF("widthMultiplier")
  globalLockConf.height = table.getF("height")
  dlog "Done"

proc parseBlockConf(table: Toml): void =
  dlog "Parsing block configuration"
  blockConf.baseHeight = table.getF("baseHeight")
  blockConf.shellHeight = table.getF("shellHeight")
  blockConf.topThickness = table.getF("topThickness")
  blockConf.shellThickness = table.getF("shellThickness")
  blockConf.lidElevation = table.getF("lidElevation")
  dlog "Done"

proc parseDefaultConf(table: Toml): void =
  dlog "Parsing default value configuration"
  defaultConf.keyWidth = table.getF("keyWidth")
  defaultConf.keyHeight = table.getF("keyHeight")
  defaultConf.keyLength = table.getF("keyLength")
  defaultConf.keySpacing = table.getF("keySpacing")
  defaultConf.rowSpacing = table.getF("rowSpacing")
  defaultConf.keyInnerLength = table.getF("keyInnerLength")
  defaultConf.keyInnerWidth = table.getF("keyInnerWidth")
  defaultConf.firstKeySpacing = table.getF("firstKeySpacing")
  defaultConf.exportMode = table.getS("exportMode").parseEnum(emSinglePiece)
  dlog "Done"

proc makeDefaultKey(
  length: Option[float] = none(float),
  width: Option[float] = none(float),
  height: Option[float] = none(float),
  keyCode: Option[string] = none(string),
  innerLength: Option[float] = none(float),
  innerWidth: Option[float] = none(float)
     ): Key =
  Key(
    length: length.get(defaultConf.keyLength),
    width: width.get(defaultConf.keyWidth),
    height: height.get(defaultConf.keyHeight),
    innerLength: innerLength.get(defaultConf.keyInnerLength),
    innerWidth: innerWidth.get(defaultConf.keyInnerWidth),
    keyCode: keyCode
  )

func optF(
  table: TomlVal, key: string, default: Option[float] = none(float)
     ): Option[float] =
  if table.hasKey(key):
    some(table[key].getFloat())
  else:
    default

func optF(table: TomlVal, default: Option[float] = none(float)): Option[float] =
  if table.kind == Float: some(table.getFloat())
  else: default

func optS(
  table: Toml, key: string, default: Option[string] = none(string)
     ): Option[string] =
  if table.hasKey(key):
    some(table[key].getStr())
  else:
    default


func optS(
  table: TomlVal, default: Option[string] = none(string)
     ): Option[string] =
  if table.kind == String: some(table.getStr())
  else: default


proc parseKeys(
  row: Toml,
  rowDefaultSpacing: Option[float] = none(float)
     ): seq[tuple[key: Key, space: float]] =
  proc getSpacing(idx: int): float =
    if idx == 0:
      rowDefaultSpacing.get(defaultConf.firstKeySpacing)
    else:
      defaultConf.keySpacing


  case row.kind:
    of Int: result = (0..<row.getInt()).mapIt(
      (key: makeDefaultKey(), space: getSpacing(it))
    )
    of Array:
      for idx, key in row.getElems():
        case key.kind:
          of String:
            result.add (
              key: makeDefaultKey(keyCode = some(key.getStr())),
              space: getSpacing(idx)
            )
          of TomlValueKind.Table:
            result.add (
              key: makeDefaultKey(
                width = key.optF("width"),
                height = key.optF("height"),
                length = key.optF("length"),
                keyCode = key.optS("keyCode"),
                innerLength = key.optF("innerLength"),
                innerWidth = key.optF("innerWidth"),
              ),
              space: key["space"].optF().get((idx == 0).tern(
                defaultConf.firstKeySpacing,
                defaultConf.keySpacing
              ))
            )
          else:
            raise newException(
              ValueError,
              "Invalid value type for 'keys' element. String or Table expected"
            )

    else:
      raise newException(
        ValueError,
        "Invalid value type for 'keys' file. integer or array expected")
  # discard

proc parseRows(blc: Toml): seq[tuple[row: Row, space: float]] =
  for row in blc["row"].getElems():
    var newRow: tuple[row: Row, space: float]
    if row.hasKey("space"):
      newRow.space = row.getF("space")
    else:
      newRow.space = defaultConf.rowSpacing

    newRow.row.keys = parseKeys(
      row["keys"],
      row.hasKey("indent").tern(
        some(row.getF("indent")), none(float))
    )
    result.add(newRow)

proc parseBlocks(blocks: seq[Toml]): seq[Block] =
  for blc in blocks:
    let pos = blc["positioning"]
    result.add Block(
      rows: blc.parseRows(),
      angles: (
        blc["angles"].getF("left"),
        blc["angles"].getF("right")),
      offsets: (
        blc["offsets"].getF("left"),
        blc["offsets"].getF("right")),
      positioning: (
        id: pos.getI("id"),
        pos: pos.getS("pos").parseEnum(rpLeft),
        offset: pos.getF("offset"),
        relativeTo: pos.getI("relativeTo")
      ),
      dimensions: (
        blc["dimensions"].getF("width"),
        blc["dimensions"].getF("lowerLen")),
    )


proc parseKeyboard*(file: string): Keyboard =
  let conf = file.parseFile()
  result = Keyboard()

  parseLockConf(conf["keyboard"]["interlockConf"])
  parseBlockConf(conf["keyboard"]["blockConf"])
  parseDefaultConf(conf["keyboard"]["defaultConf"])

  result.blocks = parseBlocks(conf["block"].getElems())

  result = result.toRadianAngles()
