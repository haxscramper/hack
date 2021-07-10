import std/[db_sqlite, sqlite3, options, strutils, macros]
import hmisc/other/oswrap

type
  SqliteStmtKind = enum
    sskCreate
    sskColumnDef
    sskColumnConstraint

    sskIntLit
    sskStrLit
    sskBlobLit
    sskNullLit
    sskTrueLit
    sskFalseLit
    sskCurrentTime
    sskCurrentDate
    sskCurrentTimestamp



  SqliteColumnConstraintKind = enum
    scckPrimary
    scckNotNull
    scckUnique
    scckCheck
    scckDefault
    scckCollate
    scckGenerated
    scckForeignKey

  SqliteForeignKeyExprKind = enum
    sfekOnDelete
    sfekOnUpdate
    sfekMatch
    sfekDeferrable
    sfekNotDeferrable

  SqliteForeignTriggerKind = enum
    sftkSetNul
    sftkSetDefault
    sftkCascade
    sftkRestrict
    sftkNoAction

  SqliteDeferrableKind = enum
    sdkNone
    sdkInitiallyDeferred
    sdkInitiallyImmediate

  SqliteForeignRefSpec = object
    case kind*: SqliteForeignKeyExprKind
      of sfekOnDelete, sfekOnUpdate:
        triggerKind*: SqliteForeignTriggerKind

      of sfekMatch:
        matchName*: string

      of sfekDeferrable, sfekNotDeferrable:
        deferrableKind*: SqliteDeferrableKind



  SqliteStmt = ref object
    case kind*: SqliteStmtKind
      of sskColumnDef:
        columnName*: string
        columnType*: Option[SqliteStmt]
        columnConstraint*: seq[SqliteStmt]

      of sskColumnConstraint:
        conflictCaluse*: SqliteStmt

        case constraintKind*: SqliteColumnConstraintKind
          of scckPrimary:
            nil

          of scckUnique, scckNotNull:
            nil

          of scckCheck:
            checkExpr*: SqliteStmt

          of scckDefault:
            defaultExpr*: SqliteStmt

          of scckCollate:
            collateName*: string

          of scckGenerated:
            generatedExpr*: SqliteStmt
            isStored*: bool
            isVirtual*: bool

          of scckForeignKey:
            foreignTableName*: string
            columns*: seq[string]
            referenceSpec*: seq[SqliteForeignRefSpec]





      of sskCreate:
        schemaName*: Option[string]
        createName*: string
        case tableAs*: bool
           of true:
             asSelect*: SqliteStmt

           of false:
             columnDef*: seq[SqliteStmt]
             tableConstraint*: seq[SqliteStmt]
             dropRowid*: bool

      of sskNullLit .. sskCurrentTimeStamp:
        nil

      of sskIntLit:
        intVal*: BiggestInt

      of sskStrLit, sskBlobLit:
        strVal*: string


proc sqliteOpen(file: AnyFile): DbConn =
  open(file.getStr(), "", "", "")


type
  SqInt = distinct char
  SqText = distinct char
  SqBlob = distinct char
  SqFloat = distinct char

  SqType = SqInt | SqText | SqBlob | SqFloat

const sqlNone = sql("")
type Sql = SqlQuery

func `?`(str: string): bool = str.len > 0
func `?`[T](o: openarray[T]): bool = o.len > 0

func `%=`(s: var string, s2: string) = s &= s2
func `%=`(s: var string, q: Sql) = s &= q.string


func table*(
    name: string,
    columns: openarray[(string, Sql)],
    constraints: openarray[string] = @[],
    select: Sql = sqlNone,
    ifNot: bool = false,
    isTemp: bool = false,
    schema: string = "",
    noRowId: bool = false
  ): Sql =
  var r: string = "CREATE "

  if isTemp: r %= "TEMPORARY "
  r %= "TABLE "
  if ifNot:
    r %= "IF NOT EXISTS"

  if ?schema: r %= schema & "."
  r %= name & " "

  r %= "(\n  "
  for idx, (name, colType) in columns:
    if idx > 0:
      r %= ",\n  "
    r %= name & " "
    r %= colType

  r %= "\n) "
  if noRowId: r %= "WITHOUT ROWID"

  return Sql(r)

func table*(
    name: string, select: Sql, schema: string = "",
    ifNot: bool = false,
    isTemp: bool = false
  ): Sql =
  var r: string = "CREATE "
  if isTemp: r &= "TEMPORARY "
  r &= "TABLE "
  if ifNot: r &= "IF NOT EXISTS "
  if ?schema: r &= schema & "."
  r &= name
  r &= "AS "
  r %= select

  return Sql(r)

func select*(
    selFrom: string,
    columns: openarray[string],
    all: bool = false,
    dist: bool = false,
    where: Sql = sqlNone,
    group: Sql = sqlNone,
    having: Sql = sqlNone,
    window: Sql = sqlNone,
    withExpr: openarray[Sql] = @[],
    rec: bool = false,
    order: openarray[Sql] = @[],
    limit: Sql = sqlNone,
    offset: Sql = sqlNone
  ): Sql =

  var r: string

  if ?withExpr:
    r %= "WITH "
    if rec:
      r %= "RECURSIVE "

    for idx, expr in withExpr:
      if idx > 0:
        r %= ", "

      r %= expr

  r %= "SELECT "
  if dist: r %= "DISTINCT "
  if all: r %= "ALL "
  r &= columns.join(", ") & " "
  r &= "FROM " & selFrom & " "

  return Sql(r)




type
  KeyDirection = enum
    kdNone = ""
    kdAsc = "ASC"
    kdDesc = "DESC"

  SqlConflict = enum
    sckNone = ""
    sckTrue = ""
    sckRollback = "ROLLBACK"
    sckAbort = "ABORT"
    sckFail = "FAIL"
    sckIgnore = "IGNORE"
    sckReplace = "REPLACE"

func col*(
    rowType: typedesc[SqType],
    primary: bool = false,
    primaryConflict: SqlConflict = sckNone,
    autoincrement: bool = false,
    primaryOrder: KeyDirection = kdNone,
    notNull: SqlConflict = sckNone,
    unique: SqlConflict = sckNone
  ): Sql =

  var r: string
  when rowType is SqInt:
    r &= "INTEGER"

  if primary:
    r %= " PRIMARY KEY "
    r %= $primaryOrder & " "

    if primaryConflict != sckNone:
      r %= $primaryConflict & " "
    if autoincrement:
      r %= "AUTOINCREMENT "

  if notNull != sckNone:
    r %= "NOT NULL "
    r %= $notNull & " "

  if unique != sckNone:
    r %= "UNIQUE "
    r %= $unique & " "

  return Sql(r)

type
  SqlInsertOr = enum
    sioNone
    sioAbort
    sioFail
    sioIgnore
    sioReplace
    sioRollback

type SqlLit = distinct string

template toSqlLit*(str: string): untyped = SqlLit(str)
template toSqlLit*(i: int): untyped = SqlLit($i)

macro sq*(args: varargs[untyped]): untyped =
  result = nnkBracket.newTree()
  for arg in args:
    if arg.eqIdent("?"):
      result.add newCall("SqlLit", newLit("?"))

    else:
      result.add newCall("toSqlLit", arg)


func insert*(
    table: string,
    columns: openarray[string] = @[],
    values: openarray[SqlLit] = @[],

    selectExpr: Sql = sqlNone,
    default: bool = false,

    schema: string = "",
    insAs: string = "",
    insOr: SqlInsertOr = sioNone
  ): Sql =

  var r: string

  r %= "INSERT "
  case insOr:
    of sioNone: discard
    of sioAbort: r %= "OR ABORT"
    of sioFail: r %= "OR FAIL"
    of sioIgnore: r %= "OR IGNORE"
    of sioReplace: r %= "OR REPLACE"
    of sioRollback: r %= "OR ROLLBACK"

  r %= "INTO "
  if ?schema: r %= schema & "."
  r %= table
  r %= " (" & columns.join(", ") & ") "
  r %= "VALUES ("

  for idx, val in values:
    if idx > 0: r %= ", "
    r %= val.string

  r %= ")"

  return Sql(r)

func `$`*(q: Sql): string = q.string



let file = AbsFile("/tmp/db.tmp")

if exists(file): rmFile file

let conn = sqliteOpen(file)

let tab = "test"


let t = tab.table({
  "id": col(SqInt, primary = true, unique = sckTrue)
})

let ins = tab.insert(["id"], sq(123))
let sel = tab.select(["id"])


echo t
echo ins
echo sel

conn.exec(t)
conn.exec ins
let rows = conn.getAllRows(sel)
echo rows
