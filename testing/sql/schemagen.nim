import
    hmisc/hasts/graphviz_ast,
    hmisc/other/[oswrap, hshell, hjson],
    hmisc/[helpers, hdebug_misc],
    std/[strformat, parsesql, hashes],
    fusion/matching


let code = RelFile("haxdoc.sql").readFile() & "\n" & """
SELECT * FROM sqlite_master;
"""

let dump = runShell(shellCmd(sqlite3, -json), stdin = code).stdout.parseJson()

var graph = makeDotGraph(dgpRecords)

iterator items*(sql: SqlNode): SqlNode =
  for i in 0 ..< sql.len:
    yield sql[i]

for record in dump:
  if record.matches({
    "type": (getStr: "table"),
    "sql": @expr
  }):
    let parsed = expr.asStr().parseSql()
    parsed[0].assertMatch(CreateTable[
      (strVal: @tableName),
      all @columns
    ])

    let name = &"[[<b> {tableName} </b>]]"

    var record = makeTableDotNode(hash(tableName), name)

    let w = name.len

    for col in columns:
      var id = col[0].strVal.hash()
      record.add makeDotNode(
        id,
        RawHtml(
          alignLeft(
            col[0].strVal & ":",
            w - col[1].strVal.len - 1,
          ) & &"<i>{col[1].strVal}</i>"
        )
      )

      for sub in col:
        if sub.kind == nkReferences:
          graph.add makeDotEdge(
            (hash(tableName), id),
            (hash(sub[0][0].strVal), hash(sub[0][1].strVal))
          )

    graph.add record


graph.toPng(AbsFile("/tmp/schema.png"))
