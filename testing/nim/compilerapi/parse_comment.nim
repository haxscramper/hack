import hnimast

const str = """
type
  MandoclevelC* {.importc: "mandoclevel", header: r"/tmp/a.c".} = enum ## /*
                                                  ##  * Status level.  This refers to both internal status
                                                  ##  */

    mandoclevel_MANDOCLEVEL_OK = 0,
    mandoclevel_MANDOCLEVEL_STYLE = 1, ## /* style suggestions */
"""

var node = parsePNodeStr(str)
echo treeRepr(node, indexed = true)

node[0][0][2].comment = "Status level.  This refers to both internal status"

echo node
