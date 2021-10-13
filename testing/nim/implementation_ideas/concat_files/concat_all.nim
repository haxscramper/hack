import std/[os, strutils, strformat, streams]

var stream = newFileStream("result.nim", fmWrite)

for file in walkDir(getCurrentDir()):
  let (dir, name, ext) = file.path.splitFile()
  if file.kind == pcFile and
     ext == ".nim" and
     name notin [ "concat_all", "result" ]:


    stream.writeLine &"{{.line: (file: \"{file.path}\", line: 0)}}:"
    for line in lines(file.path):
      stream.write "  "
      stream.writeLine line

stream.close()
