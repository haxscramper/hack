import macros

type
  Lexer = object
    buf: seq[string]
    idx: int

func `[]`(l: Lexer, idx: static[int]): string = l.buf[l.idx + idx]
func `[]`(l: Lexer, idx: int = l.idx): string = l.buf[idx]
func next(l: var Lexer) = inc l.idx

var buf = Lexer(buf: @["hello", "world", "nice"])

echo buf[+2]
buf.next()
echo buf[-1]
for i in 0 ..< buf.buf.len:
  echo buf[i]
