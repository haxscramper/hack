import os

proc `/`(up, down: string): string = joinpath(up, down)

echo "a" / "b"
echo "a" / "b" / "../c"
echo "a//" / "//b"
echo "/t/b/c" / "/d"
