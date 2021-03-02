#!/usr/bin/env elvish

/bin/rm -rf cache

try {
  nim cpp main.nim
  try {
    ./main
  } except {
    gbd -batch -ex "run" -ex "bt" main
  }
} except { }

for file [cache/*.cpp] {
  clang -E -P ^
    -I/mnt/workspace/github/hax-nim/wraptests/callback_override ^
    -I/home/test/.choosenim/toolchains/nim-1.4.2/lib $file |
    clang-format - > $file".expanded.cpp"
}
