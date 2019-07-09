#! /usr/bin/bash
find src -name "*.rs" | xargs rustfmt
RUSTFLAGS="$RUSTFLAGS -Awarnings" cargo build 
cp target/debug/v2 .
./v2
