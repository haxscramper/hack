#! /usr/bin/bash
find src -name "*.rs" | xargs rustfmt
cargo build
cp target/debug/v2 .
