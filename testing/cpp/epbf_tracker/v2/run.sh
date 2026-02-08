#!/usr/bin/env bash
set -euo pipefail

meson setup build --wipe
meson compile -C build
sudo ./build/elvish_ebpf
