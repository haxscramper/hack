#!/usr/bin/env bash
set -euo pipefail

protoc --python_out=. trace_event.proto
uv run python ebpf_tracker_visual.py /tmp/tracking.bin
