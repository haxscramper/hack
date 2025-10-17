#!/usr/bin/env bash 

set -e
set -o nounset
set -o errexit
set -x 

./scripts/run.sh --input=src/main/resources/sample-graph.json --output=src/main/resources/sample-output.json
