#!/usr/bin/env bash
set -e
set -o nounset
set -o errexit
set -x 

TOOL_VERSIONS_FILE=".tool-versions"

asdf install
