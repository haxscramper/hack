#!/usr/bin/env bash 

set -e
set -o nounset
set -o errexit
set -x 

. "$HOME/.asdf/asdf.sh"

# Check if built
if [ ! -f "build/install/elk-graph-layout-kotlin/bin/elk-graph-layout-kotlin" ]; then
  echo "Application not built. Building now..."
  ./scripts/build.sh
fi

echo "Running ELK Graph Layout application..."
echo "Arguments: $*"

# Run the application with all passed arguments
build/install/elk-graph-layout-kotlin/bin/elk-graph-layout-kotlin "$@"
