#!/usr/bin/env bash
set -e
set -o nounset
set -o errexit
set -x 

. "$HOME/.asdf/asdf.sh"

echo "Building Kotlin application..."

# Ensure we're using the correct tool versions
if [ -f ".tool-versions" ]; then
  echo "Using tool versions from .tool-versions"
else
  echo "Warning: .tool-versions not found. Run scripts/setup.sh first."
  exit 1
fi

# Clean and build
echo "Cleaning previous build..."
gradle clean

echo "Building application..."
gradle build

echo "Creating distribution..."
gradle installDist

echo "Build completed successfully!"
echo "Executable available at: build/install/elk-graph-layout-kotlin/bin/elk-graph-layout-kotlin"
