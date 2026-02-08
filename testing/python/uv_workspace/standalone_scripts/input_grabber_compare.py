#!/usr/bin/env python3

import subprocess
import os

input_dir = "/home/haxscramper/defaultdirs/input/"
grabber_dir = "/home/haxscramper/defaultdirs/input/grabber"

# Image extensions to search for
image_extensions = ["jpg", "jpeg", "png", "gif", "bmp", "webp", "tiff", "svg"]

# Build fsearch query for images
# Using regex to match image extensions
ext_pattern = "|".join(image_extensions)
query = f'path:"{input_dir}" ext:regex:({ext_pattern})'

# Run fsearch in CLI mode
result = subprocess.run(
    ["fsearch", "--query", query],
    capture_output=True,
    text=True
)

# Filter out files that are in grabber directory
for line in result.stdout.strip().split('\n'):
    if line and not line.startswith(grabber_dir):
        print(line)
