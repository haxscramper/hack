#!/usr/bin/env python

from pathlib import Path

input_dir = Path.home() / "defaultdirs/input"

with open(input_dir / "grabber/missing_files", "r") as file:
    for line in file:
        file_id = line.strip()
        file_path = input_dir / f"{file_id}.png"
        if file_path.is_file():
            print(file_path)
