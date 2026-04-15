"""Command-line interface.

The CLI intentionally remains small:
- read a JSON file
- validate and resolve
- print resolved JSON to stdout
"""

from __future__ import annotations

import argparse
import sys

from .resolver import resolve_json_file, resolved_to_json
from beartype import beartype

@beartype
def main(argv: list[str] | None = None) -> int:
    """CLI entrypoint."""
    parser = argparse.ArgumentParser(prog="diagram-layout")
    parser.add_argument("input", help="Path to input JSON diagram file.")
    args = parser.parse_args(argv)

    resolved = resolve_json_file(args.input)
    sys.stdout.write(resolved_to_json(resolved))
    sys.stdout.write("\n")
    return 0
