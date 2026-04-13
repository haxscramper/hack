#!/usr/bin/env python

import argparse
import sys
from pathlib import Path

from src.diagrammer.parser import parse
from src.diagrammer.resolver import resolve
from src.diagrammer.renderer import render


def main() -> None:
    ap = argparse.ArgumentParser(
        description="DSL to ASCII/Unicode diagram renderer")
    ap.add_argument("input", type=Path, help="Input DSL file")
    ap.add_argument("-o",
                    "--output",
                    type=Path,
                    required=True,
                    help="Output file")
    ap.add_argument(
        "--charset",
        choices=["ascii", "unicode"],
        default="unicode",
        help="Character set for drawing (default: unicode)",
    )
    ap.add_argument(
        "--scale",
        type=float,
        default=1.0,
        help="Scale factor: 1 unit = scale characters (default: 1.0)",
    )
    ap.add_argument(
        "--canvas-width",
        type=float,
        default=None,
        help="Canvas width (inferred if not set)",
    )
    ap.add_argument(
        "--canvas-height",
        type=float,
        default=None,
        help="Canvas height (inferred if not set)",
    )
    args = ap.parse_args()

    source = args.input.read_text()
    statements = parse(source)
    scene = resolve(statements,
                    canvas_width=args.canvas_width,
                    canvas_height=args.canvas_height)
    output = render(scene, charset=args.charset, scale=args.scale)
    args.output.write_text(output)


if __name__ == "__main__":
    main()
