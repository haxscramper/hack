#!/usr/bin/env python

import argparse
import logging
import sys
from pathlib import Path

from diagrammer.parser import parse
from diagrammer.resolver import resolve
from diagrammer.renderer import render
from beartype import beartype

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
)

@beartype
def main() -> None:
    ap = argparse.ArgumentParser(
        description="DSL to ASCII/Unicode diagram renderer")
    ap.add_argument("input", type=Path, help="Input DSL file")
    ap.add_argument("-o",
                    "--output",
                    type=Path,
                    default=None,
                    required=False,
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
    ap.add_argument(
        "--log-level",
        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
        default="INFO",
        help="Logging level (default: INFO)",
    )
    args = ap.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.log_level),
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        stream=sys.stderr,
    )

    source = args.input.read_text()
    logging.debug("Read source file: %s", args.input)
    statements = parse(source)
    logging.debug("Parsed %d statements", len(statements))
    logging.info("Statements: %s", statements)
    scene = resolve(statements,
                    canvas_width=args.canvas_width,
                    canvas_height=args.canvas_height)
    output = render(scene, charset=args.charset, scale=args.scale)
    if args.output:
        args.output.write_text(output)
    else:
        print(output)

    logging.info("Output written to %s", args.output)


if __name__ == "__main__":
    main()
