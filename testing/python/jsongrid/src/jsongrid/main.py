#!/usr/bin/env python
# jsongrid/main.py
from __future__ import annotations

import argparse
import json
import logging
import sys
from pathlib import Path

from beartype import beartype
from PyQt6.QtWidgets import QApplication

from jsongrid.structure import (
    StructureInference,
    TabularityConfig,
    container_item_count,
)
from jsongrid.view import STYLE_SHEET, MainWindow, RenderConfig

log = logging.getLogger(__name__)


@beartype
def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Nested grid viewer for JSON files.")
    parser.add_argument("input", type=Path, help="JSON document to display")
    parser.add_argument("--min-fill-ratio", type=float, default=0.4)
    parser.add_argument("--max-columns", type=int, default=32)
    parser.add_argument("--max-rows", type=int, default=200)
    parser.add_argument("--expand-depth", type=int, default=2)
    parser.add_argument("--max-scalar-chars", type=int, default=160)
    return parser.parse_args()


@beartype
def main() -> None:
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
    )
    arguments = parse_arguments()
    if not arguments.input.is_file():
        raise FileNotFoundError(
            f"Input JSON path {arguments.input} is not an existing file")

    document = json.loads(arguments.input.read_text(encoding="utf-8"))
    inference = StructureInference(
        TabularityConfig(
            min_fill_ratio=arguments.min_fill_ratio,
            max_columns=arguments.max_columns,
        ))
    root = inference.classify(document, "$")
    log.info(
        f"root of {arguments.input} inferred as {root.kind.value} "
        f"with {container_item_count(root) if root.kind.value != 'scalar' else 1} items"
    )

    application = QApplication(sys.argv)
    application.setStyleSheet(STYLE_SHEET)
    window = MainWindow(
        title=f"jsongrid - {arguments.input}",
        node=root,
        config=RenderConfig(
            auto_expand_depth=arguments.expand_depth,
            max_rows=arguments.max_rows,
            max_scalar_chars=arguments.max_scalar_chars,
        ),
    )
    window.show()
    sys.exit(application.exec())


if __name__ == "__main__":
    main()
