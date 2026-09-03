#!/usr/bin/env python

import argparse
import sys
from pathlib import Path

from PyQt6.QtWidgets import QApplication

from graphviz_viewer.graph_viewer_v2.executor import GraphvizLayoutExecutor
from graphviz_viewer.graph_viewer_v2.graph import GraphvizGraphProvider
from graphviz_viewer.graph_viewer_v2.layout_mapper_components import ConnectedComponentLayoutHierarchyMapper
from graphviz_viewer.graph_viewer_v2.layout_mapper_label_nodes import EdgeLabelLayoutHierarchyMapper
from graphviz_viewer.graph_viewer_v2.model import GraphLayoutModel, GraphRole
from graphviz_viewer.graph_viewer_v2.window import MainWindow


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Interactive PyQt6 Graphviz graph viewer")
    parser.add_argument(
        "graph",
        type=Path,
        help="Graphviz DOT input file",
    )
    return parser.parse_args()


def main() -> int:
    arguments = parse_arguments()

    application = QApplication(sys.argv)
    application.setApplicationName("Graphviz Qt Viewer")

    provider = GraphvizGraphProvider()
    source_hierarchy = provider.read(arguments.graph)

    mapper = ConnectedComponentLayoutHierarchyMapper()
    layout_hierarchy = mapper.map(source_hierarchy)

    executor = GraphvizLayoutExecutor()
    rank_direction = source_hierarchy.properties.get("rankdir", "TB")
    rank_direction = str(rank_direction).strip('"').upper()

    if rank_direction not in {"TB", "BT", "LR", "RL"}:
        rank_direction = "TB"

    executor.execute(layout_hierarchy, rank_direction)
    model = GraphLayoutModel(layout_hierarchy)
    window = MainWindow(
        model,
        executor,
        f"Graphviz Qt Viewer — {arguments.graph.name}",
    )

    root_index = model.index(0, 0)
    window.configuration.rank_direction.setCurrentText(rank_direction)
    window.show()

    return application.exec()


if __name__ == "__main__":
    raise SystemExit(main())
