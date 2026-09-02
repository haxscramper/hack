#!/usr/bin/env python

import argparse
import sys
from pathlib import Path

from PyQt6.QtWidgets import QApplication

from graphviz_viewer.graph_viewer_v2.graph_viewer_executor import GraphvizLayoutExecutor
from graphviz_viewer.graph_viewer_v2.graph_viewer_graph import GraphvizGraphProvider
from graphviz_viewer.graph_viewer_v2.graph_viewer_layout import EdgeLabelLayoutHierarchyMapper
from graphviz_viewer.graph_viewer_v2.graph_viewer_model import GraphLayoutModel, GraphRole
from graphviz_viewer.graph_viewer_v2.graph_viewer_window import MainWindow


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

    mapper = EdgeLabelLayoutHierarchyMapper()
    layout_hierarchy = mapper.map(source_hierarchy)

    executor = GraphvizLayoutExecutor()
    rank_direction = source_hierarchy.properties.get("rankdir", "TB")
    rank_direction = str(rank_direction).strip('"').upper()

    if rank_direction not in {"TB", "BT", "LR", "RL"}:
        rank_direction = "TB"

    executor.execute(layout_hierarchy, rank_direction)
    model = GraphLayoutModel(layout_hierarchy)
    print(f"source root: {source_hierarchy.unique_id}")
    print(f"source nodes: {len(source_hierarchy.nodes)}")
    print(f"source edges: {len(source_hierarchy.edges)}")
    print(f"source clusters: {len(source_hierarchy.clusters)}")
    print(f"layout nodes: {len(layout_hierarchy.nodes)}")
    print(f"layout edges: {len(layout_hierarchy.edges)}")
    print(f"layout clusters: {len(layout_hierarchy.clusters)}")
    print(f"model root rows: {model.rowCount()}")
    print(f"root index valid: {model.index(0, 0).isValid()}")
    print(f"root rectangle: {layout_hierarchy.rectangle}")

    window = MainWindow(
        model,
        executor,
        f"Graphviz Qt Viewer — {arguments.graph.name}",
    )

    root_index = model.index(0, 0)

    print(f"root display: {model.data(root_index)}")
    print(f"root kind: {model.data(root_index, GraphRole.ElementKind)}")
    print(f"root child rows: {model.rowCount(root_index)}")
    print(f"scene items: {len(window.graph_view.scene().items())}")
    print(f"node items: {len(window.graph_view.node_items)}")
    print(f"edge items: {len(window.graph_view.edge_items)}")
    print(f"scene bounds: {window.graph_view.scene().itemsBoundingRect()}")

    window.configuration.rank_direction.setCurrentText(rank_direction)
    window.show()

    return application.exec()


if __name__ == "__main__":
    raise SystemExit(main())
