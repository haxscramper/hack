#!/usr/bin/env python

import sys

from beartype import beartype
from PyQt6.QtWidgets import QApplication, QMainWindow, QTabWidget

from graphviz_viewer.task_graph_v1.task_graph_input import collect_input
from graphviz_viewer.task_graph_v1.task_graph_layout import layout_views
from graphviz_viewer.task_graph_v1.task_graph_model import TaskGraphModel
from graphviz_viewer.task_graph_v1.task_graph_scene import VisualizationPanel
from graphviz_viewer.task_graph_v1.task_graph_semantic_placement import build_semantic_views
from graphviz_viewer.task_graph_v1.task_graph_types import PipelineResult


@beartype
def process_collection() -> PipelineResult:
    collection = collect_input()
    semantic_views = build_semantic_views(collection)
    placed_views = layout_views(semantic_views)
    return PipelineResult(
        source=collection,
        views=placed_views,
    )


class TaskGraphWindow(QMainWindow):

    def __init__(self, result: PipelineResult) -> None:
        super().__init__()
        self.setWindowTitle("Task collection visualization")
        self.resize(1500, 950)

        graph_model = TaskGraphModel(
            result.views.graph,
            result.source,
            self,
        )
        calendar_model = TaskGraphModel(
            result.views.calendar,
            result.source,
            self,
        )

        tabs = QTabWidget()
        tabs.addTab(
            VisualizationPanel(graph_model, calendar_mode=False),
            "Task graph",
        )
        tabs.addTab(
            VisualizationPanel(calendar_model, calendar_mode=True),
            "Calendar",
        )
        self.setCentralWidget(tabs)


@beartype
def main() -> int:
    application = QApplication(sys.argv)
    result = process_collection()
    window = TaskGraphWindow(result)
    window.setVisible(True)
    return application.exec()


if __name__ == "__main__":
    raise SystemExit(main())
