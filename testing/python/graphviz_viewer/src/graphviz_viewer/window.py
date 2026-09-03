#!/usr/bin/env python

from __future__ import annotations

from PyQt6.QtCore import QModelIndex, Qt, pyqtSignal
from PyQt6.QtGui import QAction, QFont
from PyQt6.QtWidgets import (
    QAbstractItemView,
    QApplication,
    QComboBox,
    QFormLayout,
    QHeaderView,
    QLabel,
    QMainWindow,
    QMessageBox,
    QSplitter,
    QTableWidget,
    QTableWidgetItem,
    QTreeView,
    QVBoxLayout,
    QWidget,
)

from graphviz_viewer.graph_viewer_v2.model import GraphRole
from graphviz_viewer.graph_viewer_v2.view import GraphView


class PropertyPanel(QWidget):

    def __init__(self) -> None:
        super().__init__()
        layout = QVBoxLayout(self)

        title = QLabel("Element properties")
        font = QFont(title.font())
        font.setBold(True)
        title.setFont(font)
        layout.addWidget(title)

        self.properties = QTableWidget(0, 2)
        self.properties.setHorizontalHeaderLabels(["Property", "Value"])
        self.properties.setEditTriggers(
            QAbstractItemView.EditTrigger.NoEditTriggers)
        self.properties.verticalHeader().setVisible(False)
        self.properties.horizontalHeader().setSectionResizeMode(
            0,
            QHeaderView.ResizeMode.ResizeToContents,
        )
        self.properties.horizontalHeader().setSectionResizeMode(
            1,
            QHeaderView.ResizeMode.Stretch,
        )
        layout.addWidget(self.properties)

    def show_index(self, index: QModelIndex) -> None:
        properties = index.data(GraphRole.Properties) or {}
        rows = [
            ("id", index.data(GraphRole.Element).unique_id),
            ("kind", index.data(GraphRole.ElementKind)),
            (
                "underlying IDs",
                ", ".join(index.data(GraphRole.RelatedUnderlyingIds)),
            ),
            *sorted(
                (str(key), str(value)) for key, value in properties.items()),
        ]

        self.properties.setRowCount(len(rows))

        for row, (key, value) in enumerate(rows):
            self.properties.setItem(row, 0, QTableWidgetItem(key))
            self.properties.setItem(row, 1, QTableWidgetItem(value))

        self.properties.resizeRowsToContents()


class ConfigurationPanel(QWidget):
    rankDirectionChanged = pyqtSignal(str)

    def __init__(self, model: GraphLayoutModel) -> None:
        super().__init__()
        layout = QVBoxLayout(self)

        title = QLabel("Hierarchy")
        font = QFont(title.font())
        font.setBold(True)
        title.setFont(font)
        layout.addWidget(title)

        self.rank_direction = QComboBox()
        self.rank_direction.addItems(["TB", "BT", "LR", "RL"])
        self.rank_direction.currentTextChanged.connect(
            self.rankDirectionChanged)

        form = QFormLayout()
        form.addRow("Rank direction", self.rank_direction)
        layout.addLayout(form)

        self.tree = QTreeView()
        self.tree.setModel(model)
        self.tree.setHeaderHidden(True)
        self.tree.expandToDepth(2)
        layout.addWidget(self.tree)


class MainWindow(QMainWindow):

    def __init__(
        self,
        model: GraphLayoutModel,
        executor: LayoutExecutor,
        title: str,
    ) -> None:
        super().__init__()
        self.model = model
        self.executor = executor

        self.setWindowTitle(title)
        self.resize(1500, 900)

        self.configuration = ConfigurationPanel(model)
        self.graph_view = GraphView()
        self.properties = PropertyPanel()

        splitter = QSplitter(Qt.Orientation.Horizontal)
        splitter.addWidget(self.configuration)
        splitter.addWidget(self.graph_view)
        splitter.addWidget(self.properties)
        splitter.setSizes([280, 950, 320])
        splitter.setStretchFactor(1, 1)
        self.setCentralWidget(splitter)

        self.graph_view.set_model(model)
        self.graph_view.elementSelected.connect(self._select_index)
        self.configuration.tree.selectionModel().currentChanged.connect(
            self._select_index)
        self.configuration.rankDirectionChanged.connect(
            self._set_rank_direction)

        quit_action = QAction("&Quit", self)
        quit_action.setShortcut("Ctrl+Q")
        quit_action.triggered.connect(self.close)

        file_menu = self.menuBar().addMenu("&File")
        file_menu.addAction(quit_action)

    def _select_index(self, index: QModelIndex) -> None:
        if not index.isValid():
            return

        self.properties.show_index(index)
        self.configuration.tree.setCurrentIndex(index)

    def _set_rank_direction(self, direction: str) -> None:
        QApplication.setOverrideCursor(Qt.CursorShape.WaitCursor)

        try:
            self.executor.execute(self.model.root, direction)
            self.model.rebuild()
            self.graph_view.set_model(self.model)
        except Exception as error:
            QMessageBox.critical(
                self,
                "Unable to compute layout",
                str(error),
            )
        finally:
            QApplication.restoreOverrideCursor()
