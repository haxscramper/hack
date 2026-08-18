#!/usr/bin/env python

import argparse
import sys
from pathlib import Path

from beartype import beartype
from beartype.typing import Optional
from PyQt6.QtCore import (
    QByteArray,
    Qt,
    pyqtSignal,
    pyqtSlot,
)
from PyQt6.QtWidgets import (
    QApplication,
    QHBoxLayout,
    QLineEdit,
    QPushButton,
    QSizePolicy,
    QSplitter,
    QVBoxLayout,
    QWidget,
)

from ui_utils import (
    FlatSubtreeModel,
    OrgItemView,
    OrgWindow,
    ReloadManager,
    TreeNode,
    collect_subtrees,
    configure_header,
    export_output_path,
    subtree_matches_tags,
)


class TagArea(QWidget):
    close_requested = pyqtSignal(object)
    query_changed = pyqtSignal()
    header_changed = pyqtSignal(object)

    @beartype
    def __init__(
        self,
        query: str,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.subtrees: list[TreeNode] = []

        self.query = QLineEdit(self)
        self.query.setText(query)
        self.query.setPlaceholderText("#tag #parent##child")
        self.query.setClearButtonEnabled(True)
        self.query.textChanged.connect(self.on_query_changed)

        self.close_button = QPushButton("×", self)
        self.close_button.setToolTip("Close area")
        self.close_button.setFixedWidth(32)
        self.close_button.clicked.connect(self.request_close)

        controls = QHBoxLayout()
        controls.setContentsMargins(0, 0, 0, 0)
        controls.addWidget(self.query)
        controls.addWidget(self.close_button)

        self.model = FlatSubtreeModel([], self)

        self.view = OrgItemView(self)
        self.view.setModel(self.model)
        self.view.setRootIsDecorated(False)
        self.view.setItemsExpandable(False)
        self.view.setAlternatingRowColors(True)
        self.view.setUniformRowHeights(True)
        self.view.setSortingEnabled(False)
        configure_header(self.view)

        self.view.header().sectionResized.connect(self.on_header_changed)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)
        layout.addLayout(controls)
        layout.addWidget(self.view)

        self.setSizePolicy(
            QSizePolicy.Policy.Expanding,
            QSizePolicy.Policy.Expanding,
        )

    @beartype
    def query_text(self) -> str:
        return self.query.text()

    @beartype
    def set_subtrees(self, subtrees: list[TreeNode]) -> None:
        self.subtrees = subtrees
        self.refresh()

    @beartype
    def refresh(self) -> None:
        query = self.query.text()
        matching = [
            node for node in self.subtrees
            if subtree_matches_tags(node, query)
        ]
        self.model.replace_nodes(matching)

    @pyqtSlot(str)
    @beartype
    def on_query_changed(self, text: str) -> None:
        self.refresh()
        self.query_changed.emit()

    @pyqtSlot()
    @beartype
    def request_close(self) -> None:
        self.close_requested.emit(self)

    @pyqtSlot(int, int, int)
    @beartype
    def on_header_changed(
        self,
        logical_index: int,
        old_size: int,
        new_size: int,
    ) -> None:
        self.header_changed.emit(self.view.header().saveState())


class MainWindow(OrgWindow):

    @beartype
    def __init__(
        self,
        input_path: Path,
        manager: ReloadManager,
    ) -> None:
        super().__init__(
            input_path,
            manager,
            settings_prefix="flat",
        )
        self.subtrees: list[TreeNode] = []
        self.areas: list[TagArea] = []
        self.restoring_header = False

        self.splitter = QSplitter(
            Qt.Orientation.Vertical,
            self,
        )
        self.splitter.setChildrenCollapsible(False)
        self.splitter.splitterMoved.connect(self.save_splitter_state)

        self.add_button = QPushButton("+", self)
        self.add_button.setToolTip("Add tag area")
        self.add_button.clicked.connect(self.add_empty_area)

        central = QWidget(self)
        layout = QVBoxLayout(central)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.splitter)
        layout.addWidget(self.add_button)
        self.setCentralWidget(central)

        stored_queries = self.settings.value("area_queries", [])
        if isinstance(stored_queries, str):
            stored_queries = [stored_queries]

        for query in stored_queries:
            self.add_area(str(query), save=False)

        if not self.areas:
            self.add_area("", save=False)

        splitter_state = self.settings.value("splitter_state")
        if isinstance(splitter_state, QByteArray):
            self.splitter.restoreState(splitter_state)

        self.setWindowTitle(f"Org subtree areas: {self.input_path}")
        self.resize(1600, 900)

    @beartype
    def replace_document(self, root: TreeNode) -> None:
        self.subtrees = collect_subtrees(root)

        for area in self.areas:
            area.set_subtrees(self.subtrees)

    @pyqtSlot()
    @beartype
    def add_empty_area(self) -> None:
        self.add_area("")

    @beartype
    def add_area(
        self,
        query: str,
        save: bool = True,
    ) -> None:
        area = TagArea(query, self.splitter)
        area.set_subtrees(self.subtrees)
        area.close_requested.connect(self.close_area)
        area.query_changed.connect(self.save_areas)
        area.header_changed.connect(self.save_header_state)
        area.view.subtree_activated.connect(self.open_subtree)

        header_state = self.settings.value("header_state")
        if isinstance(header_state, QByteArray):
            area.view.header().restoreState(header_state)

        self.areas.append(area)
        self.splitter.addWidget(area)

        if save:
            self.save_areas()

        area.query.setFocus()

    @pyqtSlot(object)
    @beartype
    def close_area(self, area: TagArea) -> None:
        self.areas.remove(area)
        area.setParent(None)
        area.deleteLater()
        self.save_areas()

    @pyqtSlot()
    @beartype
    def save_areas(self) -> None:
        self.settings.setValue(
            "area_queries",
            [area.query_text() for area in self.areas],
        )
        self.settings.sync()

    @pyqtSlot(int, int)
    @beartype
    def save_splitter_state(
        self,
        position: int,
        index: int,
    ) -> None:
        self.settings.setValue(
            "splitter_state",
            self.splitter.saveState(),
        )
        self.settings.sync()

    @pyqtSlot(object)
    @beartype
    def save_header_state(self, state: QByteArray) -> None:
        if self.restoring_header:
            return

        self.settings.setValue("header_state", state)
        self.settings.sync()

        self.restoring_header = True
        try:
            for area in self.areas:
                if area.view.header().saveState() != state:
                    area.view.header().restoreState(state)
        finally:
            self.restoring_header = False


@beartype
def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "haxorg_cpp_org_cli",
        type=Path,
        help="Path to the haxorg_cpp_org_cli executable",
    )
    parser.add_argument(
        "input",
        type=Path,
        help="Org-mode document or project input passed to the exporter",
    )
    return parser.parse_args()


@beartype
def main() -> None:
    args = parse_arguments()

    application = QApplication(sys.argv)
    manager = ReloadManager(
        args.haxorg_cpp_org_cli,
        args.input,
        export_output_path(args.input, "flat"),
    )
    window = MainWindow(args.input, manager)

    application.aboutToQuit.connect(manager.stop)
    window.show()
    manager.start()

    sys.exit(application.exec())


if __name__ == "__main__":
    main()
