#!/usr/bin/env python

import argparse
import sys
from pathlib import Path

from beartype import beartype
from PyQt6.QtCore import (
    QModelIndex,
    QSignalBlocker,
    QSortFilterProxyModel,
    Qt,
    pyqtSlot,
)
from PyQt6.QtWidgets import (
    QApplication,
    QLineEdit,
    QVBoxLayout,
    QWidget,
)

from ui_utils import (
    NodeKind,
    OrgTreeModel,
    OrgTreeView,
    OrgWindow,
    ReloadManager,
    TreeNode,
    configure_header,
    export_output_path,
)


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
            settings_prefix="tree",
        )

        stored_paths = self.settings.value("expanded_paths", [])
        if isinstance(stored_paths, str):
            stored_paths = [stored_paths]

        self.expanded_paths = {str(path) for path in stored_paths}

        root = TreeNode(
            kind=NodeKind.ROOT,
            name=self.input_path.name,
            identity=f"root:{self.input_path}",
            source_path=self.input_path,
        )
        self.model = OrgTreeModel(root, self)

        self.proxy = QSortFilterProxyModel(self)
        self.proxy.setSourceModel(self.model)
        self.proxy.setFilterKeyColumn(1)
        self.proxy.setFilterCaseSensitivity(Qt.CaseSensitivity.CaseInsensitive)
        self.proxy.setRecursiveFilteringEnabled(True)

        self.search = QLineEdit(self)
        self.search.setPlaceholderText("Search subtree names...")
        self.search.setClearButtonEnabled(True)
        self.search.textChanged.connect(self.apply_filter)

        self.view = OrgTreeView(self)
        self.view.setModel(self.proxy)
        self.view.setAlternatingRowColors(True)
        self.view.setUniformRowHeights(True)
        self.view.setSortingEnabled(False)
        self.view.expanded.connect(self.on_expanded)
        self.view.collapsed.connect(self.on_collapsed)
        self.view.subtree_activated.connect(self.open_subtree)
        self.view.expand_all_requested.connect(self.expand_all)
        self.view.collapse_all_requested.connect(self.collapse_all)

        configure_header(self.view)

        header_state = self.settings.value("header_state")
        if header_state is not None:
            self.view.header().restoreState(header_state)

        self.view.header().sectionResized.connect(self.on_section_resized)

        central = QWidget(self)
        layout = QVBoxLayout(central)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.search)
        layout.addWidget(self.view)
        self.setCentralWidget(central)

        self.setWindowTitle(f"Org tree: {self.input_path}")
        self.resize(1600, 900)

    @beartype
    def replace_document(self, root: TreeNode) -> None:
        self.model.replace_root(root)
        self.apply_filter(self.search.text())

    @beartype
    def node_at(self, index: QModelIndex) -> TreeNode:
        source_index = self.proxy.mapToSource(index)
        return self.model.node_at(source_index)

    @beartype
    def expansion_path(self, node: TreeNode) -> str:
        names: list[str] = []
        current = node

        while current.container is not None:
            names.append(current.name)
            current = current.container

        names.reverse()
        return "\x1f".join(names)

    @beartype
    def save_expansion(self) -> None:
        self.settings.setValue(
            "expanded_paths",
            sorted(self.expanded_paths),
        )
        self.settings.sync()

    @pyqtSlot(QModelIndex)
    @beartype
    def on_expanded(self, index: QModelIndex) -> None:
        self.expanded_paths.add(self.expansion_path(self.node_at(index)))
        self.save_expansion()

    @pyqtSlot(QModelIndex)
    @beartype
    def on_collapsed(self, index: QModelIndex) -> None:
        self.expanded_paths.discard(self.expansion_path(self.node_at(index)))
        self.save_expansion()

    @beartype
    def restore_expansion(self, parent: QModelIndex) -> None:
        for row in range(self.proxy.rowCount(parent)):
            index = self.proxy.index(row, 0, parent)
            node = self.node_at(index)

            if self.expansion_path(node) in self.expanded_paths:
                self.view.setExpanded(index, True)

            self.restore_expansion(index)

    @pyqtSlot(str)
    @beartype
    def apply_filter(self, text: str) -> None:
        self.proxy.setFilterFixedString(text)

        blocker = QSignalBlocker(self.view)
        self.view.collapseAll()

        if text:
            self.view.expandAll()
        else:
            self.restore_expansion(QModelIndex())

        del blocker

    @pyqtSlot()
    @beartype
    def expand_all(self) -> None:
        self.view.expandAll()

    @pyqtSlot()
    @beartype
    def collapse_all(self) -> None:
        self.view.collapseAll()

    @pyqtSlot(int, int, int)
    @beartype
    def on_section_resized(
        self,
        logical_index: int,
        old_size: int,
        new_size: int,
    ) -> None:
        self.settings.setValue(
            "header_state",
            self.view.header().saveState(),
        )
        self.settings.sync()


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
        export_output_path(args.input, "tree"),
    )
    window = MainWindow(args.input, manager)

    application.aboutToQuit.connect(manager.stop)
    window.show()
    manager.start()

    sys.exit(application.exec())


if __name__ == "__main__":
    main()
