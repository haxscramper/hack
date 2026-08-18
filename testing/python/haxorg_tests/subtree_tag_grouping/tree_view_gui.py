#!/usr/bin/env python

import argparse
import hashlib
import json
import sys
import traceback
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
from pathlib import Path

import betterproto2
from beartype import beartype
from beartype.typing import Any, Optional
from loguru import logger
from plumbum import local
from PyQt6.QtCore import (
    QAbstractItemModel,
    QFileSystemWatcher,
    QModelIndex,
    QObject,
    QThread,
    QTimer,
    Qt,
    pyqtSignal,
    pyqtSlot,
)
from PyQt6.QtGui import QMouseEvent
from PyQt6.QtWidgets import (
    QApplication,
    QHeaderView,
    QMainWindow,
    QMessageBox,
    QTreeView,
)

from gen import orgproto as proto
from utils import *


class NodeKind(Enum):
    ROOT = "Root"
    DIRECTORY = "Directory"
    FILE = "File"
    SUBTREE = "Subtree"


@dataclass
class TreeNode:
    kind: NodeKind
    name: str
    identity: str
    container: Optional["TreeNode"] = None
    nested: list["TreeNode"] = field(default_factory=list)
    summary: Optional[SubtreeSummary] = None
    source_path: Optional[Path] = None
    line: Optional[int] = None
    column: Optional[int] = None


@beartype
def extract_subtree(subtree: proto.Subtree, now: datetime) -> SubtreeSummary:
    tag_paths: list[list[str]] = []
    for tag in subtree.tags:
        tag_paths.extend(expand_hashtag(tag.text))

    clocked, last_clocked = clock_stats(subtree.logbook)
    scheduled = user_time_to_datetime(subtree.scheduled)
    delta = int((scheduled - now).total_seconds()) if scheduled else None

    return SubtreeSummary(
        title=paragraph_text(subtree.title),
        clocked_seconds=clocked,
        created=extract_created(subtree.properties),
        deadline=to_iso(subtree.deadline),
        closed=to_iso(subtree.closed),
        tags=tag_paths,
        last_clocked=last_clocked,
        todo=subtree.todo or None,
        effort_minutes=extract_effort(subtree.properties),
        priority=subtree.priority or None,
        scheduled=scheduled.isoformat() if scheduled else None,
        scheduled_delta_seconds=delta,
    )


@beartype
def location_line(value: Any) -> Optional[int]:
    loc = getattr(value, "loc", None)
    return loc.line if loc is not None else None


@beartype
def location_column(value: Any) -> Optional[int]:
    loc = getattr(value, "loc", None)
    return loc.column if loc is not None else None


@beartype
def resolved_file_path(value: proto.File, input_path: Path) -> Path:
    if value.abs_path:
        return Path(value.abs_path)

    if value.rel_path:
        return (input_path.parent / value.rel_path).resolve()

    return input_path.resolve()


@beartype
def append_nodes(
    node: proto.AnyNode,
    container: TreeNode,
    source_path: Path,
    now: datetime,
) -> None:
    kind, value = betterproto2.which_one_of(node, "kind")

    match value:
        case proto.Directory():
            name = value.rel_path or value.abs_path or "Directory"
            identity = (f"{container.identity}/directory:"
                        f"{value.abs_path or value.rel_path}")
            entry = TreeNode(
                kind=NodeKind.DIRECTORY,
                name=name,
                identity=identity,
                container=container,
                source_path=source_path,
                line=location_line(value),
                column=location_column(value),
            )
            container.nested.append(entry)

            for nested in value.subnodes:
                append_nodes(nested, entry, source_path, now)

        case proto.File():
            file_path = resolved_file_path(value, source_path)
            name = value.rel_path or value.abs_path or file_path.name
            identity = f"{container.identity}/file:{file_path}"
            entry = TreeNode(
                kind=NodeKind.FILE,
                name=name,
                identity=identity,
                container=container,
                source_path=file_path,
                line=location_line(value),
                column=location_column(value),
            )
            container.nested.append(entry)

            for nested in value.subnodes:
                append_nodes(nested, entry, file_path, now)

        case proto.Subtree():
            summary = extract_subtree(value, now)
            line = location_line(value)
            column = location_column(value)
            identity = (f"{container.identity}/subtree:{source_path}:"
                        f"{line}:{column}:{summary.title}")
            entry = TreeNode(
                kind=NodeKind.SUBTREE,
                name=summary.title,
                identity=identity,
                container=container,
                summary=summary,
                source_path=source_path,
                line=line,
                column=column,
            )
            container.nested.append(entry)

            for nested in value.subnodes:
                append_nodes(nested, entry, source_path, now)

        case _:
            for nested in getattr(value, "subnodes", []):
                append_nodes(nested, container, source_path, now)


@beartype
def build_tree(document: proto.AnyNode, input_path: Path) -> TreeNode:
    root = TreeNode(
        kind=NodeKind.ROOT,
        name=input_path.name,
        identity=f"root:{input_path.resolve()}",
        source_path=input_path.resolve(),
    )
    append_nodes(
        document,
        root,
        input_path.resolve(),
        datetime.now(tz=timezone.utc),
    )
    return root


class OrgTreeModel(QAbstractItemModel):
    headers = [
        "Type",
        "Title / Path",
        "TODO",
        "Priority",
        "Tags",
        "Clocked Seconds",
        "Effort Minutes",
        "Created",
        "Scheduled",
        "Scheduled Delta",
        "Deadline",
        "Closed",
        "Last Clocked",
        "Line",
        "Column",
    ]

    def __init__(self,
                 root: TreeNode,
                 parent: Optional[QObject] = None) -> None:
        super().__init__(parent)
        self.root = root

    def index(
            self,
            row: int,
            column: int,
            parent: QModelIndex = QModelIndex(),
    ) -> QModelIndex:
        container = self.node_at(parent)
        if not 0 <= row < len(container.nested):
            return QModelIndex()

        if not 0 <= column < len(self.headers):
            return QModelIndex()

        return self.createIndex(row, column, container.nested[row])

    def parent(self, index: QModelIndex) -> QModelIndex:
        if not index.isValid():
            return QModelIndex()

        node = self.node_at(index)
        container = node.container
        if container is None or container is self.root:
            return QModelIndex()

        grand = container.container
        if grand is None:
            return QModelIndex()

        row = grand.nested.index(container)
        return self.createIndex(row, 0, container)

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        if parent.isValid() and parent.column() != 0:
            return 0

        return len(self.node_at(parent).nested)

    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return len(self.headers)

    def data(
            self,
            index: QModelIndex,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ) -> Any:
        if not index.isValid():
            return None

        if role != Qt.ItemDataRole.DisplayRole:
            return None

        node = self.node_at(index)
        return self.cell_value(node, index.column())

    def headerData(
            self,
            section: int,
            orientation: Qt.Orientation,
            role: int = int(Qt.ItemDataRole.DisplayRole),
    ) -> Any:
        if orientation != Qt.Orientation.Horizontal:
            return None

        if role != Qt.ItemDataRole.DisplayRole:
            return None

        if not 0 <= section < len(self.headers):
            return None

        return self.headers[section]

    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        if not index.isValid():
            return Qt.ItemFlag.NoItemFlags

        return (Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable)

    @beartype
    def node_at(self, index: QModelIndex) -> TreeNode:
        if index.isValid():
            return index.internalPointer()

        return self.root

    @beartype
    def replace_root(self, root: TreeNode) -> None:
        self.beginResetModel()
        self.root = root
        self.endResetModel()

    @beartype
    def cell_value(self, node: TreeNode, column: int) -> Any:
        summary = node.summary

        match column:
            case 0:
                return node.kind.value
            case 1:
                return node.name
            case 2:
                return summary.todo if summary else None
            case 3:
                return summary.priority if summary else None
            case 4:
                if summary is None:
                    return None
                return " ".join(f"#{'##'.join(path)}" for path in summary.tags)
            case 5:
                return summary.clocked_seconds if summary else None
            case 6:
                return summary.effort_minutes if summary else None
            case 7:
                return summary.created if summary else None
            case 8:
                return summary.scheduled if summary else None
            case 9:
                return summary.scheduled_delta_seconds if summary else None
            case 10:
                return summary.deadline if summary else None
            case 11:
                return summary.closed if summary else None
            case 12:
                return summary.last_clocked if summary else None
            case 13:
                return node.line
            case 14:
                return node.column
            case _:
                return None


class OrgTreeView(QTreeView):
    subtree_activated = pyqtSignal(object)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        index = self.indexAt(event.position().toPoint())

        if (event.button() == Qt.MouseButton.LeftButton
                and bool(event.modifiers()
                         & Qt.KeyboardModifier.ControlModifier)
                and index.isValid()):
            model = self.model()
            if isinstance(model, OrgTreeModel):
                node = model.node_at(index)
                if node.kind is NodeKind.SUBTREE:
                    self.subtree_activated.emit(node)

        super().mousePressEvent(event)


class ExportWorker(QObject):
    completed = pyqtSignal(bytes)
    failed = pyqtSignal(str)

    @pyqtSlot(str, str, str)
    @beartype
    def export(
        self,
        cli_path: str,
        input_path: str,
        output_path: str,
    ) -> None:
        try:
            command = local[cli_path]
            command[
                "export",
                "--input",
                input_path,
                "--output",
                output_path,
                "proto",
                "--format",
                "Binary",
            ]()
            self.completed.emit(Path(output_path).read_bytes())
        except Exception:
            self.failed.emit(traceback.format_exc())


class ReloadManager(QObject):
    document_ready = pyqtSignal(bytes)
    reload_started = pyqtSignal()
    reload_failed = pyqtSignal(str)
    request_export = pyqtSignal(str, str, str)

    @beartype
    def __init__(
        self,
        cli_path: Path,
        input_path: Path,
        output_path: Path,
        parent: Optional[QObject] = None,
    ) -> None:
        super().__init__(parent)
        self.cli_path = cli_path.resolve()
        self.input_path = input_path.resolve()
        self.output_path = output_path
        self.busy = False
        self.pending = False
        self.last_digest = self.source_digest()

        self.watcher = QFileSystemWatcher(
            [
                str(self.input_path),
                str(self.input_path.parent),
            ],
            self,
        )
        self.watcher.fileChanged.connect(self.on_fs_change)
        self.watcher.directoryChanged.connect(self.on_fs_change)

        self.timer = QTimer(self)
        self.timer.setSingleShot(True)
        self.timer.setInterval(300)
        self.timer.timeout.connect(self.check_source)

        self.thread = QThread(self)
        self.worker = ExportWorker()
        self.worker.moveToThread(self.thread)
        self.request_export.connect(self.worker.export)
        self.worker.completed.connect(self.on_completed)
        self.worker.failed.connect(self.on_failed)
        self.thread.start()

    @beartype
    def start(self) -> None:
        self.begin_export()

    @pyqtSlot(str)
    @beartype
    def on_fs_change(self, changed_path: str) -> None:
        logger.debug(f"Filesystem change detected at {changed_path}")
        self.timer.start()

    @pyqtSlot()
    @beartype
    def check_source(self) -> None:
        input_text = str(self.input_path)
        if input_text not in self.watcher.files():
            self.watcher.addPath(input_text)

        digest = self.source_digest()
        if digest == self.last_digest:
            return

        self.last_digest = digest
        if self.busy:
            self.pending = True
        else:
            self.begin_export()

    @beartype
    def source_digest(self) -> bytes:
        return hashlib.sha256(self.input_path.read_bytes()).digest()

    @beartype
    def begin_export(self) -> None:
        self.busy = True
        self.reload_started.emit()
        self.request_export.emit(
            str(self.cli_path),
            str(self.input_path),
            str(self.output_path),
        )

    @pyqtSlot(bytes)
    @beartype
    def on_completed(self, data: bytes) -> None:
        self.busy = False
        self.document_ready.emit(data)
        self.run_pending()

    @pyqtSlot(str)
    @beartype
    def on_failed(self, message: str) -> None:
        self.busy = False
        self.reload_failed.emit(message)
        self.run_pending()

    @beartype
    def run_pending(self) -> None:
        if self.pending:
            self.pending = False
            self.begin_export()

    @pyqtSlot()
    @beartype
    def stop(self) -> None:
        self.thread.quit()
        self.thread.wait()


class MainWindow(QMainWindow):

    @beartype
    def __init__(
        self,
        input_path: Path,
        manager: ReloadManager,
    ) -> None:
        super().__init__()
        self.input_path = input_path.resolve()
        self.manager = manager
        self.expanded_identities: set[str] = set()

        root = TreeNode(
            kind=NodeKind.ROOT,
            name=self.input_path.name,
            identity=f"root:{self.input_path}",
            source_path=self.input_path,
        )
        self.model = OrgTreeModel(root, self)
        self.view = OrgTreeView(self)
        self.view.setModel(self.model)
        self.view.setAlternatingRowColors(True)
        self.view.setUniformRowHeights(True)
        self.view.setSortingEnabled(False)
        self.view.expanded.connect(self.on_expanded)
        self.view.collapsed.connect(self.on_collapsed)
        self.view.subtree_activated.connect(self.open_subtree)

        header = self.view.header()
        header.setSectionResizeMode(QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(
            1,
            QHeaderView.ResizeMode.Stretch,
        )

        self.setCentralWidget(self.view)
        self.setWindowTitle(f"Org tree: {self.input_path}")
        self.resize(1600, 900)

        self.manager.reload_started.connect(self.on_reload_started)
        self.manager.document_ready.connect(self.on_document_ready)
        self.manager.reload_failed.connect(self.on_reload_failed)

    @pyqtSlot()
    @beartype
    def on_reload_started(self) -> None:
        self.statusBar().showMessage(f"Exporting {self.input_path}...")

    @pyqtSlot(bytes)
    @beartype
    def on_document_ready(self, data: bytes) -> None:
        document = proto.AnyNode().parse(data)
        root = build_tree(document, self.input_path)
        self.model.replace_root(root)
        self.restore_expansion(QModelIndex())
        self.statusBar().showMessage(
            f"Loaded {len(data)} protobuf bytes",
            3000,
        )

    @pyqtSlot(str)
    @beartype
    def on_reload_failed(self, message: str) -> None:
        logger.error(f"Org protobuf export failed:\n{message}")
        QMessageBox.critical(
            self,
            "Org export failed",
            message,
        )
        self.statusBar().showMessage("Export failed")

    @pyqtSlot(QModelIndex)
    @beartype
    def on_expanded(self, index: QModelIndex) -> None:
        self.expanded_identities.add(self.model.node_at(index).identity)

    @pyqtSlot(QModelIndex)
    @beartype
    def on_collapsed(self, index: QModelIndex) -> None:
        self.expanded_identities.discard(self.model.node_at(index).identity)

    @beartype
    def restore_expansion(self, parent: QModelIndex) -> None:
        for row in range(self.model.rowCount(parent)):
            index = self.model.index(row, 0, parent)
            node = self.model.node_at(index)

            if node.identity in self.expanded_identities:
                self.view.setExpanded(index, True)

            self.restore_expansion(index)

    @pyqtSlot(object)
    @beartype
    def open_subtree(self, node: TreeNode) -> None:
        if node.source_path is None:
            raise ValueError(
                f"Subtree {node.name!r} does not have a source file path")

        if node.line is None:
            raise ValueError(f"Subtree {node.name!r} in {node.source_path} "
                             "does not have a source line")

        file_expression = json.dumps(str(node.source_path))
        expression = (f"(progn (find-file {file_expression}) "
                      f"(goto-char (point-min)) "
                      f"(forward-line {node.line}) "
                      f"(recenter))")
        local["emacsclient"]["-n", "-e", expression]()


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
    output_path = Path("/tmp/export-res.pb")

    application = QApplication(sys.argv)
    manager = ReloadManager(
        args.haxorg_cpp_org_cli,
        args.input,
        output_path,
    )
    window = MainWindow(args.input, manager)
    application.aboutToQuit.connect(manager.stop)
    window.show()
    manager.start()
    sys.exit(application.exec())


if __name__ == "__main__":
    main()
