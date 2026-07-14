from PyQt6.QtCore import QAbstractItemModel, QModelIndex, QSortFilterProxyModel, Qt, QRect, QPoint
from PyQt6.QtGui import QColor, QPalette, QPen
from PyQt6.QtWidgets import (
    QApplication,
    QMainWindow,
)

from pathlib import Path

import pytest
from pytestqt.qtbot import QtBot
from PyQt6.QtCore import QModelIndex, Qt
from PyQt6.QtTest import QSignalSpy
from index_service.gui.widgets.directory_view import BaseDirDelegate, MixedTreeTileView, TileDelegate, attr_label


class SynNode:
    __slots__ = ("name", "is_dir", "props", "children", "parent")

    def __init__(self, name, is_dir, props=None):
        self.name = name
        self.is_dir = is_dir
        self.props = props or ["", "", ""]
        self.children: list[SynNode] = []
        self.parent: SynNode | None = None

    def add(self, child: "SynNode") -> "SynNode":
        child.parent = self
        self.children.append(child)
        return child


class SynModel(QAbstractItemModel):
    HEADERS = ["Name", "Alpha", "Beta", "Gamma"]

    def __init__(self, root: SynNode):
        super().__init__()
        self._root = root

    def index(self, row, column, parent=QModelIndex()):
        if not self.hasIndex(row, column, parent):
            return QModelIndex()
        pnode = parent.internalPointer() if parent.isValid() else self._root
        if row < len(pnode.children):
            return self.createIndex(row, column, pnode.children[row])
        return QModelIndex()

    def parent(self, index):
        if not index.isValid():
            return QModelIndex()
        node = index.internalPointer()
        p = node.parent
        if p is None or p is self._root:
            return QModelIndex()
        gp = p.parent
        return self.createIndex(gp.children.index(p), 0, p)

    def rowCount(self, parent=QModelIndex()):
        if parent.isValid() and parent.column() > 0:
            return 0
        node = parent.internalPointer() if parent.isValid() else self._root
        return len(node.children)

    def columnCount(self, parent=QModelIndex()):
        return 4

    def hasChildren(self, parent=QModelIndex()):
        node = parent.internalPointer() if parent.isValid() else self._root
        return len(node.children) > 0

    def data(self, index, role=Qt.ItemDataRole.DisplayRole):
        if not index.isValid() or role != Qt.ItemDataRole.DisplayRole:
            return None
        node = index.internalPointer()
        if index.column() == 0:
            return node.name
        return node.props[index.column() - 1]

    def headerData(self, section, orientation, role=Qt.ItemDataRole.DisplayRole):
        if (orientation == Qt.Orientation.Horizontal and
                role == Qt.ItemDataRole.DisplayRole):
            return self.HEADERS[section]
        return None

    def set_root(self, root: SynNode) -> None:
        self.beginResetModel()
        self._root = root
        self.endResetModel()


def wrap_proxy(
    model: QAbstractItemModel,
    view: MixedTreeTileView,
) -> QSortFilterProxyModel:
    proxy = QSortFilterProxyModel(view)
    model.setParent(proxy)

    proxy.setSourceModel(model)
    proxy.setSortRole(Qt.ItemDataRole.DisplayRole)
    proxy.setDynamicSortFilter(True)
    proxy.sort(0, Qt.SortOrder.AscendingOrder)
    return proxy


class SynDirDelegate(BaseDirDelegate):
    pass


class SynFileDelegate(TileDelegate):

    def paint(self, painter, primary, area, attributes, state):
        pal = QApplication.palette()
        rect = QRect(QPoint(0, 0), area)
        painter.save()

        if state.selected:
            border = pal.color(QPalette.ColorRole.Highlight)
            bg = pal.color(QPalette.ColorRole.Highlight).lighter(165)
        else:
            border = pal.color(QPalette.ColorRole.Mid)
            bg = pal.color(QPalette.ColorRole.AlternateBase)
        painter.setPen(QPen(border, 2 if state.current else 1))
        painter.setBrush(bg)
        painter.drawRoundedRect(rect.adjusted(1, 1, -1, -1), 6, 6)

        name = str(primary.data(Qt.ItemDataRole.DisplayRole) or "")
        margin = 6
        side = area.height() - 2 * margin
        preview = QRect(margin, margin, side, side)
        h = (hash(name) % 360)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QColor.fromHsv(h, 160, 220))
        painter.drawEllipse(preview.adjusted(6, 6, -6, -6))

        text_x = preview.right() + 8
        painter.setPen(pal.color(QPalette.ColorRole.Text))
        painter.drawText(QRect(text_x, 6,
                               area.width() - text_x - 6, 18),
                         Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter, name)

        y = 26
        for a in attributes:
            painter.drawText(QRect(text_x, y,
                                   area.width() - text_x - 6, 16),
                             Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter,
                             attr_label(a))
            y += 16
        painter.restore()

    def getItemBoundingBoxes(self, primary, area, attributes):
        margin = 6
        side = area.height() - 2 * margin
        preview = QRect(margin, margin, side, side)
        text_x = preview.right() + 8
        boxes = [QRect(text_x, 6, area.width() - text_x - 6, 18)]
        y = 26
        for _ in attributes:
            boxes.append(QRect(text_x, y, area.width() - text_x - 6, 16))
            y += 16
        return boxes


class MainWindow(QMainWindow):

    def __init__(self, root: SynNode, parent=None):
        super().__init__(parent)
        self.setWindowTitle("MixedTreeTileView demo")

        self.directory_view = MixedTreeTileView(self)
        self.synthetic_model = SynModel(root)
        self.proxy_model = wrap_proxy(
            self.synthetic_model,
            self.directory_view,
        )

        self.directory_view.set_delegates(
            SynDirDelegate(),
            SynFileDelegate(),
        )
        self.directory_view.setModel(self.proxy_model)
        self.directory_view.setRootIndex(QModelIndex())

        self.setCentralWidget(self.directory_view)

    def set_synthetic_tree(self, root: SynNode) -> None:
        self.synthetic_model.set_root(root)
        self.directory_view.setRootIndex(QModelIndex())


@pytest.fixture
def synthetic_directory() -> SynNode:
    root = SynNode("Fixture root", True)

    album = root.add(SynNode("Album", True, ["directory", "fixture", "root"]))
    album.add(SynNode("first.png", False, ["100", "image", "png"]))
    album.add(SynNode("second.txt", False, ["200", "text", "txt"]))

    archive = root.add(SynNode("Archive", True, ["directory", "fixture", "empty"]))
    archive.add(SynNode("record.dat", False, ["300", "binary", "dat"]))

    return root


@pytest.fixture
def gui_app_instance(
    synthetic_directory: SynNode,
    qtbot: QtBot,
) -> MainWindow:
    window = MainWindow(synthetic_directory)
    window.resize(900, 600)

    qtbot.addWidget(window)
    window.show()
    qtbot.waitExposed(window)

    yield window

    window.close()


def find_index(
        window: MainWindow,
        name: str,
        parent: QModelIndex = QModelIndex(),
) -> QModelIndex:
    model = window.proxy_model

    for row in range(model.rowCount(parent)):
        index = model.index(row, 0, parent)

        if index.data(Qt.ItemDataRole.DisplayRole) == name:
            return index

        child = find_index(window, name, index)
        if child.isValid():
            return child

    return QModelIndex()


def click_index(
    window: MainWindow,
    qtbot: QtBot,
    index: QModelIndex,
) -> None:
    view = window.directory_view
    rect = view.visualRect(index)

    assert not rect.isNull()
    qtbot.mouseClick(
        view.viewport(),
        Qt.MouseButton.LeftButton,
        pos=rect.center(),
    )


def test_root_directories_are_visible(gui_app_instance: MainWindow,) -> None:
    window = gui_app_instance
    view = window.directory_view

    album = find_index(window, "Album")
    archive = find_index(window, "Archive")
    first_file = find_index(window, "first.png")

    assert album.isValid()
    assert archive.isValid()
    assert first_file.isValid()
    assert not view.visualRect(album).isNull()
    assert not view.visualRect(archive).isNull()
    assert view.visualRect(first_file).isNull()


def test_directory_emits_expanded_and_collapsed(
    gui_app_instance: MainWindow,
    qtbot: QtBot,
) -> None:
    window = gui_app_instance
    view = window.directory_view
    album = find_index(window, "Album")
    first_file = find_index(window, "first.png")

    expanded_spy = QSignalSpy(view.expanded)
    collapsed_spy = QSignalSpy(view.collapsed)

    click_index(window, qtbot, album)

    assert len(expanded_spy) == 1
    assert not view.visualRect(first_file).isNull()

    click_index(window, qtbot, album)

    assert len(collapsed_spy) == 1
    assert view.visualRect(first_file).isNull()


def test_file_click_updates_current_index_and_selection(
    gui_app_instance: MainWindow,
    qtbot: QtBot,
) -> None:
    window = gui_app_instance
    view = window.directory_view
    album = find_index(window, "Album")
    first_file = find_index(window, "first.png")

    click_index(window, qtbot, album)

    current_spy = QSignalSpy(view.selectionModel().currentChanged)
    selection_spy = QSignalSpy(view.selectionModel().selectionChanged)

    click_index(window, qtbot, first_file)

    assert len(current_spy) == 1
    assert len(selection_spy) == 1
    assert view.currentIndex() == first_file
    assert view.selectionModel().isSelected(first_file)


def test_setting_tree_resets_model_and_view(gui_app_instance: MainWindow,) -> None:
    window = gui_app_instance
    reset_spy = QSignalSpy(window.synthetic_model.modelReset)

    replacement = SynNode("Replacement root", True)
    replacement.add(SynNode("Replacement directory", True, ["new", "tree", "value"]))

    window.set_synthetic_tree(replacement)

    replacement_index = find_index(window, "Replacement directory")
    old_index = find_index(window, "Album")

    assert len(reset_spy) == 1
    assert replacement_index.isValid()
    assert not window.directory_view.visualRect(replacement_index).isNull()
    assert not old_index.isValid()


def test_model_reset_clears_expansion_state(
    gui_app_instance: MainWindow,
    qtbot: QtBot,
) -> None:
    window = gui_app_instance
    view = window.directory_view
    album = find_index(window, "Album")

    click_index(window, qtbot, album)
    assert view._is_expanded(album)

    replacement = SynNode("Replacement root", True)
    directory = replacement.add(SynNode("New directory", True))
    directory.add(SynNode("hidden.txt", False))

    window.set_synthetic_tree(replacement)

    new_directory = find_index(window, "New directory")
    hidden_file = find_index(window, "hidden.txt")

    assert not view._is_expanded(new_directory)
    assert view.visualRect(hidden_file).isNull()
