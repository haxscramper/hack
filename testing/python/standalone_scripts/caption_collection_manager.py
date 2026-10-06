#!/usr/bin/env python
# /// script
# requires-python = ">=3.12"
# dependencies = ["PyQt6>=6.8", "beartype>=0.19"]
# ///

import argparse
import sys
from pathlib import Path

from beartype import beartype
from beartype.typing import Any
from PyQt6.QtCore import (
    QAbstractTableModel,
    QEvent,
    QModelIndex,
    QObject,
    QRegularExpression,
    QSize,
    Qt,
    pyqtSignal,
)
from PyQt6.QtGui import QCloseEvent, QColor, QFocusEvent, QFont, QImageReader, QPixmap, QSyntaxHighlighter, QTextCharFormat, QTextDocument, QWheelEvent
from PyQt6.QtWidgets import (
    QAbstractItemView,
    QApplication,
    QHeaderView,
    QMainWindow,
    QPlainTextEdit,
    QStyledItemDelegate,
    QStyleOptionViewItem,
    QTableView,
    QWidget,
)

SCALE = 2.0


@beartype
class DatasetModel(QAbstractTableModel):

    def __init__(self, directory: Path, parent: QObject | None = None) -> None:
        super().__init__(parent)

        extensions = {
            f".{bytes(image_format).decode('ascii').lower()}"
            for image_format in QImageReader.supportedImageFormats()
        }
        self.images = sorted(
            (path for path in directory.iterdir()
             if path.is_file() and path.suffix.lower() in extensions),
            key=lambda path: path.name.casefold(),
        )
        self.captions: dict[Path, str] = {}
        self.previews: dict[Path, QPixmap] = {}
        self.preview_size = QSize(int(240 * SCALE), int(180 * SCALE))

        for image in self.images:
            caption = image.with_suffix(".txt")
            self.captions[image] = (caption.read_text(
                encoding="utf-8") if caption.exists() else "")

    def rowCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return 0 if parent.isValid() else len(self.images)

    def columnCount(self, parent: QModelIndex = QModelIndex()) -> int:
        return 0 if parent.isValid() else 2

    def headerData(
        self,
        section: int,
        orientation: Qt.Orientation,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        if role != Qt.ItemDataRole.DisplayRole:
            return None

        if orientation == Qt.Orientation.Horizontal:
            return ("Image", "Caption")[section]

        return section + 1

    def data(
        self,
        index: QModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        if not index.isValid():
            return None

        image = self.images[index.row()]

        match role:
            case Qt.ItemDataRole.ToolTipRole:
                return str(image if index.column() ==
                           0 else image.with_suffix(".txt"))

            case Qt.ItemDataRole.DecorationRole:
                if index.column() == 0:
                    return self.preview(image)

            case Qt.ItemDataRole.DisplayRole | Qt.ItemDataRole.EditRole:
                if index.column() == 1:
                    return self.captions[image]

            case Qt.ItemDataRole.TextAlignmentRole:
                return Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop

        return None

    def preview(self, image: Path) -> QPixmap:
        if image not in self.previews:
            reader = QImageReader(str(image))
            reader.setAutoTransform(True)
            size = reader.size()

            if size.isValid():
                reader.setScaledSize(
                    size.scaled(
                        self.preview_size,
                        Qt.AspectRatioMode.KeepAspectRatio,
                    ))

            decoded = reader.read()
            if decoded.isNull():
                raise ValueError(
                    f"Cannot decode image {image}: {reader.errorString()}")

            self.previews[image] = QPixmap.fromImage(decoded).scaled(
                self.preview_size,
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation,
            )

        return self.previews[image]

    def flags(self, index: QModelIndex) -> Qt.ItemFlag:
        if not index.isValid():
            return Qt.ItemFlag.NoItemFlags

        flags = Qt.ItemFlag.ItemIsEnabled | Qt.ItemFlag.ItemIsSelectable
        if index.column() == 1:
            flags |= Qt.ItemFlag.ItemIsEditable

        return flags

    def setData(
        self,
        index: QModelIndex,
        value: Any,
        role: int = Qt.ItemDataRole.EditRole,
    ) -> bool:
        if (not index.isValid() or index.column() != 1
                or role != Qt.ItemDataRole.EditRole):
            return False

        image = self.images[index.row()]
        caption = image.with_suffix(".txt")

        if not isinstance(value, str):
            raise TypeError(f"Caption for {caption} must be a string, "
                            f"received {type(value).__name__}")

        if value == self.captions[image]:
            return True

        caption.write_text(value, encoding="utf-8")
        self.captions[image] = value
        self.dataChanged.emit(
            index,
            index,
            [Qt.ItemDataRole.DisplayRole, Qt.ItemDataRole.EditRole],
        )
        return True


@beartype
class CaptionEditor(QPlainTextEdit):
    editingFinished = pyqtSignal()

    def focusOutEvent(self, event: QFocusEvent) -> None:
        super().focusOutEvent(event)
        self.editingFinished.emit()


@beartype
class CaptionHighlighter(QSyntaxHighlighter):

    def __init__(
        self,
        document: QTextDocument,
        patterns: list[tuple[QRegularExpression, QTextCharFormat]],
    ) -> None:
        super().__init__(document)
        self.patterns = patterns

    def highlightBlock(self, text: str) -> None:
        for expression, text_format in self.patterns:
            matches = expression.globalMatch(text)
            while matches.hasNext():
                match = matches.next()
                self.setFormat(
                    match.capturedStart(),
                    match.capturedLength(),
                    text_format,
                )


@beartype
class CaptionDelegate(QStyledItemDelegate):

    def __init__(
        self,
        patterns: list[tuple[QRegularExpression, QTextCharFormat]],
        parent: QObject | None = None,
    ) -> None:
        super().__init__(parent)
        self.patterns = patterns

    def createEditor(
        self,
        parent: QWidget,
        option: QStyleOptionViewItem,
        index: QModelIndex,
    ) -> CaptionEditor:
        editor = CaptionEditor(parent)
        font = QFont()
        font.setPointSize(18)
        editor.setFont(font)
        editor.highlighter = CaptionHighlighter(
            editor.document(),
            self.patterns,
        )

        editor.setLineWrapMode(QPlainTextEdit.LineWrapMode.WidgetWidth)
        editor.editingFinished.connect(lambda: self.commitData.emit(editor))
        return editor

    def setEditorData(self, editor: QWidget, index: QModelIndex) -> None:
        if isinstance(editor, CaptionEditor):
            text = index.data(Qt.ItemDataRole.EditRole)
            if editor.toPlainText() != text:
                editor.setPlainText(text)

    def setModelData(
        self,
        editor: QWidget,
        model: QAbstractTableModel,
        index: QModelIndex,
    ) -> None:
        if isinstance(editor, CaptionEditor):
            model.setData(
                index,
                editor.toPlainText(),
                Qt.ItemDataRole.EditRole,
            )

    def updateEditorGeometry(
        self,
        editor: QWidget,
        option: QStyleOptionViewItem,
        index: QModelIndex,
    ) -> None:
        editor.setGeometry(option.rect.adjusted(4, 4, -4, -4))

    def eventFilter(self, watched: QObject, event: QEvent) -> bool:
        if isinstance(watched, CaptionEditor):
            return False
        return super().eventFilter(watched, event)


@beartype
class DatasetTable(QTableView):

    def wheelEvent(self, event: QWheelEvent) -> None:
        delta = event.angleDelta().y() or event.pixelDelta().y()
        scrollbar = self.verticalScrollBar()

        if 0 < delta:
            scrollbar.setValue(scrollbar.value() - 1)
        elif delta < 0:
            scrollbar.setValue(scrollbar.value() + 1)

        event.accept()


@beartype
class DatasetWindow(QMainWindow):

    def __init__(
        self,
        directory: Path,
        patterns: list[tuple[QRegularExpression, QTextCharFormat]],
    ) -> None:

        super().__init__()
        self.setWindowTitle(f"Tag dataset — {directory}")
        self.resize(1100, 800)

        self.model = DatasetModel(directory, self)
        self.table = DatasetTable(self)
        self.table.setVerticalScrollMode(
            QAbstractItemView.ScrollMode.ScrollPerItem)
        self.delegate = CaptionDelegate(patterns, self.table)

        self.table.setModel(self.model)
        self.table.setItemDelegateForColumn(1, self.delegate)
        self.table.setIconSize(self.model.preview_size)
        self.table.setWordWrap(False)

        header = self.table.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.Fixed)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
        self.table.setColumnWidth(0, int(260 * SCALE))

        self.table.verticalHeader().setDefaultSectionSize(int(200 * SCALE))
        self.setCentralWidget(self.table)

        for row in range(self.model.rowCount()):
            self.table.openPersistentEditor(self.model.index(row, 1))

    def closeEvent(self, event: QCloseEvent) -> None:
        for row in range(self.model.rowCount()):
            index = self.model.index(row, 1)
            editor = self.table.indexWidget(index)
            if isinstance(editor, CaptionEditor):
                self.delegate.setModelData(editor, self.model, index)

        super().closeEvent(event)


@beartype
def main() -> int:
    parser = argparse.ArgumentParser(
        description="Edit image captions with automatic saving on focus loss.")
    parser.add_argument("directory", type=Path)
    parser.add_argument(
        "--pattern",
        action="append",
        default=[],
        metavar="COLOR=REGEXP",
        help="Highlight regex matches; repeat for multiple patterns.",
    )

    arguments = parser.parse_args()
    patterns: list[tuple[QRegularExpression, QTextCharFormat]] = []

    for specification in arguments.pattern:
        color_name, separator, expression_text = specification.partition("=")
        if not separator:
            parser.error(
                f"Invalid pattern {specification!r}: expected COLOR=REGEXP")

        color = QColor(color_name)
        if not color.isValid():
            parser.error(
                f"Invalid color {color_name!r} in pattern {specification!r}")

        expression = QRegularExpression(expression_text)
        if not expression.isValid():
            parser.error(
                f"Invalid regex in pattern {specification!r} at offset "
                f"{expression.patternErrorOffset()}: {expression.errorString()}"
            )

        text_format = QTextCharFormat()
        text_format.setForeground(color)
        patterns.append((expression, text_format))

    directory = arguments.directory.expanduser().resolve()

    if not directory.is_dir():
        raise NotADirectoryError(
            f"Dataset directory does not exist: {directory}")

    application = QApplication([sys.argv[0]])
    window = DatasetWindow(directory, patterns)

    window.show()
    return application.exec()


if __name__ == "__main__":
    sys.exit(main())
