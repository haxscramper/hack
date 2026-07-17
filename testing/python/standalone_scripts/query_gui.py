#!/usr/bin/env python

# /// script
# dependencies = ["PyQt6"]
# ///

import sys

from PyQt6.QtCore import QPoint, Qt, pyqtSignal
from PyQt6.QtGui import QAction
from PyQt6.QtWidgets import (
    QApplication,
    QDialog,
    QFrame,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QMainWindow,
    QPushButton,
    QPlainTextEdit,
    QToolButton,
    QVBoxLayout,
    QWidget,
)


class QueryPickerPopup(QDialog):
    querySelected = pyqtSignal(str, str)

    def __init__(self,
                 queries: dict[str, str],
                 parent: QWidget | None = None) -> None:
        super().__init__(parent, Qt.WindowType.Popup)
        self.setWindowTitle("Select query")
        self.queries = queries

        self.search = QLineEdit()
        self.search.setPlaceholderText("Search queries...")

        self.list_widget = QListWidget()
        self.list_widget.itemClicked.connect(self._on_item_clicked)
        self.list_widget.itemActivated.connect(self._on_item_clicked)

        layout = QVBoxLayout()
        layout.setContentsMargins(8, 8, 8, 8)
        layout.setSpacing(6)
        layout.addWidget(self.search)
        layout.addWidget(self.list_widget)
        self.setLayout(layout)

        self.search.textChanged.connect(self._rebuild_list)
        self.search.returnPressed.connect(self._activate_current)

        self._rebuild_list()
        self.resize(320, 360)
        self.search.setFocus()

    def _rebuild_list(self) -> None:
        filter_text = self.search.text().strip().lower()
        self.list_widget.clear()

        for name in sorted(self.queries):
            if filter_text and filter_text not in name.lower():
                continue
            item = QListWidgetItem(name)
            self.list_widget.addItem(item)

        if self.list_widget.count() > 0:
            self.list_widget.setCurrentRow(0)

    def _activate_current(self) -> None:
        item = self.list_widget.currentItem()
        if item is not None:
            self._on_item_clicked(item)

    def _on_item_clicked(self, item: QListWidgetItem) -> None:
        name = item.text()
        self.querySelected.emit(name, self.queries[name])
        self.close()


class MainWindow(QMainWindow):

    def __init__(self) -> None:
        super().__init__()
        self.setWindowTitle("Query Editor POC")
        self.resize(900, 600)

        self.saved_queries: dict[str, str] = {
            "Recent errors":
            "select *\nfrom logs\nwhere level = 'error'\norder by ts desc;",
            "Active users":
            "select id, name\nfrom users\nwhere active = true;",
            "Slow requests":
            "select path, duration_ms\nfrom requests\nwhere duration_ms > 1000;",
            "Revenue by day":
            "select day, sum(total)\nfrom orders\ngroup by day\norder by day desc;",
        }

        self.current_name = QLineEdit()
        self.current_name.setPlaceholderText("Query name")

        self.select_button = QToolButton()
        self.select_button.setText("☰")
        self.select_button.setToolTip("Select saved query")
        self.select_button.clicked.connect(self.show_query_picker)

        self.new_button = QPushButton("New")
        self.new_button.clicked.connect(self.new_query)

        self.save_button = QPushButton("Save")
        self.save_button.clicked.connect(self.save_query)

        top_row = QHBoxLayout()
        top_row.setContentsMargins(0, 0, 0, 0)
        top_row.setSpacing(8)
        top_row.addWidget(self.select_button)
        top_row.addWidget(self.current_name, 1)
        top_row.addWidget(self.new_button)
        top_row.addWidget(self.save_button)

        self.editor = QPlainTextEdit()
        self.editor.setPlaceholderText("Enter multiline query here...")

        root = QVBoxLayout()
        root.setContentsMargins(12, 12, 12, 12)
        root.setSpacing(10)
        root.addLayout(top_row)
        root.addWidget(self.editor, 1)

        container = QWidget()
        container.setLayout(root)
        self.setCentralWidget(container)

        self.statusBar().showMessage("POC UI only")

        self.picker: QueryPickerPopup | None = None

    def new_query(self) -> None:
        self.current_name.clear()
        self.editor.clear()

    def save_query(self) -> None:
        name = self.current_name.text().strip()
        if not name:
            self.statusBar().showMessage("Enter a query name before saving",
                                         2000)
            return

        self.saved_queries[name] = self.editor.toPlainText()
        self.statusBar().showMessage(f'Saved "{name}"', 2000)

    def show_query_picker(self) -> None:
        self.picker = QueryPickerPopup(self.saved_queries, self)
        self.picker.querySelected.connect(self.load_query)

        button_pos = self.select_button.mapToGlobal(
            QPoint(0, self.select_button.height()))
        self.picker.move(button_pos)
        self.picker.show()

    def load_query(self, name: str, text: str) -> None:
        self.current_name.setText(name)
        self.editor.setPlainText(text)
        self.statusBar().showMessage(f'Loaded "{name}"', 2000)


def main() -> int:
    app = QApplication(sys.argv)
    window = MainWindow()
    window.show()
    return app.exec()


if __name__ == "__main__":
    raise SystemExit(main())
