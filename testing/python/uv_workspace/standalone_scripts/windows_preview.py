#!/usr/bin/env python
import subprocess
import json
import sys
from dataclasses import dataclass
from PyQt6.QtWidgets import (
    QApplication,
    QWidget,
    QVBoxLayout,
    QHBoxLayout,
    QLabel,
    QFrame,
    QScrollArea,
)
from PyQt6.QtCore import Qt, pyqtSignal
from PyQt6.QtGui import QFont, QKeyEvent
from beartype import beartype
from beartype.typing import Optional
import logging


@dataclass
class WindowInfo:
    id: int
    name: str
    class_name: str


@dataclass
class TagInfo:
    name: str
    index: int
    selected: bool
    windows: list[WindowInfo]


@dataclass
class ScreenInfo:
    index: int
    focused: bool
    tags: list[TagInfo]


@beartype
def get_awesome_state() -> list[ScreenInfo]:
    lua_code = """
    local json = require("cjson")
    local result = {}
    for s in screen do
        local screen_data = {
            index = s.index,
            focused = s == awful.screen.focused(),
            tags = {}
        }
        for _, t in ipairs(s.tags) do
            local tag_data = {
                name = t.name,
                index = t.index,
                selected = t.selected,
                windows = {}
            }
            for _, c in ipairs(t:clients()) do
                table.insert(tag_data.windows, {
                    id = c.window,
                    name = c.name or "",
                    class_name = c.class or ""
                })
            end
            table.insert(screen_data.tags, tag_data)
        end
        table.insert(result, screen_data)
    end
    return json.encode(result)
    """
    result = subprocess.run(
        ["awesome-client", lua_code],
        capture_output=True,
        text=True,
    )
    output = result.stdout.strip()
    if output.startswith("string "):
        output = output[7:]
    output = output.strip('"')
    output = output.replace('\\"', '"')
    data = json.loads(output)
    screens = []
    for screen_data in data:
        tags = []
        for tag_data in screen_data["tags"]:
            windows = [
                WindowInfo(
                    id=w["id"],
                    name=w["name"],
                    class_name=w["class_name"],
                ) for w in tag_data["windows"]
            ]
            tags.append(
                TagInfo(
                    name=tag_data["name"],
                    index=tag_data["index"],
                    selected=tag_data["selected"],
                    windows=windows,
                ))
        screens.append(
            ScreenInfo(
                index=screen_data["index"],
                focused=screen_data["focused"],
                tags=tags,
            ))
    return screens


@beartype
def focus_window(window_id: int) -> None:
    lua_code = f"""
    for _, c in ipairs(client.get()) do
        if c.window == {window_id} then
            c:jump_to()
            break
        end
    end
    """
    subprocess.run(["awesome-client", lua_code])


class WindowCell(QFrame):
    clicked = pyqtSignal(int)

    @beartype
    def __init__(self,
                 window: WindowInfo,
                 parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.window_id = window.id
        self.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Raised)
        self.setStyleSheet(
            "WindowCell { background-color: #3c3c3c; border: 1px solid #555; border-radius: 3px; }"
            "WindowCell:hover { background-color: #4c4c4c; }")
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)
        layout.setSpacing(2)
        class_label = QLabel(window.class_name or "Unknown")
        class_label.setFont(QFont("monospace", 9, QFont.Weight.Bold))
        class_label.setStyleSheet("color: #aaffaa;")
        layout.addWidget(class_label)
        name_label = QLabel(window.name[:30] +
                            "..." if 30 < len(window.name) else window.name)
        name_label.setFont(QFont("monospace", 8))
        name_label.setStyleSheet("color: #cccccc;")
        name_label.setWordWrap(True)
        layout.addWidget(name_label)

    @beartype
    def mousePressEvent(self, event) -> None:
        self.clicked.emit(self.window_id)


class TagCell(QFrame):
    window_selected = pyqtSignal(int)

    @beartype
    def __init__(self, tag: TagInfo, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.setMinimumWidth(150)
        self.setMinimumHeight(100)
        bg_color = "#2a4a2a" if tag.selected else "#2a2a2a"
        border_color = "#66ff66" if tag.selected else "#444"
        self.setStyleSheet(
            f"TagCell {{ background-color: {bg_color}; border: 2px solid {border_color}; border-radius: 5px; }}"
        )
        layout = QVBoxLayout(self)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(3)
        header = QLabel(f"Tag {tag.name}")
        header.setFont(QFont("monospace", 10, QFont.Weight.Bold))
        header.setStyleSheet("color: #ffffff;")
        header.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(header)
        if tag.windows:
            for window in tag.windows:
                window_cell = WindowCell(window)
                window_cell.clicked.connect(self.window_selected.emit)
                layout.addWidget(window_cell)
        else:
            empty_label = QLabel("(empty)")
            empty_label.setStyleSheet("color: #666;")
            empty_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
            layout.addWidget(empty_label)
        layout.addStretch()


class ScreenRow(QFrame):
    window_selected = pyqtSignal(int)

    @beartype
    def __init__(self,
                 screen: ScreenInfo,
                 parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        bg_color = "#1a3a1a" if screen.focused else "#1a1a1a"
        border_color = "#00ff00" if screen.focused else "#333"
        self.setStyleSheet(
            f"ScreenRow {{ background-color: {bg_color}; border: 3px solid {border_color}; border-radius: 8px; margin: 5px; }}"
        )
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(10, 10, 10, 10)
        header_text = f"Screen {screen.index}"
        if screen.focused:
            header_text += " (focused)"
        header = QLabel(header_text)
        header.setFont(QFont("monospace", 12, QFont.Weight.Bold))
        header.setStyleSheet(
            "color: #00ff00;" if screen.focused else "color: #888;")
        main_layout.addWidget(header)
        tags_layout = QHBoxLayout()
        tags_layout.setSpacing(10)
        for tag in screen.tags:
            tag_cell = TagCell(tag)
            tag_cell.window_selected.connect(self.window_selected.emit)
            tags_layout.addWidget(tag_cell)
        tags_layout.addStretch()
        main_layout.addLayout(tags_layout)


class WorkspacePopup(QWidget):

    @beartype
    def __init__(self,
                 screens: list[ScreenInfo],
                 parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.setWindowTitle("Workspace Navigator")
        self.setWindowFlags(Qt.WindowType.WindowStaysOnTopHint
                            | Qt.WindowType.FramelessWindowHint
                            | Qt.WindowType.Popup)
        self.setStyleSheet("background-color: #0a0a0a;")
        sorted_screens = sorted(screens,
                                key=lambda s: (not s.focused, s.index))
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(10, 10, 10, 10)
        title = QLabel("Workspace Navigator - Press Escape to close")
        title.setFont(QFont("monospace", 14, QFont.Weight.Bold))
        title.setStyleSheet("color: #00ff00; padding: 10px;")
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        main_layout.addWidget(title)
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setStyleSheet("QScrollArea { border: none; }")
        content = QWidget()
        content_layout = QVBoxLayout(content)
        content_layout.setSpacing(10)
        for screen in sorted_screens:
            row = ScreenRow(screen)
            row.window_selected.connect(self.on_window_selected)
            content_layout.addWidget(row)
        content_layout.addStretch()
        scroll.setWidget(content)
        main_layout.addWidget(scroll)
        self.adjustSize()
        screen_geometry = QApplication.primaryScreen().availableGeometry()
        max_width = int(screen_geometry.width() * 0.9)
        max_height = int(screen_geometry.height() * 0.8)
        self.setMaximumSize(max_width, max_height)
        self.resize(min(self.sizeHint().width(), max_width),
                    min(self.sizeHint().height(), max_height))
        self.move(
            screen_geometry.center().x() - self.width() // 2,
            screen_geometry.center().y() - self.height() // 2,
        )

    @beartype
    def on_window_selected(self, window_id: int) -> None:
        logging.info(f"Selected window: {window_id}")
        self.close()
        focus_window(window_id)

    @beartype
    def keyPressEvent(self, event: QKeyEvent) -> None:
        if event.key() == Qt.Key.Key_Escape:
            self.close()
        else:
            super().keyPressEvent(event)


@beartype
def main() -> None:
    logging.basicConfig(level=logging.INFO)
    app = QApplication(sys.argv)
    screens = get_awesome_state()
    popup = WorkspacePopup(screens)
    popup.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()
