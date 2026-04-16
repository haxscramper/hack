#!/usr/bin/env -S uv run
# /// script
# dependencies = ["pyside6>=6.7"]
# ///

from __future__ import annotations

import math
import os
import sys
from dataclasses import dataclass, field
from pathlib import Path

from PySide6.QtCore import (
    QObject,
    QPoint,
    QRect,
    QRunnable,
    QSize,
    Qt,
    QThreadPool,
    QTimer,
    Signal,
)
from PySide6.QtGui import (
    QAction,
    QColor,
    QImage,
    QImageReader,
    QMouseEvent,
    QPainter,
    QPaintEvent,
    QPalette,
    QPen,
    QPixmap,
    QResizeEvent,
    QWheelEvent,
    QPolygon,
)
from PySide6.QtWidgets import (
    QApplication,
    QAbstractScrollArea,
    QMainWindow,
    QMessageBox,
    QMenu,
)


IMAGE_EXTENSIONS = {
    ".bmp",
    ".gif",
    ".ico",
    ".jpeg",
    ".jpg",
    ".png",
    ".tif",
    ".tiff",
    ".webp",
}


def is_image_file(path: Path) -> bool:
    return path.is_file() and path.suffix.lower() in IMAGE_EXTENSIONS


@dataclass
class DirNode:
    path: Path
    expanded: bool = True
    children_loaded: bool = False
    child_dirs: list["DirNode"] = field(default_factory=list)
    image_files: list[Path] = field(default_factory=list)

    def load(self) -> None:
        if self.children_loaded:
            return

        child_dirs: list[DirNode] = []
        image_files: list[Path] = []

        try:
            entries = sorted(
                self.path.iterdir(),
                key=lambda p: (not p.is_dir(), p.name.lower()),
            )
        except PermissionError, FileNotFoundError, OSError:
            entries = []

        for entry in entries:
            try:
                if entry.is_dir():
                    child_dirs.append(DirNode(entry, expanded=False))
                elif is_image_file(entry):
                    image_files.append(entry)
            except PermissionError, OSError:
                pass

        self.child_dirs = child_dirs
        self.image_files = image_files
        self.children_loaded = True


@dataclass
class HeaderHit:
    node: DirNode
    rect: QRect
    toggle_rect: QRect
    depth: int


@dataclass
class TileHit:
    file_path: Path
    rect: QRect
    depth: int


class ThumbSignals(QObject):
    loaded = Signal(str, QImage)


class ThumbTask(QRunnable):
    def __init__(self, path: Path, thumb_size: int, signals: ThumbSignals):
        super().__init__()
        self.path = path
        self.thumb_size = thumb_size
        self.signals = signals

    def run(self) -> None:
        reader = QImageReader(str(self.path))
        reader.setAutoTransform(True)

        size = reader.size()
        if size.isValid():
            w = size.width()
            h = size.height()
            if w > 0 and h > 0:
                scale = min(self.thumb_size / w, self.thumb_size / h)
                target = QSize(max(1, int(w * scale)), max(1, int(h * scale)))
                reader.setScaledSize(target)

        image = reader.read()
        self.signals.loaded.emit(str(self.path), image)


class MixedTreeTileView(QAbstractScrollArea):
    imageClicked = Signal(object)
    fileSelected = Signal(object)

    HEADER_HEIGHT = 28
    INDENT = 20
    SECTION_SPACING = 8
    TILE_W = 116
    TILE_H = 116
    TILE_SPACING_X = 8
    TILE_SPACING_Y = 8
    TILE_TEXT_H = 32
    THUMB_SIZE = 72
    CONTENT_MARGIN = 8
    PREFETCH_MARGIN = 300
    TILE_MARGIN = 4

    def __init__(self, root_path: Path, parent=None):
        super().__init__(parent)
        self.zoom_factor = 1.0

        self.root_node = DirNode(root_path.resolve(), expanded=True)
        self.root_node.load()

        self.header_hits: list[HeaderHit] = []
        self.tile_hits: list[TileHit] = []
        self.selected_files: set[Path] = set()
        self.fully_annotated_files: set[Path] = set()
        self.last_clicked_file: Path | None = None
        self.hovered_file_path: Path | None = None

        self.thumb_cache: dict[Path, QPixmap] = {}
        self.loading_paths: set[Path] = set()

        self.thread_pool = QThreadPool.globalInstance()
        max_threads = max(1, min(8, (os.cpu_count() or 4)))
        self.thread_pool.setMaxThreadCount(max_threads)

        self.thumb_signals = ThumbSignals()
        self.thumb_signals.loaded.connect(self.on_thumbnail_loaded)

        self.setMouseTracking(True)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)

        pal = self.palette()
        self.bg = pal.color(QPalette.ColorRole.Base)
        self.fg = pal.color(QPalette.ColorRole.Text)
        self.alt_bg = pal.color(QPalette.ColorRole.AlternateBase)
        self.hl = pal.color(QPalette.ColorRole.Highlight)
        self.hl_text = pal.color(QPalette.ColorRole.HighlightedText)
        self.mid = pal.color(QPalette.ColorRole.Mid)
        self.placeholder_bg = pal.color(QPalette.ColorRole.Button)
        self.placeholder_fg = pal.color(QPalette.ColorRole.ButtonText)

        self._update_timer = QTimer(self)
        self._update_timer.setSingleShot(True)
        self._update_timer.timeout.connect(self.viewport().update)

        self._update_scrollbars()

    @property
    def tile_w(self) -> int:
        return self.thumb_size + self.TILE_MARGIN * 2

    @property
    def tile_h(self) -> int:
        return self.thumb_size + self.TILE_MARGIN * 2

    @property
    def thumb_size(self) -> int:
        return int(self.THUMB_SIZE * self.zoom_factor)

    def visible_width(self) -> int:
        return max(1, self.viewport().width())

    def flow_columns(self, depth: int) -> int:
        available = self.visible_width() - 2 * self.CONTENT_MARGIN - depth * self.INDENT
        available = max(available, self.tile_w)
        return max(
            1, (available + self.TILE_SPACING_X) // (self.tile_w + self.TILE_SPACING_X)
        )

    def layout_height_for_images(self, count: int, depth: int) -> int:
        if count == 0:
            return 0
        cols = self.flow_columns(depth)
        rows = math.ceil(count / cols)
        return rows * self.tile_h + max(0, rows - 1) * self.TILE_SPACING_Y

    def total_content_height(self) -> int:
        self.header_hits.clear()
        self.tile_hits.clear()
        _, y = self._build_layout(None, 0, self.CONTENT_MARGIN)
        return max(self.viewport().height(), y + self.CONTENT_MARGIN)

    def _update_scrollbars(self) -> None:
        total = self.total_content_height()
        page = max(1, self.viewport().height())
        self.verticalScrollBar().setPageStep(page)
        self.verticalScrollBar().setRange(0, max(0, total - page))
        self.viewport().update()

    def content_pos(self, viewport_pos: QPoint) -> QPoint:
        return viewport_pos + QPoint(0, self.verticalScrollBar().value())

    def visible_content_rect(self) -> QRect:
        y = self.verticalScrollBar().value()
        return QRect(
            0,
            y - self.PREFETCH_MARGIN,
            self.viewport().width(),
            self.viewport().height() + 2 * self.PREFETCH_MARGIN,
        )

    def request_thumbnail(self, path: Path) -> None:
        if path in self.thumb_cache or path in self.loading_paths:
            return
        self.loading_paths.add(path)
        self.thread_pool.start(ThumbTask(path, self.thumb_size, self.thumb_signals))

    def on_thumbnail_loaded(self, path_str: str, image: QImage) -> None:
        path = Path(path_str)
        self.loading_paths.discard(path)

        if image.isNull():
            pix = QPixmap(self.thumb_size, self.thumb_size)
            pix.fill(Qt.GlobalColor.lightGray)
        else:
            pix = QPixmap.fromImage(image)

        self.thumb_cache[path] = pix
        if not self._update_timer.isActive():
            self._update_timer.start(10)

    def _build_layout(
        self, painter: QPainter | None, depth: int, y: int, node: DirNode | None = None
    ) -> tuple[int, int]:
        if node is None:
            node = self.root_node

        vx = self.CONTENT_MARGIN + depth * self.INDENT
        vw = self.visible_width() - 2 * self.CONTENT_MARGIN - depth * self.INDENT
        vw = max(vw, 100)

        header_rect = QRect(vx, y, vw, self.HEADER_HEIGHT)
        toggle_rect = QRect(vx, y + 6, 16, 16)
        self.header_hits.append(
            HeaderHit(node=node, rect=header_rect, toggle_rect=toggle_rect, depth=depth)
        )

        if painter is not None:
            self._paint_header(painter, node, header_rect, toggle_rect, depth)

        y += self.HEADER_HEIGHT + 4

        if node.expanded:
            node.load()

            # Directories first
            for child in node.child_dirs:
                _, y = self._build_layout(painter, depth + 1, y, child)

            # Images after directories
            if node.image_files:
                cols = self.flow_columns(depth)
                for idx, file_path in enumerate(node.image_files):
                    row = idx // cols
                    col = idx % cols
                    tx = vx + col * (self.tile_w + self.TILE_SPACING_X)
                    ty = y + row * (self.tile_h + self.TILE_SPACING_Y)
                    rect = QRect(tx, ty, self.tile_w, self.tile_h)
                    self.tile_hits.append(
                        TileHit(file_path=file_path, rect=rect, depth=depth)
                    )
                    if painter is not None:
                        self._paint_tile(
                            painter,
                            file_path,
                            rect,
                            selected=(file_path in self.selected_files),
                        )

                y += self.layout_height_for_images(len(node.image_files), depth)
                y += self.SECTION_SPACING

        return depth, y

    def paintEvent(self, event: QPaintEvent) -> None:
        self.header_hits.clear()
        self.tile_hits.clear()

        painter = QPainter(self.viewport())
        painter.fillRect(self.viewport().rect(), self.bg)
        painter.translate(0, -self.verticalScrollBar().value())

        self._build_layout(painter, 0, self.CONTENT_MARGIN)

    def _paint_header(
        self,
        painter: QPainter,
        node: DirNode,
        rect: QRect,
        toggle_rect: QRect,
        depth: int,
    ) -> None:
        painter.save()
        from image_tagger.config import config

        is_excluded = str(node.path) in config.excluded_directories

        if is_excluded:
            bg = QColor(255, 200, 200)
        else:
            bg = self.alt_bg if depth % 2 == 0 else self.bg
        painter.fillRect(rect, bg)

        painter.setPen(QPen(self.mid))
        painter.drawLine(rect.bottomLeft(), rect.bottomRight())

        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(self.fg)
        cx = toggle_rect.center().x()
        cy = toggle_rect.center().y()

        if node.expanded:
            poly = QPolygon(
                [
                    QPoint(cx - 4, cy - 2),
                    QPoint(cx + 4, cy - 2),
                    QPoint(cx, cy + 4),
                ]
            )
        else:
            poly = QPolygon(
                [
                    QPoint(cx - 2, cy - 4),
                    QPoint(cx - 2, cy + 4),
                    QPoint(cx + 4, cy),
                ]
            )

        painter.drawPolygon(poly)

        painter.setPen(self.fg)
        font = painter.font()
        font.setBold(depth == 0)
        painter.setFont(font)

        text_x = toggle_rect.right() + 6
        text_rect = QRect(
            text_x, rect.y(), rect.width() - (text_x - rect.x()), rect.height()
        )
        label = node.path.name if node.path.name else str(node.path)
        count_text = ""
        if node.children_loaded:
            count_text = (
                f"  [{len(node.child_dirs)} dirs, {len(node.image_files)} images]"
            )
        painter.drawText(
            text_rect,
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            label + count_text,
        )

        painter.restore()

    def _paint_placeholder_thumb(
        self, painter: QPainter, thumb_rect: QRect, file_path: Path
    ) -> None:
        painter.save()
        painter.setPen(QPen(self.mid, 1, Qt.PenStyle.DashLine))
        painter.setBrush(self.placeholder_bg)
        painter.drawRoundedRect(thumb_rect, 4, 4)

        painter.setPen(self.placeholder_fg)
        painter.drawText(
            thumb_rect,
            Qt.AlignmentFlag.AlignCenter,
            file_path.suffix.lower().lstrip(".") or "img",
        )
        painter.restore()

    def _paint_tile(
        self, painter: QPainter, file_path: Path, rect: QRect, selected: bool
    ) -> None:
        painter.save()

        is_fully_annotated = file_path in self.fully_annotated_files

        if selected:
            border = self.hl
            bg = self.hl.lighter(165)
        else:
            border = self.mid
            if is_fully_annotated:
                bg = QColor(200, 255, 200)  # Pale green
            else:
                bg = QColor(self.alt_bg)

        painter.setPen(QPen(border, 1))
        painter.setBrush(bg)
        painter.drawRoundedRect(rect, 6, 6)

        margin = self.TILE_MARGIN
        thumb_rect = QRect(
            rect.x() + margin,
            rect.y() + margin,
            self.thumb_size,
            self.thumb_size,
        )

        visible_rect = self.visible_content_rect()
        should_load = rect.intersects(visible_rect)

        pix = self.thumb_cache.get(file_path)
        if pix is None:
            if should_load:
                self.request_thumbnail(file_path)
            self._paint_placeholder_thumb(painter, thumb_rect, file_path)
        else:
            scaled = pix.scaled(
                thumb_rect.size(),
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation,
            )
            px = thumb_rect.x() + (thumb_rect.width() - scaled.width()) // 2
            py = thumb_rect.y() + (thumb_rect.height() - scaled.height()) // 2
            painter.drawPixmap(px, py, scaled)

        is_hovered = getattr(self, "hovered_file_path", None) == file_path

        if selected or is_hovered:
            text_rect = QRect(
                rect.x() + 4,
                rect.bottom() - self.TILE_TEXT_H - 4,
                rect.width() - 8,
                self.TILE_TEXT_H,
            )
            painter.fillRect(text_rect, QColor(255, 255, 255, 220))
            painter.setPen(QColor(0, 0, 0))
            painter.drawText(
                text_rect,
                Qt.AlignmentFlag.AlignHCenter
                | Qt.AlignmentFlag.AlignTop
                | Qt.TextFlag.TextWordWrap,
                file_path.name,
            )

        painter.restore()

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        self._update_scrollbars()

    def zoom_in(self) -> None:
        self.zoom_factor *= 1.2
        self.zoom_factor = min(self.zoom_factor, 5.0)
        self.thumb_cache.clear()
        self._update_scrollbars()

    def zoom_out(self) -> None:
        self.zoom_factor /= 1.2
        self.zoom_factor = max(self.zoom_factor, 0.2)
        self.thumb_cache.clear()
        self._update_scrollbars()

    def wheelEvent(self, event: QWheelEvent) -> None:
        if event.modifiers() & Qt.KeyboardModifier.ControlModifier:
            delta = event.angleDelta().y()
            if delta > 0:
                self.zoom_in()
            elif delta < 0:
                self.zoom_out()
            event.accept()
            return

        delta = event.angleDelta().y()
        sb = self.verticalScrollBar()
        sb.setValue(sb.value() - delta // 2)
        event.accept()

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        pos = self.content_pos(event.position().toPoint())
        new_hover = None
        for hit in self.tile_hits:
            if hit.rect.contains(pos):
                new_hover = hit.file_path
                break

        if new_hover != self.hovered_file_path:
            self.hovered_file_path = new_hover
            self.viewport().update()

        super().mouseMoveEvent(event)

    def leaveEvent(self, event) -> None:
        if getattr(self, "hovered_file_path", None) is not None:
            self.hovered_file_path = None
            self.viewport().update()
        super().leaveEvent(event)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        pos = self.content_pos(event.position().toPoint())
        modifiers = event.modifiers()

        if event.button() == Qt.MouseButton.RightButton:
            for hit in self.header_hits:
                if hit.rect.contains(pos):
                    from image_tagger.config import config, save_config

                    path_str = str(hit.node.path)
                    if path_str in config.excluded_directories:
                        config.excluded_directories.remove(path_str)
                    else:
                        config.excluded_directories.add(path_str)
                    save_config()
                    self.viewport().update()
                    return

            for hit in self.tile_hits:
                if hit.rect.contains(pos):
                    if hit.file_path not in self.selected_files:
                        self.selected_files = {hit.file_path}
                        self.last_clicked_file = hit.file_path
                        self.viewport().update()

                    menu = QMenu(self)
                    sxiv_action = menu.addAction("open in sxiv")

                    def open_sxiv():
                        import subprocess

                        subprocess.Popen(
                            ["sxiv"] + [str(p) for p in self.selected_files]
                        )

                    sxiv_action.triggered.connect(open_sxiv)

                    copy_path_action = menu.addAction("copy path")

                    def copy_path():
                        paths = "\n".join(str(p.resolve()) for p in self.selected_files)
                        QApplication.clipboard().setText(paths)

                    copy_path_action.triggered.connect(copy_path)

                    menu.exec(event.globalPosition().toPoint())
                    return

        for hit in self.header_hits:
            if hit.toggle_rect.contains(pos) or hit.rect.contains(pos):
                hit.node.expanded = not hit.node.expanded
                if hit.node.expanded:
                    hit.node.load()
                self._update_scrollbars()
                return

        for hit in self.tile_hits:
            if hit.rect.contains(pos):
                if modifiers & Qt.KeyboardModifier.ControlModifier:
                    if hit.file_path in self.selected_files:
                        self.selected_files.remove(hit.file_path)
                    else:
                        self.selected_files.add(hit.file_path)
                    self.last_clicked_file = hit.file_path
                elif (
                    modifiers & Qt.KeyboardModifier.ShiftModifier
                    and self.last_clicked_file
                ):
                    paths = [th.file_path for th in self.tile_hits]
                    try:
                        start_idx = paths.index(self.last_clicked_file)
                        end_idx = paths.index(hit.file_path)
                        if start_idx > end_idx:
                            start_idx, end_idx = end_idx, start_idx
                        for p in paths[start_idx : end_idx + 1]:
                            self.selected_files.add(p)
                    except ValueError:
                        self.selected_files = {hit.file_path}
                    # Keep last_clicked_file unchanged for range extension, or update? usually update
                    self.last_clicked_file = hit.file_path
                else:
                    self.selected_files = {hit.file_path}
                    self.last_clicked_file = hit.file_path

                if event.button() == Qt.MouseButton.LeftButton:
                    self.imageClicked.emit(hit.file_path)

                self.viewport().update()
                return

    def mouseDoubleClickEvent(self, event: QMouseEvent) -> None:
        pos = self.content_pos(event.position().toPoint())

        for hit in self.header_hits:
            if hit.rect.contains(pos):
                hit.node.expanded = not hit.node.expanded
                if hit.node.expanded:
                    hit.node.load()
                self._update_scrollbars()
                return

        for hit in self.tile_hits:
            if hit.rect.contains(pos):
                self.selected_files = {hit.file_path}
                self.last_clicked_file = hit.file_path
                self.fileSelected.emit(str(hit.file_path))
                self.viewport().update()
                return


class MainWindow(QMainWindow):
    def __init__(self, root_dir: Path):
        super().__init__()
        self.setWindowTitle(f"Mixed Tree/Tiles Explorer - {root_dir}")
        self.resize(1200, 800)

        self.view = MixedTreeTileView(root_dir)
        self.setCentralWidget(self.view)

        quit_action = QAction("Quit", self)
        quit_action.triggered.connect(self.close)
        self.menuBar().addAction(quit_action)


def main() -> int:
    app = QApplication(sys.argv)

    if len(sys.argv) != 2:
        print(f"Usage: {Path(sys.argv[0]).name} <input-directory>", file=sys.stderr)
        return 2

    root = Path(sys.argv[1])
    if not root.exists():
        print(f"Error: path does not exist: {root}", file=sys.stderr)
        return 2
    if not root.is_dir():
        print(f"Error: path is not a directory: {root}", file=sys.stderr)
        return 2

    try:
        window = MainWindow(root)
        window.show()
        return app.exec()
    except Exception as ex:
        QMessageBox.critical(None, "Fatal Error", str(ex))
        raise


if __name__ == "__main__":
    raise SystemExit(main())
