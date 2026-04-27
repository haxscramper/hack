#!/usr/bin/env -S uv run
# /// script
# dependencies = ["pyside6>=6.7"]
# ///
"""
Mixed tree/tile view for browsing image directories.

This module implements :class:`MixedTreeTileView`, a custom
:class:`~PySide6.QtWidgets.QAbstractScrollArea` that renders a directory tree
as collapsible headers with image files displayed in a flowing grid (tile) layout
beneath each expanded directory.

**Key design points**

* **Lazy loading** -- :class:`DirNode` scans a directory only when first expanded.
* **Layout-as-you-paint** -- The recursive layout engine (:meth:`MixedTreeTileView._build_layout`)
  is run inside ``paintEvent``. It both paints visible items and records their geometry in
  ``header_hits`` / ``tile_hits`` so that subsequent mouse events can hit-test against the
  exact same geometry without a separate layout pass.
* **Async thumbnails** -- Thumbnails are generated in background threads via
  :class:`ThumbTask` / :class:`ThumbSignals` and a :class:`~PySide6.QtCore.QThreadPool`.
  A small in-memory cache stores the resulting pixmaps; visible tiles request thumbnails
  on-demand during painting.
* **Sorting** -- Image lists are sorted through :func:`image_tagger.db.sorting.sort_paths`,
  supporting name-based orders as well as similarity-based ordering backed by a
  :class:`~image_tagger.db.sorting.SimilarityIndex`.
* **State persistence** -- The view can serialize and restore expansion, zoom,
  scroll position, selection, sort mode, and weighted tag filters via
  :class:`~image_tagger.gui.state_models.MixedViewState`.

Control flow for a typical frame
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. ``paintEvent`` clears the hit lists and creates a ``QPainter`` translated by the
   current vertical scroll value.
2. ``_build_layout`` walks the :class:`DirNode` tree recursively. For each node it
   appends a :class:`HeaderHit`, then (if expanded) lays out child directories and
   image tiles, appending :class:`TileHit` records and painting directly when a painter
   is present.
3. ``_paint_tile`` checks the thumbnail cache. If the image is missing and the tile
   intersects the visible (plus prefetch) area, it calls ``request_thumbnail``,
   which starts a :class:`ThumbTask` in the thread pool. A placeholder is drawn
   immediately.
4. When the worker finishes, ``on_thumbnail_loaded`` stores the pixmap and triggers
   a short single-shot timer to repaint the viewport.
5. Mouse events use the hit lists populated by the most recent paint to toggle
   directories, select tiles, or show context menus.
"""

from __future__ import annotations

import math
import os
import sys
from dataclasses import dataclass, field
from beartype import beartype
from pathlib import Path

from sqlalchemy.orm import Session
from image_tagger.gui.state_models import MixedViewState
from image_tagger.utils.utils import confirm_clear_selection
from image_tagger.db.sorting import SimilarityIndex, SortMode, sort_paths

from pytestqt.qtbot import QtBot
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
    QDialog,
    QVBoxLayout,
    QDialogButtonBox,
    QLabel,
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
    """Return ``True`` if *path* exists and its suffix is a known image extension."""
    return path.is_file() and path.suffix.lower() in IMAGE_EXTENSIONS


@dataclass
class DirNode:
    """
    A node in the browsable directory tree.

    Each node represents a single filesystem directory. Children (sub-directories
    and image files) are loaded lazily via :meth:`load` and cached in
    :attr:`child_dirs` and :attr:`image_files`. The node also remembers its
    visual expansion state (:attr:`expanded`).
    """

    path: Path
    expanded: bool = True
    children_loaded: bool = False
    child_dirs: list["DirNode"] = field(default_factory=list)
    image_files: list[Path] = field(default_factory=list)

    def load(
        self,
        sort_mode: SortMode = SortMode.NAME_ASC,
        similarity_index: SimilarityIndex | None = None,
        reference_path: Path | None = None,
        weighted_tags: dict[int, float] | None = None,
    ) -> None:
        """
        Scan the directory on disk and populate children.

        If the node was already loaded, this delegates to :meth:`resort` instead
        of re-reading the filesystem. Permission errors while enumerating entries
        are silently ignored.
        """
        if self.children_loaded:
            self.resort(
                sort_mode=sort_mode,
                similarity_index=similarity_index,
                reference_path=reference_path,
                weighted_tags=weighted_tags,
            )
            return

        child_dirs: list[DirNode] = []
        image_files: list[Path] = []

        try:
            entries = sorted(
                self.path.iterdir(),
                key=lambda p: (not p.is_dir(), p.name.lower()),
            )
        except (PermissionError, FileNotFoundError, OSError):
            entries = []

        for entry in entries:
            try:
                if entry.is_dir():
                    child_dirs.append(DirNode(entry, expanded=False))
                elif is_image_file(entry):
                    image_files.append(entry)
            except (PermissionError, OSError):
                pass

        self.child_dirs = child_dirs
        self.image_files = sort_paths(
            paths=image_files,
            sort_mode=sort_mode,
            similarity_index=similarity_index,
            reference_path=reference_path,
            weighted_tags=weighted_tags,
        )
        self.children_loaded = True

    def resort(
        self,
        sort_mode: SortMode,
        similarity_index: SimilarityIndex | None = None,
        reference_path: Path | None = None,
        weighted_tags: dict[int, float] | None = None,
    ) -> None:
        """
        Re-sort :attr:`image_files` without touching the filesystem.

        Called when the user changes sort mode or similarity parameters.
        """
        self.image_files = sort_paths(
            paths=self.image_files,
            sort_mode=sort_mode,
            similarity_index=similarity_index,
            reference_path=reference_path,
            weighted_tags=weighted_tags,
        )


@dataclass
class HeaderHit:
    """
    Geometry record produced by the layout engine for a directory header.

    Used by mouse-event handlers to detect clicks on headers and their
    expand/collapse toggle buttons.
    """

    node: DirNode
    rect: QRect
    toggle_rect: QRect
    depth: int


@dataclass
class TileHit:
    """
    Geometry record produced by the layout engine for an image tile.

    Used by mouse-event handlers for selection, hover, and context menus.
    """

    file_path: Path
    rect: QRect
    depth: int


class ThumbSignals(QObject):
    """
    Signal carrier used to pass thumbnail results from worker threads back to the GUI.

    Must be a QObject so it can participate in Qt's signal/slot mechanism across
    thread boundaries.
    """

    loaded = Signal(str, QImage)


class ThumbTask(QRunnable):
    """
    Background task that reads an image file and produces a scaled thumbnail.

    The task uses :class:`~PySide6.QtGui.QImageReader` with auto-transform
    enabled, computes a uniform scale to fit the target thumb size, and emits
    the result through :class:`ThumbSignals`.
    """

    def __init__(self, path: Path, thumb_size: int, signals: ThumbSignals):
        super().__init__()
        self.path = path
        self.thumb_size = thumb_size
        self.signals = signals

    def run(self) -> None:
        """Read the image, scale it, and emit ``loaded``."""
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
    """
    Scrollable mixed tree/tile viewer for image directories.

    The widget displays a recursive directory structure. Each directory is
    rendered as a horizontal header row with an expand/collapse arrow. When a
    directory is expanded, its image files are laid out in a left-to-right,
    top-to-bottom grid directly beneath the header, followed by its expanded
    sub-directories.

    Thumbnails are loaded asynchronously in a thread pool and cached. The view
    supports zooming (Ctrl + mouse wheel), multi-selection (Ctrl/Shift click),
    context menus, and sorting via :mod:`image_tagger.db.sorting`.

    Signals
    -------
    imageClicked : Signal(object)
        Emitted with a :class:`~pathlib.Path` when a tile is left-clicked.
    fileSelected : Signal(object)
        Emitted with a ``str`` path when a tile is double-clicked.
    """

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
    SELECTION_CLEAR_CONFIRM_THRESHOLD = 3

    def __init__(self, root_path: Path, session: Session, parent=None):
        """
        Create the view, build the similarity index, and load the root directory.

        Parameters
        ----------
        root_path : Path
            Top-level directory to browse.
        session : Session
            SQLAlchemy session used for similarity-based sorting.
        parent : QWidget, optional
            Parent widget.
        """
        super().__init__(parent)
        self.zoom_factor = 1.0

        self.session = session
        self.sort_mode = SortMode.NAME_ASC
        # Reference image to sort similarity against
        self.similarity_reference_path: Path | None = None
        # Weighted tags for SIMILARITY_TO_WEIGHTED_TAGS mode
        self.weighted_tags: dict[int, float] | None = None

        self.similarity_index = SimilarityIndex(root_path)
        self.similarity_index.build(self.session)

        self.root_node = DirNode(root_path.resolve(), expanded=True)
        self.root_node.load(**self.get_node_load())

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
        """Current tile width in pixels, derived from zoomed thumb size and margins."""
        return self.thumb_size + self.TILE_MARGIN * 2

    @property
    def tile_h(self) -> int:
        """Current tile height in pixels, derived from zoomed thumb size and margins."""
        return self.thumb_size + self.TILE_MARGIN * 2

    @property
    def thumb_size(self) -> int:
        """Thumbnail edge length after applying the current zoom factor."""
        return int(self.THUMB_SIZE * self.zoom_factor)

    def visible_width(self) -> int:
        """Viewport width clamped to at least 1 pixel."""
        return max(1, self.viewport().width())

    def flow_columns(self, depth: int) -> int:
        """
        Compute how many tiles fit horizontally for a node at *depth*.

        Accounts for content margins and per-depth indentation.
        """
        available = self.visible_width(
        ) - 2 * self.CONTENT_MARGIN - depth * self.INDENT
        available = max(available, self.tile_w)
        return max(1, (available + self.TILE_SPACING_X) //
                   (self.tile_w + self.TILE_SPACING_X))

    def layout_height_for_images(self, count: int, depth: int) -> int:
        """Total pixel height needed to display *count* images at *depth*."""
        if count == 0:
            return 0
        cols = self.flow_columns(depth)
        rows = math.ceil(count / cols)
        return rows * self.tile_h + max(0, rows - 1) * self.TILE_SPACING_Y

    def total_content_height(self) -> int:
        """
        Measure the full content height and refresh hit-test lists.

        Runs :meth:`_build_layout` without a painter solely to repopulate
        :attr:`header_hits` and :attr:`tile_hits` and determine the Y extent.
        """
        self.header_hits.clear()
        self.tile_hits.clear()
        _, y = self._build_layout(None, 0, self.CONTENT_MARGIN)
        return max(self.viewport().height(), y + self.CONTENT_MARGIN)

    def get_node_load(self) -> dict:
        """
        Build the keyword-argument dict used for :meth:`DirNode.load` and
        :meth:`DirNode.resort` from the view's current sort settings.
        """
        return dict(
            sort_mode=self.sort_mode,
            similarity_index=self.similarity_index,
            reference_path=self.similarity_reference_path,
            weighted_tags=self.weighted_tags,
        )

    def refresh(self) -> None:
        """Reload directory structure while preserving expansion state of directories."""
        # Collect currently expanded paths before reload
        expanded_paths: set[Path] = set()

        def collect_expanded(node: DirNode) -> None:
            if node.expanded:
                expanded_paths.add(node.path)
            # Only recurse into loaded children to avoid lazy-loading everything
            if node.children_loaded:
                for child in node.child_dirs:
                    collect_expanded(child)

        collect_expanded(self.root_node)

        # Clear root to force reload
        self.root_node.children_loaded = False
        self.root_node.child_dirs = []
        self.root_node.image_files = []
        self.root_node.load(**self.get_node_load())

        # Restore expansion state
        def apply_expanded(node: DirNode) -> None:
            if node.path in expanded_paths:
                node.expanded = True
                node.load(**self.get_node_load()
                          )  # Load children so we can recurse into them
            for child in node.child_dirs:
                apply_expanded(child)

        apply_expanded(self.root_node)
        self.thumb_cache.clear()  # Clear stale thumbnails
        self._update_scrollbars()

    def _update_scrollbars(self) -> None:
        """Recalculate vertical scrollbar range from :meth:`total_content_height`."""
        total = self.total_content_height()
        page = max(1, self.viewport().height())
        self.verticalScrollBar().setPageStep(page)
        self.verticalScrollBar().setRange(0, max(0, total - page))
        self.viewport().update()

    def content_pos(self, viewport_pos: QPoint) -> QPoint:
        """Map a viewport-local point to content coordinates using scroll offset."""
        return viewport_pos + QPoint(0, self.verticalScrollBar().value())

    def visible_content_rect(self) -> QRect:
        """
        Return the content rectangle considered "visible" including the prefetch margin.

        Tiles that intersect this rect may trigger asynchronous thumbnail loads.
        """
        y = self.verticalScrollBar().value()
        return QRect(
            0,
            y - self.PREFETCH_MARGIN,
            self.viewport().width(),
            self.viewport().height() + 2 * self.PREFETCH_MARGIN,
        )

    def request_thumbnail(self, path: Path) -> None:
        """
        Enqueue a background thumbnail load for *path* if not already cached or loading.
        """
        if path in self.thumb_cache or path in self.loading_paths:
            return
        self.loading_paths.add(path)
        self.thread_pool.start(
            ThumbTask(path, self.thumb_size, self.thumb_signals))

    def on_thumbnail_loaded(self, path_str: str, image: QImage) -> None:
        """
        Slot receiving the result of a :class:`ThumbTask`.

        Stores the image in :attr:`thumb_cache` (or a gray placeholder on failure)
        and schedules a deferred viewport repaint via :attr:`_update_timer`.
        """
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

    def set_sort_mode(
        self,
        sort_mode: SortMode,
        reference_path: Path | None = None,
        weighted_tags: dict[int, float] | None = None,
    ) -> None:
        """
        Change the sort mode and re-sort every already-loaded node recursively.

        Parameters
        ----------
        sort_mode : SortMode
            New sort mode to apply.
        reference_path : Path, optional
            Image to use as a similarity reference.
        weighted_tags : dict[int, float], optional
            Tag ID -> weight mapping for weighted tag similarity sorting.
        """
        self.sort_mode = sort_mode
        self.similarity_reference_path = reference_path
        self.weighted_tags = weighted_tags

        def resort_recursive(node: DirNode) -> None:
            node.resort(
                sort_mode=self.sort_mode,
                similarity_index=self.similarity_index,
                reference_path=self.similarity_reference_path,
                weighted_tags=self.weighted_tags,
            )
            for child in node.child_dirs:
                resort_recursive(child)

        resort_recursive(self.root_node)

    def _build_layout(self,
                      painter: QPainter | None,
                      depth: int,
                      y: int,
                      node: DirNode | None = None) -> tuple[int, int]:
        """
        Recursively lay out headers and image tiles starting at *y*.

        If *painter* is not ``None``, visible elements are painted immediately.
        Regardless, this populates :attr:`header_hits` and :attr:`tile_hits`
        so that later input events can hit-test against the exact geometry.

        Returns
        -------
        tuple[int, int]
            The final (depth, y) after laying out the subtree.
        """
        if node is None:
            node = self.root_node

        vx = self.CONTENT_MARGIN + depth * self.INDENT
        vw = self.visible_width(
        ) - 2 * self.CONTENT_MARGIN - depth * self.INDENT
        vw = max(vw, 100)

        header_rect = QRect(vx, y, vw, self.HEADER_HEIGHT)
        toggle_rect = QRect(vx, y + 6, 16, 16)
        self.header_hits.append(
            HeaderHit(node=node,
                      rect=header_rect,
                      toggle_rect=toggle_rect,
                      depth=depth))

        if painter is not None:
            self._paint_header(painter, node, header_rect, toggle_rect, depth)

        y += self.HEADER_HEIGHT + 4

        if node.expanded:
            node.load(**self.get_node_load())

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
                        TileHit(file_path=file_path, rect=rect, depth=depth))
                    if painter is not None:
                        self._paint_tile(
                            painter,
                            file_path,
                            rect,
                            selected=(file_path in self.selected_files),
                        )

                y += self.layout_height_for_images(len(node.image_files),
                                                   depth)
                y += self.SECTION_SPACING

        return depth, y

    def paintEvent(self, event: QPaintEvent) -> None:
        """
        Main paint entry point.

        Clears the hit lists, fills the viewport background, applies scroll
        translation, and runs :meth:`_build_layout` with an active painter.
        """
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
        """
        Draw a single directory header: background, expand/collapse arrow, label, and counts.

        Excluded directories (from app config) are tinted red.
        """
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
            poly = QPolygon([
                QPoint(cx - 4, cy - 2),
                QPoint(cx + 4, cy - 2),
                QPoint(cx, cy + 4),
            ])
        else:
            poly = QPolygon([
                QPoint(cx - 2, cy - 4),
                QPoint(cx - 2, cy + 4),
                QPoint(cx + 4, cy),
            ])

        painter.drawPolygon(poly)

        painter.setPen(self.fg)
        font = painter.font()
        font.setBold(depth == 0)
        painter.setFont(font)

        text_x = toggle_rect.right() + 6
        text_rect = QRect(text_x, rect.y(),
                          rect.width() - (text_x - rect.x()), rect.height())
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

    def _paint_placeholder_thumb(self, painter: QPainter, thumb_rect: QRect,
                                 file_path: Path) -> None:
        """Draw a dashed placeholder with the file extension while a thumbnail loads."""
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

    def _paint_tile(self, painter: QPainter, file_path: Path, rect: QRect,
                    selected: bool) -> None:
        """
        Draw a single image tile: background, border, thumbnail (or placeholder), and filename overlay.

        The thumbnail is drawn centered and scaled with smooth transformation.
        A semi-transparent filename overlay appears for selected or hovered tiles.
        """
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
        """Update scrollbars when the viewport changes size."""
        super().resizeEvent(event)
        self._update_scrollbars()

    def zoom_in(self) -> None:
        """Increase zoom factor by 20 % (capped at 5.0) and invalidate thumbnails."""
        self.zoom_factor *= 1.2
        self.zoom_factor = min(self.zoom_factor, 5.0)
        self.thumb_cache.clear()
        self._update_scrollbars()

    def zoom_out(self) -> None:
        """Decrease zoom factor by 20 % (floor at 0.2) and invalidate thumbnails."""
        self.zoom_factor /= 1.2
        self.zoom_factor = max(self.zoom_factor, 0.2)
        self.thumb_cache.clear()
        self._update_scrollbars()

    def wheelEvent(self, event: QWheelEvent) -> None:
        """
        Ctrl + wheel zooms the tile view; normal wheel scrolls vertically.
        """
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
        """
        Track the tile currently under the cursor to enable hover highlighting.
        """
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
        """Clear hover state when the mouse leaves the widget."""
        if getattr(self, "hovered_file_path", None) is not None:
            self.hovered_file_path = None
            self.viewport().update()
        super().leaveEvent(event)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        """
        Handle header toggles, tile selection, and context menus.

        * Left click on header toggle arrow or header body expands/collapses.
        * Right click on header toggles the directory exclusion flag.
        * Right click on a tile opens a context menu (open in sxiv, copy path).
        * Left click on a tile selects it; Ctrl toggles, Shift ranges.
        * If clearing a large selection (>3 items), a confirmation dialog is shown.
        """
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

                        subprocess.Popen(["sxiv"] +
                                         [str(p) for p in self.selected_files])

                    sxiv_action.triggered.connect(open_sxiv)

                    copy_path_action = menu.addAction("copy path")

                    def copy_path():
                        paths = "\n".join(
                            str(p.resolve()) for p in self.selected_files)
                        QApplication.clipboard().setText(paths)

                    copy_path_action.triggered.connect(copy_path)

                    menu.exec(event.globalPosition().toPoint())
                    return

        for hit in self.header_hits:
            if hit.toggle_rect.contains(pos) or hit.rect.contains(pos):
                hit.node.expanded = not hit.node.expanded
                if hit.node.expanded:
                    hit.node.load(**self.get_node_load())
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
                elif (modifiers & Qt.KeyboardModifier.ShiftModifier
                      and self.last_clicked_file):
                    paths = [th.file_path for th in self.tile_hits]
                    try:
                        start_idx = paths.index(self.last_clicked_file)
                        end_idx = paths.index(hit.file_path)
                        if start_idx > end_idx:
                            start_idx, end_idx = end_idx, start_idx
                        for p in paths[start_idx:end_idx + 1]:
                            self.selected_files.add(p)
                    except ValueError:
                        self.selected_files = {hit.file_path}
                    # Keep last_clicked_file unchanged for range extension, or update? usually update
                    self.last_clicked_file = hit.file_path
                else:
                    if (len(self.selected_files)
                            > self.SELECTION_CLEAR_CONFIRM_THRESHOLD):
                        if not confirm_clear_selection(
                                self, len(self.selected_files)):
                            return

                    self.selected_files = {hit.file_path}
                    self.last_clicked_file = hit.file_path

                if event.button() == Qt.MouseButton.LeftButton:
                    self.imageClicked.emit(hit.file_path)

                self.viewport().update()
                return

    def mouseDoubleClickEvent(self, event: QMouseEvent) -> None:
        """
        Toggle directory expansion on header double-click, or emit ``fileSelected``
        when double-clicking a tile.
        """
        pos = self.content_pos(event.position().toPoint())

        for hit in self.header_hits:
            if hit.rect.contains(pos):
                hit.node.expanded = not hit.node.expanded
                if hit.node.expanded:
                    hit.node.load(**self.get_node_load())
                self._update_scrollbars()
                return

        for hit in self.tile_hits:
            if hit.rect.contains(pos):
                self.selected_files = {hit.file_path}
                self.last_clicked_file = hit.file_path
                self.fileSelected.emit(str(hit.file_path))
                self.viewport().update()
                return

    def get_tile_rect(self, file_path: Path) -> QRect | None:
        """Return tile rectangle in content coordinates, None if not currently laid out."""
        for hit in self.tile_hits:
            if hit.file_path == file_path:
                return hit.rect
        return None

    def get_header_rect(self, node_path: Path) -> QRect | None:
        """Return header rectangle in content coordinates, None if not found."""
        for hit in self.header_hits:
            if hit.node.path == node_path:
                return hit.rect
        return None

    def get_toggle_rect(self, node_path: Path) -> QRect | None:
        """Return the expand/collapse toggle button rect for a directory."""
        for hit in self.header_hits:
            if hit.node.path == node_path:
                return hit.toggle_rect
        return None

    def scroll_to_content_y(self, y: int, center: bool = True) -> None:
        """Scroll vertically to bring content y-coordinate into view."""
        sb = self.verticalScrollBar()
        if center:
            target = max(0, y - self.viewport().height() // 2)
        else:
            target = max(0, y - self.HEADER_HEIGHT)
        sb.setValue(min(target, sb.maximum()))

    def is_element_visible(self, rect: QRect) -> bool:
        """Check if the content rect is currently within the viewport."""
        if not rect:
            return False
        visible = self.visible_content_rect()
        return visible.intersects(rect)

    def get_element_click_pos(self, node_path: Path) -> QPoint:
        rect = self.get_tile_rect(node_path)
        assert rect
        scroll_y = self.verticalScrollBar().value()
        return QPoint(rect.center().x(), rect.center().y() - scroll_y)

    def toggle_subdir(self, subdir: Path, qtbot: QtBot):
        """
        Automation helper: expand or collapse *subdir* by clicking its toggle arrow.

        If the header is not currently in the hit list, the view scrolls to bring
        it into range first.
        """
        from PySide6.QtTest import QTest

        toggle_rect = self.get_toggle_rect(subdir)

        # If not found, scroll to make header visible
        if toggle_rect is None:
            header_rect = self.get_header_rect(subdir)
            if header_rect:
                self.scroll_to_content_y(header_rect.center().y())
                qtbot.wait(150)
                toggle_rect = self.get_toggle_rect(subdir)

        qtbot.wait(150)
        assert toggle_rect is not None, f"Could not find toggle for directory {subdir}"

        # Click toggle to expand
        scroll_y = self.verticalScrollBar().value()
        click_pos = QPoint(toggle_rect.center().x(),
                           toggle_rect.center().y() - scroll_y)
        QTest.mouseClick(self.viewport(),
                         Qt.MouseButton.LeftButton,
                         pos=click_pos)
        qtbot.wait(50)

    def get_state(self) -> MixedViewState:
        """
        Serialize the current view state.

        Captures expanded directory paths, zoom level, scroll offset, selected
        files, active sort mode, similarity reference, and weighted tag entries.
        """
        from image_tagger.gui.state_models import MixedViewState, WeightedTagEntry

        expanded_paths: set[str] = set()

        def collect_expanded(node: DirNode) -> None:
            if node.expanded:
                expanded_paths.add(str(node.path))
            if node.children_loaded:
                for child in node.child_dirs:
                    collect_expanded(child)

        collect_expanded(self.root_node)

        weighted_entries = []
        if self.weighted_tags:
            # Map tag IDs back to names for serialization
            from sqlalchemy import select
            from image_tagger.db.models import ProbabilisticTag

            tag_ids = list(self.weighted_tags.keys())
            rows = self.session.execute(
                select(ProbabilisticTag.id, ProbabilisticTag.name).where(
                    ProbabilisticTag.id.in_(tag_ids))).all()
            id_to_name = {tid: name for tid, name in rows}
            weighted_entries = [
                WeightedTagEntry(
                    tag_name=id_to_name.get(tid, ""),
                    weight=weight,
                ) for tid, weight in self.weighted_tags.items()
                if id_to_name.get(tid, "")
            ]

        return MixedViewState(
            expanded_paths=sorted(expanded_paths),
            zoom_factor=self.zoom_factor,
            scroll_y=self.verticalScrollBar().value(),
            selected_files=sorted(str(p) for p in self.selected_files),
            sort_mode=self.sort_mode.name,
            similarity_reference_path=str(self.similarity_reference_path)
            if self.similarity_reference_path else None,
            weighted_tag_entries=weighted_entries,
        )

    def set_state(self, state: MixedViewState) -> None:
        """
        Restore view state from a :class:`~image_tagger.gui.state_models.MixedViewState`.

        Applies zoom, expands previously open directories, restores scroll position
        and selection, and clears the thumbnail cache so that zoom is respected.
        """
        self.zoom_factor = state.zoom_factor
        self.thumb_cache.clear()

        expanded_set = set(state.expanded_paths)

        def apply_expanded(node: DirNode) -> None:
            node.expanded = str(node.path) in expanded_set
            if node.expanded:
                node.load(**self.get_node_load())
            for child in node.child_dirs:
                apply_expanded(child)

        apply_expanded(self.root_node)
        self._update_scrollbars()
        self.verticalScrollBar().setValue(state.scroll_y)

        self.selected_files = {Path(p) for p in state.selected_files}
        self.viewport().update()
