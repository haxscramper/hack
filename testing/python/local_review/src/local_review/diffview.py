import bisect
from collections.abc import Callable

from beartype import beartype
from PyQt6.QtCore import QRect, QSize, Qt
from PyQt6.QtGui import (
    QColor,
    QFont,
    QFontMetrics,
    QMouseEvent,
    QPainter,
    QPaintEvent,
    QPen,
    QResizeEvent,
)
from PyQt6.QtWidgets import (
    QAbstractScrollArea,
    QWidget,
)

from local_review.annotation_store import AnnotationStore
from local_review.models import (
    AnnotationAnchor,
    AnnotationThread,
    CommentPlacement,
    DiffFile,
    DiffLineKind,
    DiffPresentation,
    DiffRow,
    DiffSide,
    LayoutBlock,
    MarginPlacement,
)
from local_review.review_repository import GitReviewRepository, line_context
from local_review.thread import ThreadWidget


@beartype
class DiffView(QAbstractScrollArea):
    line_height = 24
    hunk_height = 30
    line_number_width = 58
    margin_gap = 10
    margin_width = 410

    def __init__(
        self,
        repository: GitReviewRepository,
        store: AnnotationStore,
        thread_count_callback: Callable[[], None],
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self.repository = repository
        self.store = store
        self.thread_count_callback = thread_count_callback
        self.diff_file: DiffFile | None = None
        self.presentation = DiffPresentation.UNIFIED
        self.placement = CommentPlacement.INLINE
        self.blocks: list[LayoutBlock] = []
        self.block_tops: list[int] = []
        self.row_tops: dict[int, int] = {}
        self.margin_placements: dict[str, MarginPlacement] = {}
        self.visible_widgets: dict[str, ThreadWidget] = {}
        self.selected_row: int | None = None
        self.selected_side = DiffSide.NEW
        self.hovered_row: int | None = None

        self.font_value = QFont("monospace")
        self.font_value.setStyleHint(QFont.StyleHint.Monospace)
        self.viewport().setFont(self.font_value)
        self.viewport().setMouseTracking(True)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOn)
        self.setHorizontalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.verticalScrollBar().valueChanged.connect(self.scroll_changed)

    def sizeHint(self) -> QSize:
        return QSize(1100, 800)

    def set_diff_file(self, diff_file: DiffFile | None) -> None:
        self.diff_file = diff_file
        self.selected_row = None
        self.hovered_row = None
        self.verticalScrollBar().setValue(0)
        self.rebuild_layout()

    def set_presentation(
        self,
        presentation: DiffPresentation,
    ) -> None:
        self.presentation = presentation
        self.rebuild_layout()

    def set_placement(self, placement: CommentPlacement) -> None:
        self.placement = placement
        self.rebuild_layout()

    def threads_for_file(self) -> list[AnnotationThread]:
        if self.diff_file is None:
            return []

        paths = {
            self.diff_file.old_path,
            self.diff_file.new_path,
        }
        return [
            thread for thread in self.store.document.threads
            if thread.anchor.path in paths
        ]

    def matching_threads(
        self,
        row: DiffRow,
    ) -> list[AnnotationThread]:
        result: list[AnnotationThread] = []
        for thread in self.threads_for_file():
            anchor = thread.anchor
            match anchor.side:
                case DiffSide.OLD:
                    line_number = row.old_line_number
                case DiffSide.NEW:
                    line_number = row.new_line_number

            if line_number is None:
                continue
            if anchor.line_start <= line_number <= anchor.line_end:
                result.append(thread)

        return result

    def thread_height(self, thread: AnnotationThread) -> int:
        editor_height = sum(
            max(
                72,
                min(
                    220,
                    34 + max(1, len(comment.text.splitlines())) * 20,
                ),
            ) for comment in thread.comments)
        return 54 + editor_height + 6 * len(thread.comments)

    def rebuild_layout(self) -> None:
        self.blocks.clear()
        self.block_tops.clear()
        self.row_tops.clear()
        self.margin_placements.clear()

        if self.diff_file is None:
            self.verticalScrollBar().setRange(0, 0)
            self.update_visible_widgets()
            self.viewport().update()
            return

        top = 0
        for row_index, row in enumerate(self.diff_file.rows):
            height = (self.hunk_height
                      if row.kind == DiffLineKind.HUNK else self.line_height)
            self.row_tops[row_index] = top
            self.blocks.append(
                LayoutBlock(
                    row_index=row_index,
                    thread_id=None,
                    top=top,
                    height=height,
                ))
            self.block_tops.append(top)
            top += height

            if self.placement == CommentPlacement.INLINE:
                for thread in self.matching_threads(row):
                    thread_height = self.thread_height(thread)
                    self.blocks.append(
                        LayoutBlock(
                            row_index=None,
                            thread_id=thread.id,
                            top=top,
                            height=thread_height + self.margin_gap,
                        ))
                    self.block_tops.append(top)
                    top += thread_height + self.margin_gap

        if self.placement == CommentPlacement.MARGIN:
            margin_top = 0
            placed: set[str] = set()
            for row_index, row in enumerate(self.diff_file.rows):
                for thread in self.matching_threads(row):
                    if thread.id in placed:
                        continue
                    placed.add(thread.id)
                    anchor_y = self.row_tops[row_index]
                    thread_height = self.thread_height(thread)
                    thread_top = max(anchor_y, margin_top)
                    self.margin_placements[thread.id] = MarginPlacement(
                        thread_id=thread.id,
                        anchor_y=anchor_y,
                        top=thread_top,
                        height=thread_height,
                    )
                    margin_top = thread_top + thread_height + self.margin_gap
            top = max(top, margin_top)

        maximum = max(0, top - self.viewport().height())
        self.verticalScrollBar().setRange(0, maximum)
        self.verticalScrollBar().setPageStep(self.viewport().height())
        self.update_visible_widgets()
        self.viewport().update()

    def thread_by_id(self, thread_id: str) -> AnnotationThread:
        return self.store.find_thread(thread_id)

    def update_visible_widgets(self) -> None:
        scroll = self.verticalScrollBar().value()
        viewport_height = self.viewport().height()
        visible_ids: set[str] = set()
        rectangles: dict[str, QRect] = {}

        if self.placement == CommentPlacement.INLINE:
            for block in self.blocks:
                if block.thread_id is None:
                    continue
                viewport_top = block.top - scroll
                if viewport_top + block.height < 0:
                    continue
                if viewport_height < viewport_top:
                    continue

                width = max(
                    200,
                    self.viewport().width() - self.line_number_width - 24,
                )
                rectangles[block.thread_id] = QRect(
                    self.line_number_width + 12,
                    viewport_top + 4,
                    width,
                    block.height - self.margin_gap,
                )
                visible_ids.add(block.thread_id)
        else:
            left = max(
                0,
                self.viewport().width() - self.margin_width,
            )
            width = max(
                200,
                self.viewport().width() - left - 10,
            )
            for placement in self.margin_placements.values():
                viewport_top = placement.top - scroll
                if viewport_top + placement.height < 0:
                    continue
                if viewport_height < viewport_top:
                    continue

                rectangles[placement.thread_id] = QRect(
                    left + 5,
                    viewport_top,
                    width,
                    placement.height,
                )
                visible_ids.add(placement.thread_id)

        stale_ids = set(self.visible_widgets) - visible_ids
        for thread_id in stale_ids:
            widget = self.visible_widgets.pop(thread_id)
            widget.hide()
            widget.deleteLater()

        for thread_id in visible_ids:
            widget = self.visible_widgets.get(thread_id)
            if widget is None:
                thread = self.thread_by_id(thread_id)
                widget = ThreadWidget(
                    thread=thread,
                    update_callback=self.update_comment,
                    reply_callback=self.append_reply,
                    remove_callback=self.remove_thread,
                    layout_callback=self.rebuild_layout,
                    parent=self.viewport(),
                )
                self.visible_widgets[thread_id] = widget
                widget.show()

            rectangle = rectangles[thread_id]
            widget.setGeometry(rectangle)
            widget.raise_()

    def update_comment(
        self,
        thread_id: str,
        comment_id: str,
        text: str,
    ) -> None:
        self.store.update_comment(
            thread_id,
            comment_id,
            text,
        )

    def append_reply(self, thread_id: str) -> None:
        self.store.append_reply(thread_id)
        widget = self.visible_widgets.pop(thread_id, None)
        if widget is not None:
            widget.hide()
            widget.deleteLater()
        self.rebuild_layout()

    def remove_thread(self, thread_id: str) -> None:
        self.store.remove_thread(thread_id)
        self.thread_count_callback()
        widget = self.visible_widgets.pop(thread_id, None)
        if widget is not None:
            widget.hide()
            widget.deleteLater()
        self.rebuild_layout()

    def scroll_changed(self, value: int) -> None:
        self.update_visible_widgets()
        self.viewport().update()

    def resizeEvent(self, event: QResizeEvent) -> None:
        super().resizeEvent(event)
        self.rebuild_layout()

    def block_at_viewport_y(self, viewport_y: int) -> LayoutBlock | None:
        if not self.block_tops:
            return None

        absolute_y = viewport_y + self.verticalScrollBar().value()
        index = bisect.bisect_right(self.block_tops, absolute_y) - 1
        if index < 0:
            return None
        if len(self.blocks) <= index:
            return None

        block = self.blocks[index]
        if block.top + block.height <= absolute_y:
            return None
        return block

    def side_at_x(self, x: int) -> DiffSide:
        if self.presentation == DiffPresentation.UNIFIED:
            if self.selected_row is not None and self.diff_file is not None:
                row = self.diff_file.rows[self.selected_row]
                if row.kind == DiffLineKind.DELETION:
                    return DiffSide.OLD
            return DiffSide.NEW

        code_width = self.code_width()
        if x < code_width // 2:
            return DiffSide.OLD
        return DiffSide.NEW

    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() != Qt.MouseButton.LeftButton:
            super().mousePressEvent(event)
            return

        block = self.block_at_viewport_y(int(event.position().y()))
        if block is None or block.row_index is None:
            return

        row = self.diff_file.rows[block.row_index] if self.diff_file else None
        if row is None or row.kind == DiffLineKind.HUNK:
            return

        self.selected_row = block.row_index
        self.selected_side = self.side_at_x(int(event.position().x()))

        if int(event.position().x()) < 30:
            self.add_comment_for_selection()

        self.viewport().update()

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        block = self.block_at_viewport_y(int(event.position().y()))
        hovered = (block.row_index if block is not None
                   and block.row_index is not None else None)
        if hovered != self.hovered_row:
            self.hovered_row = hovered
            self.viewport().update()

    def leaveEvent(self, event) -> None:
        self.hovered_row = None
        self.viewport().update()

    def add_comment_for_selection(self) -> None:
        if self.diff_file is None or self.selected_row is None:
            return

        row = self.diff_file.rows[self.selected_row]
        side = self.selected_side
        match side:
            case DiffSide.OLD:
                line_number = row.old_line_number
            case DiffSide.NEW:
                line_number = row.new_line_number

        if line_number is None:
            side = DiffSide.NEW if row.new_line_number is not None else DiffSide.OLD
            line_number = (row.new_line_number
                           if side == DiffSide.NEW else row.old_line_number)

        if line_number is None:
            return

        path, blob_oid, snapshot_oid, lines = self.repository.source_for_anchor(
            self.diff_file,
            side,
        )
        if len(lines) < line_number:
            raise ValueError(
                f"Line {line_number} is outside {path!r}, which has {len(lines)} lines"
            )

        selected_text = lines[line_number - 1]
        before, after = line_context(
            lines,
            line_number,
            line_number,
        )
        thread = AnnotationThread(anchor=AnnotationAnchor(
            path=path,
            side=side,
            line_start=line_number,
            line_end=line_number,
            snapshot_commit_oid=snapshot_oid,
            blob_oid=blob_oid,
            selected_text=selected_text,
            context_before=before,
            context_after=after,
        ))
        self.store.document.threads.append(thread)
        self.store.save()
        self.thread_count_callback()
        self.rebuild_layout()

    def code_width(self) -> int:
        if self.placement == CommentPlacement.MARGIN:
            return max(
                300,
                self.viewport().width() - self.margin_width,
            )
        return self.viewport().width()

    def paintEvent(self, event: QPaintEvent) -> None:
        painter = QPainter(self.viewport())
        painter.setRenderHint(
            QPainter.RenderHint.TextAntialiasing,
            True,
        )
        palette = self.viewport().palette()
        painter.fillRect(
            self.viewport().rect(),
            palette.base(),
        )
        painter.setFont(self.font_value)

        if self.diff_file is None:
            painter.setPen(palette.text().color())
            painter.drawText(
                self.viewport().rect(),
                Qt.AlignmentFlag.AlignCenter,
                "Select a changed file",
            )
            return

        scroll = self.verticalScrollBar().value()
        viewport_height = self.viewport().height()
        if self.block_tops:
            index = max(
                0,
                bisect.bisect_right(self.block_tops, scroll) - 1,
            )
        else:
            index = 0

        while index < len(self.blocks):
            block = self.blocks[index]
            viewport_top = block.top - scroll
            if viewport_height < viewport_top:
                break
            if block.row_index is not None:
                self.paint_row(
                    painter,
                    block.row_index,
                    viewport_top,
                    block.height,
                )
            index += 1

        if self.placement == CommentPlacement.MARGIN:
            self.paint_margin_connectors(painter, scroll)

    def paint_row(
        self,
        painter: QPainter,
        row_index: int,
        top: int,
        height: int,
    ) -> None:
        if self.diff_file is None:
            return

        row = self.diff_file.rows[row_index]
        code_width = self.code_width()
        rectangle = QRect(0, top, code_width, height)
        palette = self.viewport().palette()
        dark = palette.base().color().lightness() < 128

        if row.kind == DiffLineKind.HUNK:
            color = QColor(44, 62, 80) if dark else QColor(221, 235, 247)
            painter.fillRect(rectangle, color)
            painter.setPen(palette.text().color())
            painter.drawText(
                rectangle.adjusted(10, 0, -10, 0),
                Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                row.header or "",
            )
            return

        if row_index == self.selected_row:
            selection = palette.highlight().color()
            selection.setAlpha(70)
            painter.fillRect(rectangle, selection)
        else:
            match row.kind:
                case DiffLineKind.ADDITION:
                    color = QColor(25, 60, 40) if dark else QColor(
                        230, 255, 237)
                    painter.fillRect(rectangle, color)
                case DiffLineKind.DELETION:
                    color = QColor(72, 35, 38) if dark else QColor(
                        255, 235, 233)
                    painter.fillRect(rectangle, color)
                case DiffLineKind.CONTEXT:
                    pass

        if row_index == self.hovered_row:
            painter.setPen(palette.highlight().color())
            painter.drawEllipse(QRect(6, top + 3, 18, 18))
            painter.drawLine(11, top + 12, 19, top + 12)
            painter.drawLine(15, top + 8, 15, top + 16)

        match self.presentation:
            case DiffPresentation.UNIFIED:
                self.paint_unified_row(
                    painter,
                    row,
                    top,
                    height,
                    code_width,
                )
            case DiffPresentation.SPLIT:
                self.paint_split_row(
                    painter,
                    row,
                    top,
                    height,
                    code_width,
                )

    def paint_unified_row(
        self,
        painter: QPainter,
        row: DiffRow,
        top: int,
        height: int,
        width: int,
    ) -> None:
        palette = self.viewport().palette()
        painter.setPen(palette.mid().color())
        number_rectangle = QRect(28, top, 94, height)
        old_number = str(
            row.old_line_number) if row.old_line_number is not None else ""
        new_number = str(
            row.new_line_number) if row.new_line_number is not None else ""
        painter.drawText(
            number_rectangle.adjusted(0, 0, -48, 0),
            Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter,
            old_number,
        )
        painter.drawText(
            number_rectangle.adjusted(48, 0, 0, 0),
            Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter,
            new_number,
        )

        match row.kind:
            case DiffLineKind.ADDITION:
                marker = "+"
                text = row.new_text or ""
            case DiffLineKind.DELETION:
                marker = "-"
                text = row.old_text or ""
            case DiffLineKind.CONTEXT:
                marker = " "
                text = row.new_text or ""

        painter.setPen(palette.text().color())
        painter.drawText(
            QRect(128, top, 16, height),
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            marker,
        )
        metrics = QFontMetrics(self.font_value)
        visible_text = metrics.elidedText(
            text.expandtabs(4),
            Qt.TextElideMode.ElideRight,
            max(0, width - 152),
        )
        painter.drawText(
            QRect(146, top, max(0, width - 152), height),
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            visible_text,
        )

    def paint_split_row(
        self,
        painter: QPainter,
        row: DiffRow,
        top: int,
        height: int,
        width: int,
    ) -> None:
        half = width // 2
        palette = self.viewport().palette()
        painter.setPen(palette.mid().color())
        painter.drawLine(half, top, half, top + height)

        self.paint_split_cell(
            painter,
            QRect(0, top, half, height),
            row.old_line_number,
            row.old_text,
            row.kind == DiffLineKind.DELETION,
            "-",
        )
        self.paint_split_cell(
            painter,
            QRect(half, top, width - half, height),
            row.new_line_number,
            row.new_text,
            row.kind == DiffLineKind.ADDITION,
            "+",
        )

    def paint_split_cell(
        self,
        painter: QPainter,
        rectangle: QRect,
        line_number: int | None,
        text: str | None,
        changed: bool,
        marker: str,
    ) -> None:
        palette = self.viewport().palette()
        number_width = self.line_number_width
        painter.setPen(palette.mid().color())
        painter.drawText(
            QRect(
                rectangle.left() + 26,
                rectangle.top(),
                number_width - 30,
                rectangle.height(),
            ),
            Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter,
            str(line_number) if line_number is not None else "",
        )

        if text is None:
            return

        painter.setPen(palette.text().color())
        painter.drawText(
            QRect(
                rectangle.left() + number_width + 4,
                rectangle.top(),
                14,
                rectangle.height(),
            ),
            Qt.AlignmentFlag.AlignVCenter,
            marker if changed else " ",
        )
        available = rectangle.width() - number_width - 24
        metrics = QFontMetrics(self.font_value)
        visible_text = metrics.elidedText(
            text.expandtabs(4),
            Qt.TextElideMode.ElideRight,
            max(0, available),
        )
        painter.drawText(
            QRect(
                rectangle.left() + number_width + 20,
                rectangle.top(),
                max(0, available),
                rectangle.height(),
            ),
            Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
            visible_text,
        )

    def paint_margin_connectors(
        self,
        painter: QPainter,
        scroll: int,
    ) -> None:
        code_width = self.code_width()
        palette = self.viewport().palette()
        pen = QPen(palette.mid().color())
        pen.setWidth(1)
        painter.setPen(pen)

        for placement in self.margin_placements.values():
            anchor_y = placement.anchor_y - scroll + self.line_height // 2
            thread_y = placement.top - scroll + 16
            if anchor_y < 0 and thread_y < 0:
                continue
            if (self.viewport().height() < anchor_y
                    and self.viewport().height() < thread_y):
                continue

            painter.drawLine(
                code_width - 8,
                anchor_y,
                code_width + 16,
                anchor_y,
            )
            painter.drawLine(
                code_width + 16,
                anchor_y,
                code_width + 16,
                thread_y,
            )
            painter.drawLine(
                code_width + 16,
                thread_y,
                code_width + 28,
                thread_y,
            )
