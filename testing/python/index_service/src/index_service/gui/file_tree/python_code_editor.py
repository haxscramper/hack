from PyQt6.Qsci import QsciLexerPython, QsciScintilla
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFontDatabase, QKeyEvent, QColor
from PyQt6.QtWidgets import QWidget
from beartype import beartype
from dataclasses import dataclass
import traceback
import logging

log = logging.getLogger(__name__)

QUERY_FILENAME = "<glom-query>"
QUERY_ERROR_INDICATOR = 8


@dataclass(frozen=True)
class QueryLocation:
    line: int  # QScintilla: zero-based
    start_column: int  # QScintilla: zero-based
    end_column: int  # exclusive


class QueryError(Exception):

    def __init__(self, message: str, location: QueryLocation | None = None) -> None:
        super().__init__(message)
        self.location = location


def query_exception_location(exc: Exception) -> QueryLocation | None:
    if isinstance(exc, SyntaxError) and exc.filename == QUERY_FILENAME:
        # SyntaxError lines are one-based, and offsets are one-based.
        line = max(0, (exc.lineno or 1) - 1)
        start = max(0, (exc.offset or 1) - 1)
        end = max(start + 1, (exc.end_offset or (start + 2)) - 1)
        return QueryLocation(line, start, end)

    # The last matching entry is the innermost source location in query code.
    frames = traceback.extract_tb(exc.__traceback__)
    for frame in reversed(frames):
        if frame.filename == QUERY_FILENAME:
            # FrameSummary columns are already zero-based.
            start = frame.colno or 0
            end = frame.end_colno or (start + 1)
            return QueryLocation(frame.lineno - 1, start, max(start + 1, end))

    return None


def as_query_error(exc: Exception) -> QueryError | None:
    location = query_exception_location(exc)
    if location is None:
        return None

    if isinstance(exc, SyntaxError):
        message = exc.msg
    else:
        message = f"{type(exc).__name__}: {exc}"

    return QueryError(message, location)


@beartype
class PythonQueryEditor(QsciScintilla):
    """Python editor with common IDE-style editing behavior."""

    _DELIMITER_PAIRS = {
        "(": ")",
        "[": "]",
        "{": "}",
        "'": "'",
        '"': '"',
    }

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._configure_editor()

    def _configure_editor(self) -> None:
        font = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont,)
        font.setFixedPitch(True)

        self.python_lexer = QsciLexerPython(self)
        self.python_lexer.setDefaultFont(font)

        for style in range(1 << self.python_lexer.styleBitsNeeded()):
            self.python_lexer.setFont(font, style)

        self.setFont(font)
        self.setLexer(self.python_lexer)
        self.setUtf8(True)

        self.setAutoIndent(True)
        self.setIndentationsUseTabs(False)
        self.setIndentationWidth(4)
        self.setTabWidth(4)
        self.setIndentationGuides(True)

        self.setBraceMatching(QsciScintilla.BraceMatch.SloppyBraceMatch,)
        self.setCaretLineVisible(True)

        self.setMarginType(
            0,
            QsciScintilla.MarginType.NumberMargin,
        )
        self.setMarginLineNumbers(0, True)
        self.setMarginWidth(0, "00000")

        self.setWrapMode(QsciScintilla.WrapMode.WrapNone)

        self.indicatorDefine(
            QsciScintilla.IndicatorStyle.SquiggleIndicator,
            QUERY_ERROR_INDICATOR,
        )

        self.setIndicatorForegroundColor(
            QColor("#d73a49"),
            QUERY_ERROR_INDICATOR,
        )

        self.setAnnotationDisplay(QsciScintilla.AnnotationDisplay.AnnotationBoxed,)

    def keyPressEvent(self, event: QKeyEvent) -> None:
        modifiers = event.modifiers()
        key = event.key()

        if (key == Qt.Key.Key_Backspace and
                modifiers == Qt.KeyboardModifier.NoModifier and
                not self.hasSelectedText() and self._delete_blank_line_indentation()):
            event.accept()
            return

        if (key == Qt.Key.Key_Delete and modifiers == Qt.KeyboardModifier.ShiftModifier):
            self.SendScintilla(QsciScintilla.SCI_LINEDELETE)
            event.accept()
            return

        if (key == Qt.Key.Key_Slash and modifiers == Qt.KeyboardModifier.ControlModifier):
            self._toggle_current_line_comment()
            event.accept()
            return

        opening = event.text()
        if (self.hasSelectedText() and opening in self._DELIMITER_PAIRS and
                not modifiers &
            (Qt.KeyboardModifier.ControlModifier | Qt.KeyboardModifier.AltModifier |
             Qt.KeyboardModifier.MetaModifier)):
            selected = self.selectedText()
            closing = self._DELIMITER_PAIRS[opening]
            self.replaceSelectedText(f"{opening}{selected}{closing}")
            event.accept()
            return

        super().keyPressEvent(event)

    def _delete_blank_line_indentation(self) -> bool:
        line, column = self.getCursorPosition()
        content = self.text(line).rstrip("\r\n")

        if column == 0 or content.strip():
            return False

        width = self.indentationWidth()
        delete_count = column % width or width
        start_column = max(0, column - delete_count)

        self.setSelection(line, start_column, line, column)
        self.replaceSelectedText("")
        return True

    def _toggle_current_line_comment(self) -> None:
        line, column = self.getCursorPosition()
        content = self.text(line).rstrip("\r\n")
        indentation_length = len(content) - len(content.lstrip(" \t"))
        remainder = content[indentation_length:]

        if remainder.startswith("# "):
            marker_length = 2
            updated = content[:indentation_length] + remainder[marker_length:]
            updated_column = (column -
                              marker_length if column > indentation_length else column)
        elif remainder.startswith("#"):
            marker_length = 1
            updated = content[:indentation_length] + remainder[marker_length:]
            updated_column = (column -
                              marker_length if column > indentation_length else column)
        else:
            marker = "# "
            updated = (content[:indentation_length] + marker +
                       content[indentation_length:])
            updated_column = (column +
                              len(marker) if column >= indentation_length else column)

        self.setSelection(line, 0, line, len(content))
        self.replaceSelectedText(updated)
        self.setCursorPosition(line, max(0, updated_column))

    def clear_query_error(self) -> None:
        self.clearAnnotations()

        last_line = self.lines() - 1
        last_column = len(self.text(last_line))
        self.clearIndicatorRange(
            0,
            0,
            last_line,
            last_column,
            QUERY_ERROR_INDICATOR,
        )

    def show_query_error(self, error: QueryError) -> None:
        self.clear_query_error()

        if error.location is None:
            last_line = max(0, self.lines() - 1)
            self.annotate(last_line, str(error), 0)
            self.ensureLineVisible(last_line)
            log.error("got error without location")
            return

        location = error.location

        self.fillIndicatorRange(
            location.line,
            location.start_column,
            location.line,
            location.end_column,
            QUERY_ERROR_INDICATOR,
        )
        self.annotate(location.line, str(error), 0)
        self.setCursorPosition(location.line, location.start_column)
        self.ensureLineVisible(location.line)
