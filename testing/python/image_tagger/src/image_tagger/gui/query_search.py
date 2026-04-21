from __future__ import annotations
from beartype.typing import Optional, Any
from beartype import beartype

from PySide6.QtWidgets import (
    QWidget,
    QVBoxLayout,
    QPushButton,
    QLabel,
    QSplitter,
    QAbstractItemView,
    QCompleter,
    QPlainTextEdit,
    QTextEdit,
)
from PySide6.QtCore import Qt, Signal, QStringListModel
from PySide6.QtGui import (
    QTextCursor,
    QKeyEvent,
    QTextCharFormat,
    QColor,
    QSyntaxHighlighter,
)
from pathlib import Path
import os
import logging
import re

from image_tagger.gui.image_list_widget import ImageListWidget
from sqlalchemy.orm import Session
from sqlalchemy import select, text, func
from image_tagger.db.models import (
    ImageEntry,
    ProbabilisticTag,
    ImageProbabilisticTag,
    RegularTag,
    ImageRegularTag,
    ImageDescription,
)

from sexpdata import loads, dumps, Symbol


class ImageThumbnailList(ImageListWidget):
    """Displays image thumbnails in an icon-mode list."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.list_view.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection
        )

    def set_images(self, images: list):
        """Set the images to display in the thumbnail list."""
        from pathlib import Path

        # Accept list or sequence of paths
        image_paths = [Path(p) if isinstance(p, str) else p for p in images]
        self.model.set_images(image_paths)
        self.list_view.clearSelection()

    def get_selected_images(self) -> list[Path]:
        """Return the selected images as a list of Path objects."""
        indexes = self.list_view.selectedIndexes()
        return [self.model.images[idx.row()] for idx in indexes]

    def set_selection(self, paths: set[Path]) -> None:
        """Set the selection to the given set of file paths."""
        from PySide6.QtCore import QItemSelectionModel

        self.list_view.clearSelection()
        selection_model = self.list_view.selectionModel()

        for path in paths:
            # Convert to string for comparison if needed
            path_str = str(path)
            for idx, img_path in enumerate(self.model.images):
                if img_path == path or str(img_path) == path_str:
                    index = self.model.index(idx, 0)
                    selection_model.select(
                        index, QItemSelectionModel.SelectionFlag.Select
                    )
                    break


class SexpHighlighter(QSyntaxHighlighter):
    """Simple syntax highlighter for S-expressions."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._keyword_format = QTextCharFormat()
        self._keyword_format.setForeground(QColor("#0077aa"))
        self._keyword_format.setFontWeight(700)

        self._string_format = QTextCharFormat()
        self._string_format.setForeground(QColor("#690"))

        self._comment_format = QTextCharFormat()
        self._comment_format.setForeground(QColor("#999"))

        self._keywords = {
            "and",
            "or",
            "not",
            "probabilistic_tag",
            "regular_tag",
            "description",
            "path_contains",
        }

    def highlightBlock(self, text: str):
        # Highlight keywords
        for kw in self._keywords:
            for m in re.finditer(r"\b" + re.escape(kw) + r"\b", text):
                self.setFormat(m.start(), m.end() - m.start(), self._keyword_format)

        # Highlight strings
        for m in re.finditer(r'"([^"\\]|\\.)*"', text):
            self.setFormat(m.start(), m.end() - m.start(), self._string_format)

        # Highlight comments
        for m in re.finditer(r";.*$", text):
            self.setFormat(m.start(), m.end() - m.start(), self._comment_format)


class SexpTextEdit(QPlainTextEdit):
    """Multi-line text edit with autocomplete for S-expressions."""

    def __init__(self, session: Session, parent=None):
        super().__init__(parent)
        self.session = session
        self._completer = QCompleter()
        self._completer.setWidget(self)
        self._completer.setCompletionMode(QCompleter.CompletionMode.PopupCompletion)
        self._completer.setCaseSensitivity(Qt.CaseSensitivity.CaseInsensitive)
        self._completer.setFilterMode(Qt.MatchFlag.MatchContains)
        self._completer.activated.connect(self._insert_completion)

        self._highlighter = SexpHighlighter(self.document())

        self.setPlaceholderText(
            "Enter S-expression query, e.g.:\n"
            "(and (probabilistic_tag general castle 0.5)\n"
            "     (regular_tag category1 tag_a))"
        )
        self.setLineWrapMode(QPlainTextEdit.LineWrapMode.WidgetWidth)

        self._load_suggestions()

    def _load_suggestions(self):
        prob_cats = sorted(
            set(
                r[0]
                for r in self.session.execute(
                    select(ProbabilisticTag.category).distinct()
                ).all()
            )
        )
        reg_cats = sorted(
            set(
                r[0]
                for r in self.session.execute(
                    select(RegularTag.category).distinct()
                ).all()
            )
        )
        prob_names = sorted(
            set(
                r[0]
                for r in self.session.execute(
                    select(ProbabilisticTag.name).distinct()
                ).all()
            )
        )
        reg_names = sorted(
            set(
                r[0]
                for r in self.session.execute(select(RegularTag.name).distinct()).all()
            )
        )

        self._prob_categories = prob_cats
        self._reg_categories = reg_cats
        self._prob_names = prob_names
        self._reg_names = reg_names

        # Base suggestions: function names
        self._base_suggestions = [
            "and",
            "or",
            "not",
            "probabilistic_tag",
            "regular_tag",
            "description",
            "path_contains",
        ]

    def _current_word(self) -> tuple[str, int, int]:
        """Return (word, start, end) of the word under the cursor."""
        cursor = self.textCursor()
        pos = cursor.position()
        block_text = cursor.block().text()
        col = pos - cursor.block().position()

        # Find word boundaries
        start = col
        while start > 0 and block_text[start - 1] not in ' \t\n()"':
            start -= 1
        end = col
        while end < len(block_text) and block_text[end] not in ' \t\n()"':
            end += 1

        return block_text[start:end], start, end

    def _get_context_suggestions(self) -> list[str]:
        """Determine autocomplete suggestions based on cursor context."""
        text = self.toPlainText()
        cursor = self.textCursor()
        pos = cursor.position()

        # Find the enclosing expression context by looking backwards
        before = text[:pos]

        # Try to find the most recent unclosed '(' and what function it contains
        depth = 0
        i = len(before) - 1
        func_name = None
        arg_index = 0

        while i >= 0:
            c = before[i]
            if c == ")":
                depth += 1
            elif c == "(":
                if depth == 0:
                    # Found our opening paren, extract function name
                    m = re.match(r"\s*([a-zA-Z_][a-zA-Z0-9_]*)", before[i + 1 :])
                    if m:
                        func_name = m.group(1)
                        # Count arguments between i+1 and pos
                        inner = before[i + 1 + len(func_name) :]
                        arg_index = inner.count(" ") + inner.count("\n")
                        # More accurate: split by whitespace outside parens
                        arg_index = self._count_args(inner)
                    break
                else:
                    depth -= 1
            i -= 1

        if func_name == "probabilistic_tag":
            if arg_index == 0:
                return self._prob_categories
            elif arg_index == 1:
                # Try to find category
                cat = self._get_arg_at_index(before, 0)
                if cat:
                    return sorted(
                        r[0]
                        for r in self.session.execute(
                            select(ProbabilisticTag.name).where(
                                ProbabilisticTag.category == cat
                            )
                        ).all()
                    )
                return self._prob_names
            elif arg_index == 2:
                return []  # probability is a number
        elif func_name == "regular_tag":
            if arg_index == 0:
                return self._reg_categories
            elif arg_index == 1:
                cat = self._get_arg_at_index(before, 0)
                if cat:
                    return sorted(
                        r[0]
                        for r in self.session.execute(
                            select(RegularTag.name).where(RegularTag.category == cat)
                        ).all()
                    )
                return self._reg_names
        elif func_name in ("description", "path_contains"):
            return []
        elif func_name in ("and", "or", "not"):
            return self._base_suggestions

        return self._base_suggestions

    def _count_args(self, inner: str) -> int:
        """Count top-level arguments in inner text."""
        depth = 0
        args = 0
        in_word = False
        for c in inner:
            if c == "(":
                depth += 1
            elif c == ")":
                depth -= 1
            elif depth == 0 and c not in " \t\n":
                if not in_word:
                    args += 1
                    in_word = True
            elif depth == 0 and c in " \t\n":
                in_word = False
        return max(0, args - 1)  # subtract 1 for function name

    def _get_arg_at_index(self, before_cursor: str, index: int) -> Optional[str]:
        """Try to extract the argument at the given index from the current expression."""
        # Find the opening paren of current expression
        depth = 0
        i = len(before_cursor) - 1
        while i >= 0:
            c = before_cursor[i]
            if c == ")":
                depth += 1
            elif c == "(":
                if depth == 0:
                    break
                depth -= 1
            i -= 1
        if i < 0:
            return None

        expr_text = before_cursor[i:]
        m = re.match(r"\(\s*([a-zA-Z_][a-zA-Z0-9_]*)\s+", expr_text)
        if not m:
            return None

        func_start = m.end()
        rest = expr_text[func_start:]

        # Parse arguments
        args = []
        current = ""
        depth = 0
        in_string = False
        for c in rest:
            if in_string:
                current += c
                if c == '"' and (len(current) < 2 or current[-2] != "\\"):
                    in_string = False
            elif c == '"':
                current += c
                in_string = True
            elif c == "(":
                depth += 1
                current += c
            elif c == ")":
                if depth == 0:
                    if current.strip():
                        args.append(current.strip())
                    break
                depth -= 1
                current += c
            elif c in " \t\n" and depth == 0:
                if current.strip():
                    args.append(current.strip())
                    current = ""
            else:
                current += c

        if current.strip():
            args.append(current.strip())

        if index < len(args):
            val = args[index]
            # Remove quotes if present
            if val.startswith('"') and val.endswith('"'):
                val = val[1:-1]
            return val
        return None

    def keyPressEvent(self, event: QKeyEvent):
        if self._completer.popup().isVisible():
            if event.key() in (
                Qt.Key.Key_Enter,
                Qt.Key.Key_Return,
                Qt.Key.Key_Tab,
                Qt.Key.Key_Escape,
            ):
                event.ignore()
                if event.key() == Qt.Key.Key_Escape:
                    self._completer.popup().hide()
                return

        super().keyPressEvent(event)

        word, start, end = self._current_word()
        if len(word) < 1:
            self._completer.popup().hide()
            return

        suggestions = self._get_context_suggestions()
        filtered = [s for s in suggestions if word.lower() in s.lower()]
        if not filtered:
            self._completer.popup().hide()
            return

        self._completer.setModel(QStringListModel(filtered))
        self._completer.setCompletionPrefix(word)
        self._completer.popup().setCurrentIndex(
            self._completer.completionModel().index(0, 0)
        )

        cr = self.cursorRect()
        cr.setWidth(self._completer.popup().sizeHintForColumn(0) + 20)
        self._completer.complete(cr)

    def _insert_completion(self, completion: str):
        cursor = self.textCursor()
        word, start, end = self._current_word()
        block_pos = cursor.block().position()
        cursor.setPosition(block_pos + start)
        cursor.setPosition(block_pos + end, QTextCursor.MoveMode.KeepAnchor)
        cursor.insertText(completion)
        self.setTextCursor(cursor)

    def refresh_suggestions(self):
        """Reload tag/category suggestions from the database."""
        self._load_suggestions()


def _symbol_to_str(val: Any) -> str:
    """Convert a sexpdata Symbol or string to a Python string."""
    if isinstance(val, Symbol):
        return val.value()
    if isinstance(val, str):
        return val
    raise ValueError(f"Expected Symbol or str, got {type(val)}: {val}")


def _sexp_to_spec(sexp: Any) -> Optional[dict]:
    """Convert a parsed sexpdata expression to the internal filter spec dict."""
    if not isinstance(sexp, list) or not sexp:
        return None

    op = _symbol_to_str(sexp[0])

    if op == "probabilistic_tag":
        if len(sexp) < 3:
            return None
        category = _symbol_to_str(sexp[1])
        name = _symbol_to_str(sexp[2])
        min_probability = 0.5
        if len(sexp) >= 4:
            min_probability = float(sexp[3])
        return {
            "type": "probabilistic_tag",
            "category": category,
            "name": name,
            "min_probability": min_probability,
        }

    elif op == "regular_tag":
        if len(sexp) < 3:
            return None
        return {
            "type": "regular_tag",
            "category": _symbol_to_str(sexp[1]),
            "name": _symbol_to_str(sexp[2]),
        }

    elif op == "description":
        if len(sexp) < 2:
            return None
        return {
            "type": "description",
            "text": _symbol_to_str(sexp[1]),
        }

    elif op == "path_contains":
        if len(sexp) < 2:
            return None
        return {
            "type": "path_contains",
            "text": _symbol_to_str(sexp[1]),
        }

    elif op == "and":
        children = [_sexp_to_spec(child) for child in sexp[1:]]
        children = [c for c in children if c is not None]
        if not children:
            return None
        if len(children) == 1:
            return children[0]
        return {"type": "and", "children": children}

    elif op == "or":
        children = [_sexp_to_spec(child) for child in sexp[1:]]
        children = [c for c in children if c is not None]
        if not children:
            return None
        if len(children) == 1:
            return children[0]
        return {"type": "or", "children": children}

    elif op == "not":
        if len(sexp) < 2:
            return None
        child = _sexp_to_spec(sexp[1])
        if child is None:
            return None
        return {"type": "not", "child": child}

    return None


def _spec_to_sexp(spec: dict) -> Any:
    """Convert an internal filter spec dict to a sexpdata expression."""
    t = spec["type"]
    if t == "probabilistic_tag":
        return [
            Symbol("probabilistic_tag"),
            Symbol(spec["category"]),
            Symbol(spec["name"]),
            spec.get("min_probability", 0.5),
        ]
    elif t == "regular_tag":
        return [Symbol("regular_tag"), Symbol(spec["category"]), Symbol(spec["name"])]
    elif t == "description":
        return [Symbol("description"), spec["text"]]
    elif t == "path_contains":
        return [Symbol("path_contains"), spec["text"]]
    elif t == "and":
        return [Symbol("and"), *[_spec_to_sexp(c) for c in spec["children"]]]
    elif t == "or":
        return [Symbol("or"), *[_spec_to_sexp(c) for c in spec["children"]]]
    elif t == "not":
        return [Symbol("not"), _spec_to_sexp(spec["child"])]
    raise ValueError(f"Unknown spec type: {t}")


def build_query(session: Session, spec: dict):
    t = spec["type"]

    if t == "probabilistic_tag":
        tag = session.execute(
            select(ProbabilisticTag.id).where(
                ProbabilisticTag.category == spec["category"],
                ProbabilisticTag.name == spec["name"],
            )
        ).scalar()
        if tag is None:
            return select(ImageEntry.id).where(text("0"))
        return select(ImageProbabilisticTag.image_id).where(
            ImageProbabilisticTag.tag_id == tag,
            ImageProbabilisticTag.probability >= spec["min_probability"],
        )

    elif t == "regular_tag":
        tag = session.execute(
            select(RegularTag.id).where(
                RegularTag.category == spec["category"],
                RegularTag.name == spec["name"],
            )
        ).scalar()
        if tag is None:
            return select(ImageEntry.id).where(text("0"))
        return select(ImageRegularTag.image_id).where(ImageRegularTag.tag_id == tag)

    elif t == "description":
        return select(ImageDescription.image_id).where(
            ImageDescription.description.contains(spec["text"])
        )

    elif t == "path_contains":
        return select(ImageEntry.id).where(
            func.instr(ImageEntry.relative_path, spec["text"]) > 0
        )

    elif t == "and":
        subqueries = [build_query(session, c) for c in spec["children"]]
        if not subqueries:
            return select(ImageEntry.id).where(text("0"))
        result = subqueries[0].subquery()
        for sq in subqueries[1:]:
            sq_sub = sq.subquery()
            result = (
                select(result.c.image_id).where(
                    result.c.image_id.in_(select(sq_sub.c.image_id))
                )
            ).subquery()
        return select(result.c.image_id)

    elif t == "or":
        subqueries = [build_query(session, c) for c in spec["children"]]
        if not subqueries:
            return select(ImageEntry.id).where(text("0"))
        result = subqueries[0].subquery()
        for sq in subqueries[1:]:
            sq_sub = sq.subquery()
            result = (
                select(result.c.image_id).union(select(sq_sub.c.image_id))
            ).subquery()
        return select(result.c.image_id)

    elif t == "not":
        child_query = build_query(session, spec["child"]).subquery()
        return select(ImageEntry.id).where(
            ImageEntry.id.notin_(select(child_query.c.image_id))
        )

    raise ValueError(f"Unknown filter spec type: {t}")


class SearchTab(QWidget):
    """Tab with S-expression query builder for filtering images."""

    results_found = Signal(list)

    def __init__(self, session: Session, base_dir: str, parent=None):
        super().__init__(parent)
        self.session = session
        self.base_dir = base_dir

        layout = QVBoxLayout(self)

        splitter = QSplitter(Qt.Orientation.Vertical)

        # Expression builder area
        expr_container = QWidget()
        expr_layout = QVBoxLayout(expr_container)
        expr_layout.setContentsMargins(0, 0, 0, 0)

        logging.info(f"Root dir for search tab: {base_dir}")

        self.sexp_input = SexpTextEdit(session)
        expr_layout.addWidget(self.sexp_input)

        search_btn = QPushButton("Search")
        search_btn.clicked.connect(self._execute_search)
        expr_layout.addWidget(search_btn)

        self.status_label = QLabel("")
        expr_layout.addWidget(self.status_label)

        splitter.addWidget(expr_container)

        # Results area
        self.thumbnail_list = ImageThumbnailList()
        splitter.addWidget(self.thumbnail_list)

        splitter.setStretchFactor(0, 1)
        splitter.setStretchFactor(1, 2)
        layout.addWidget(splitter)

    def add_tag_to_query(self, tag_type: str, category: str, name: str):
        """Append a tag condition to the current S-expression query."""
        current_text = self.sexp_input.toPlainText().strip()

        if tag_type == "probabilistic_tag":
            new_expr = [
                Symbol("probabilistic_tag"),
                Symbol(category),
                Symbol(name),
                0.5,
            ]
        else:
            new_expr = [Symbol("regular_tag"), Symbol(category), Symbol(name)]

        if not current_text:
            self.sexp_input.setPlainText(dumps(new_expr))
            return

        try:
            parsed = loads(current_text)
        except Exception:
            # If current text is invalid, replace it
            self.sexp_input.setPlainText(dumps(new_expr))
            return

        # If current expression is already an 'and', append the new condition
        if (
            isinstance(parsed, list)
            and len(parsed) > 0
            and _symbol_to_str(parsed[0]) == "and"
        ):
            parsed.append(new_expr)
            self.sexp_input.setPlainText(dumps(parsed))
        else:
            # Wrap existing expression and new one in an 'and'
            combined = [Symbol("and"), parsed, new_expr]
            self.sexp_input.setPlainText(dumps(combined))

    def set_search_spec(self, spec: dict) -> None:
        """Set the query from an internal filter spec dict."""
        sexp = _spec_to_sexp(spec)
        self.sexp_input.setPlainText(dumps(sexp))

    def execute_search(self) -> None:
        self._execute_search()

    def get_result_images(self) -> list[Path]:
        return list(self.thumbnail_list.model.images)

    def clear_search(self) -> None:
        self.sexp_input.setPlainText("")
        self.thumbnail_list.set_images([])
        self.status_label.setText("")

    def _execute_search(self):
        text = self.sexp_input.toPlainText().strip()
        if not text:
            self.status_label.setText("No valid filter conditions specified.")
            self.thumbnail_list.set_images([])
            return

        try:
            parsed = loads(text)
        except Exception as e:
            self.status_label.setText(f"Parse error: {e}")
            self.thumbnail_list.set_images([])
            return

        spec = _sexp_to_spec(parsed)
        if spec is None:
            self.status_label.setText("No valid filter conditions specified.")
            self.thumbnail_list.set_images([])
            return

        paths = self._resolve_search_results(spec)

        if not paths:
            self.status_label.setText("No images found.")
            self.thumbnail_list.set_images([])
            return

        self.status_label.setText(f"Found {len(paths)} images.")
        self.thumbnail_list.set_images(paths)

    def _resolve_search_results(self, spec: dict) -> list[str]:
        if spec["type"] == "path_contains":
            return self._search_by_path(spec["text"])
        if spec["type"] == "and":
            result_sets = [
                set(self._resolve_search_results(c)) for c in spec["children"]
            ]
            if not result_sets:
                return []
            return sorted(set.intersection(*result_sets))
        if spec["type"] == "or":
            result_sets = [
                set(self._resolve_search_results(c)) for c in spec["children"]
            ]
            if not result_sets:
                return []
            return sorted(set.union(*result_sets))
        if spec["type"] == "not":
            all_images = set(self._search_by_path(""))
            excluded = set(self._resolve_search_results(spec["child"]))
            return sorted(all_images - excluded)

        query = build_query(self.session, spec)
        image_ids = [r[0] for r in self.session.execute(query).all()]
        if not image_ids:
            return []
        images = self.session.execute(
            select(ImageEntry.relative_path).where(ImageEntry.id.in_(image_ids))
        ).all()
        paths = []
        for (rel_path,) in images:
            full_path = os.path.join(self.base_dir, rel_path)
            if os.path.isfile(full_path):
                paths.append(full_path)
        return paths

    def _search_by_path(self, text: str) -> list[str]:
        paths = []
        base = Path(self.base_dir)
        for file_path in base.rglob("*"):
            if not file_path.is_file():
                continue
            rel = str(file_path.relative_to(base))
            if text in rel:
                paths.append(str(file_path))
        return sorted(paths)
