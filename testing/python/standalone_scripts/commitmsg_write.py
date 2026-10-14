#!/usr/bin/env python
# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "PyQt6>=6.7",
#     "PyQt6-QScintilla>=2.14",
# ]
# ///

from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

from PyQt6.Qsci import QsciLexerDiff, QsciLexerMarkdown, QsciScintilla
from PyQt6.QtCore import Qt, QThread, QTimer, pyqtSignal
from PyQt6.QtGui import QFontDatabase
from PyQt6.QtWidgets import (
    QAbstractItemView,
    QApplication,
    QHBoxLayout,
    QLabel,
    QMainWindow,
    QMessageBox,
    QPushButton,
    QScrollArea,
    QSplitter,
    QTreeWidget,
    QTreeWidgetItem,
    QVBoxLayout,
    QWidget,
)

HUNK_HEADER = re.compile(r"^@@ -\d+(?:,\d+)? \+\d+(?:,\d+)? @@")
BACKTICKS = re.compile(r"`+")


class GitError(RuntimeError):
    pass


@dataclass(frozen=True)
class Change:
    path: str
    index_status: str
    worktree_status: str

    @property
    def staged(self) -> bool:
        return self.index_status not in (" ", "?")

    @property
    def unstaged(self) -> bool:
        return self.worktree_status != " "

    @property
    def untracked(self) -> bool:
        return self.index_status == "?"


@dataclass(frozen=True)
class FileDiff:
    section: str
    path: str
    text: str

    @property
    def key(self) -> tuple[str, str]:
        return self.section, self.path


class Repository:

    def __init__(self, path: Path):
        result = subprocess.run(
            ["git", "-C",
             str(path), "rev-parse", "--show-toplevel"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=True,
        )
        self.root = Path(os.fsdecode(result.stdout.rstrip(b"\n")))

        git_path = self.run(
            ["rev-parse", "--git-path", "COMMIT_EDITMSG.haxscramper-tmp"])
        self.message_path = Path(os.fsdecode(git_path.rstrip(b"\n")))
        if not self.message_path.is_absolute():
            self.message_path = self.root / self.message_path

    def process(self, arguments: list[str]) -> subprocess.CompletedProcess:
        return subprocess.run(
            ["git", "-C", str(self.root), *arguments],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            env={
                **os.environ, "GIT_TERMINAL_PROMPT": "0"
            },
        )

    def run(
            self,
            arguments: list[str],
            allowed_codes: tuple[int, ...] = (0, ),
    ) -> bytes:
        result = self.process(arguments)
        if result.returncode not in allowed_codes:
            output = result.stdout.decode("utf-8", errors="replace")
            raise GitError(output.strip()
                           or f"Git exited with {result.returncode}")
        return result.stdout

    def changes(self) -> tuple[Change, ...]:
        # Disabling rename detection represents renames as deletion/addition.
        # This also avoids ambiguous filename parsing and staging path pairs.
        output = self.run([
            "-c",
            "status.renames=false",
            "status",
            "--porcelain=v1",
            "-z",
            "--untracked-files=all",
        ])

        changes = []
        for record in output.split(b"\0"):
            if not record:
                continue
            changes.append(
                Change(
                    path=os.fsdecode(record[3:]),
                    index_status=chr(record[0]),
                    worktree_status=chr(record[1]),
                ))
        return tuple(changes)

    def diff(self, change: Change, staged: bool) -> str:
        arguments = [
            "diff",
            "--no-ext-diff",
            "--no-textconv",
            "--no-color",
            "--no-renames",
            "--unified=3",
        ]

        if staged:
            arguments.extend(["--cached", "--", change.path])
            output = self.run(arguments)
        elif change.untracked:
            arguments.extend([
                "--no-index", "--", "/dev/null",
                str(self.root / change.path)
            ])
            output = self.run(arguments, allowed_codes=(0, 1))
        else:
            arguments.extend(["--", change.path])
            output = self.run(arguments)

        return output.decode("utf-8", errors="replace")

    def stage(self, paths: list[str]) -> None:
        if paths:
            self.run(["add", "-A", "--", *paths])

    def unstage(self, paths: list[str]) -> None:
        if not paths:
            return

        head = self.process(["rev-parse", "--verify", "--quiet", "HEAD"])
        if head.returncode == 0:
            self.run(["restore", "--staged", "--", *paths])
        elif head.returncode == 1:
            # An initial commit has no HEAD to restore from.
            self.run(["rm", "--cached", "-f", "--", *paths])
        else:
            raise GitError(head.stdout.decode("utf-8", errors="replace"))

    def save_message(self, text: str) -> None:
        temporary = self.message_path.with_name(self.message_path.name +
                                                ".write-tmp")
        temporary.write_text(text, encoding="utf-8")
        temporary.replace(self.message_path)


def extract_hunks(diff: str) -> list[str]:
    hunks = []
    current = []

    for line in diff.splitlines(keepends=True):
        if HUNK_HEADER.match(line):
            if current:
                hunks.append("".join(current))
            current = [line]
        elif current:
            current.append(line)

    if current:
        hunks.append("".join(current))

    return hunks


def markdown_reference(path: str, hunk: str) -> str:
    path_runs = [len(match.group()) for match in BACKTICKS.finditer(path)]
    path_delimiter = "`" * (max(path_runs, default=0) + 1)
    padding = " " if path.startswith("`") or path.endswith("`") else ""
    pathname = f"{path_delimiter}{padding}{path}{padding}{path_delimiter}"

    hunk_runs = [len(match.group()) for match in BACKTICKS.finditer(hunk)]
    fence = "`" * max(3, max(hunk_runs, default=0) + 1)

    return f"{pathname}:\n\n{fence}diff\n{hunk.rstrip(chr(10))}\n{fence}\n"


def configure_editor(editor: QsciScintilla) -> None:
    font = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
    editor.setUtf8(True)
    editor.setFont(font)
    editor.setMarginsFont(font)
    editor.setTabWidth(4)
    editor.setIndentationsUseTabs(False)
    editor.setMarginWidth(0, 0)
    editor.setMarginWidth(1, 0)
    editor.setFolding(QsciScintilla.FoldStyle.NoFoldStyle)


class CommitWorker(QThread):
    progress = pyqtSignal(str)
    completed = pyqtSignal(bool, str)

    def __init__(
        self,
        repository: Repository,
        staged_paths: set[str],
        parent: QWidget,
    ):
        super().__init__(parent)
        self.repository = repository
        # Freeze the user's selection before hooks have a chance to modify it.
        self.staged_paths = frozenset(staged_paths)

    def run(self) -> None:
        transcript = []

        try:
            for attempt in range(1, 5):
                self.progress.emit(f"Committing: attempt {attempt}/4")

                result = self.repository.process([
                    "commit",
                    "--cleanup=verbatim",
                    "--file",
                    str(self.repository.message_path),
                ])
                output = result.stdout.decode("utf-8", errors="replace")
                transcript.append(
                    f"=== Commit attempt {attempt}/4 ===\n{output}")

                if result.returncode == 0:
                    self.completed.emit(True, "\n".join(transcript))
                    return

                if attempt < 4:
                    # Never stage unrelated changes introduced by hooks.
                    changed_selected_paths = [
                        change.path for change in self.repository.changes()
                        if change.unstaged and change.path in self.staged_paths
                    ]
                    if changed_selected_paths:
                        self.progress.emit("Re-staging selected files")
                        self.repository.stage(changed_selected_paths)

            self.completed.emit(False, "\n".join(transcript))
        except (GitError, OSError) as error:
            transcript.append(str(error))
            self.completed.emit(False, "\n".join(transcript))


class CommitWindow(QMainWindow):

    def __init__(self, repository: Repository):
        super().__init__()
        self.repository = repository
        self.worker: CommitWorker | None = None
        self.snapshot = None
        self.file_diffs: tuple[FileDiff, ...] = ()
        self.changes: tuple[Change, ...] = ()
        self.filter_key: tuple[str, str] | None = None

        self.setWindowTitle(f"Commit — {repository.root.name}")
        self.resize(1550, 950)

        self.build_ui()

        self.save_timer = QTimer(self)
        self.save_timer.setSingleShot(True)
        self.save_timer.setInterval(250)
        self.save_timer.timeout.connect(self.save_message)

        if repository.message_path.exists():
            self.message.setText(
                repository.message_path.read_text(encoding="utf-8"))
        self.message.textChanged.connect(self.message_changed)
        self.update_commit_button()

        self.refresh()

        self.refresh_timer = QTimer(self)
        self.refresh_timer.setInterval(3000)
        self.refresh_timer.timeout.connect(self.refresh)
        self.refresh_timer.start()

    def build_ui(self) -> None:
        self.splitter = QSplitter(Qt.Orientation.Horizontal)
        self.setCentralWidget(self.splitter)

        # Changed-file tree.
        files_panel = QWidget()
        files_layout = QVBoxLayout(files_panel)

        files_toolbar = QHBoxLayout()
        refresh_button = QPushButton("Refresh")
        refresh_button.clicked.connect(self.force_refresh)
        all_button = QPushButton("All changes")
        all_button.clicked.connect(self.show_all_changes)
        files_toolbar.addWidget(refresh_button)
        files_toolbar.addWidget(all_button)
        files_layout.addLayout(files_toolbar)

        self.tree = QTreeWidget()
        self.tree.setHeaderLabels(["Files", "Status"])
        self.tree.setSelectionMode(
            QAbstractItemView.SelectionMode.ExtendedSelection)
        self.tree.currentItemChanged.connect(self.selected_file_changed)
        files_layout.addWidget(self.tree)

        staging_buttons = QHBoxLayout()
        self.stage_button = QPushButton("Stage selected")
        self.stage_button.clicked.connect(self.stage_selected)
        self.unstage_button = QPushButton("Unstage selected")
        self.unstage_button.clicked.connect(self.unstage_selected)
        staging_buttons.addWidget(self.stage_button)
        staging_buttons.addWidget(self.unstage_button)
        files_layout.addLayout(staging_buttons)

        self.splitter.addWidget(files_panel)

        # Read-only, individually referenceable diff hunks.
        hunks_panel = QWidget()
        hunks_layout = QVBoxLayout(hunks_panel)
        self.hunks_title = QLabel("All changes")
        hunks_layout.addWidget(self.hunks_title)

        self.hunks_scroll = QScrollArea()
        self.hunks_scroll.setWidgetResizable(True)
        hunks_layout.addWidget(self.hunks_scroll)
        self.splitter.addWidget(hunks_panel)

        # Markdown commit message.
        message_panel = QWidget()
        message_layout = QVBoxLayout(message_panel)
        message_layout.addWidget(QLabel("Commit message"))

        self.message = QsciScintilla()
        configure_editor(self.message)

        markdown_lexer = QsciLexerMarkdown(self.message)
        markdown_lexer.setDefaultFont(self.message.font())
        self.message.setLexer(markdown_lexer)
        self.message.setWrapMode(QsciScintilla.WrapMode.WrapWord)
        message_layout.addWidget(self.message)

        self.commit_button = QPushButton("Commit")
        self.commit_button.clicked.connect(self.commit)
        message_layout.addWidget(self.commit_button)

        self.splitter.addWidget(message_panel)
        self.splitter.setSizes([300, 650, 600])

    def message_changed(self) -> None:
        self.save_timer.start()
        self.update_commit_button()

    def save_message(self) -> None:
        self.repository.save_message(self.message.text())

    def update_commit_button(self) -> None:
        self.commit_button.setEnabled(self.worker is None
                                      and bool(self.message.text().strip()))

    def force_refresh(self) -> None:
        self.snapshot = None
        self.refresh()

    def refresh(self) -> None:
        if self.worker is not None:
            return

        changes = self.repository.changes()
        diffs = []

        for change in changes:
            if change.staged:
                diffs.append(
                    FileDiff(
                        "Staged",
                        change.path,
                        self.repository.diff(change, staged=True),
                    ))
            if change.unstaged:
                diffs.append(
                    FileDiff(
                        "Unstaged",
                        change.path,
                        self.repository.diff(change, staged=False),
                    ))

        file_diffs = tuple(diffs)
        snapshot = changes, file_diffs
        if snapshot == self.snapshot:
            return

        self.snapshot = snapshot
        self.changes = changes
        self.file_diffs = file_diffs

        if self.filter_key not in {item.key for item in file_diffs}:
            self.filter_key = None

        self.rebuild_tree()
        self.render_hunks()

    def rebuild_tree(self) -> None:
        self.tree.blockSignals(True)
        self.tree.clear()

        for section in ("Staged", "Unstaged"):
            matching = [
                change for change in self.changes
                if (change.staged if section == "Staged" else change.unstaged)
            ]
            root = QTreeWidgetItem(self.tree, [f"{section} ({len(matching)})"])
            directories: dict[tuple[str, ...], QTreeWidgetItem] = {(): root}

            for change in matching:
                parts = change.path.split("/")
                parent = root

                for depth, part in enumerate(parts[:-1], start=1):
                    key = tuple(parts[:depth])
                    if key not in directories:
                        directories[key] = QTreeWidgetItem(parent, [part])
                    parent = directories[key]

                status = (change.index_status
                          if section == "Staged" else change.worktree_status)
                item = QTreeWidgetItem(parent, [parts[-1], status])
                key = section, change.path
                item.setData(0, Qt.ItemDataRole.UserRole, key)
                item.setToolTip(0, change.path)

                if key == self.filter_key:
                    self.tree.setCurrentItem(item)

        self.tree.expandAll()
        self.tree.resizeColumnToContents(0)
        self.tree.blockSignals(False)

    def selected_file_changed(
        self,
        current: QTreeWidgetItem | None,
        previous: QTreeWidgetItem | None,
    ) -> None:
        self.filter_key = (current.data(0, Qt.ItemDataRole.UserRole)
                           if current is not None else None)
        self.render_hunks()

    def show_all_changes(self) -> None:
        self.tree.clearSelection()
        self.filter_key = None
        self.render_hunks()

    def selected_paths(self, section: str) -> list[str]:
        paths = set()

        def collect(item: QTreeWidgetItem) -> None:
            data = item.data(0, Qt.ItemDataRole.UserRole)
            if data is not None:
                item_section, path = data
                if item_section == section:
                    paths.add(path)
                return

            for index in range(item.childCount()):
                collect(item.child(index))

        for item in self.tree.selectedItems():
            collect(item)

        return sorted(paths)

    def stage_selected(self) -> None:
        self.change_staging(staged=True)

    def unstage_selected(self) -> None:
        self.change_staging(staged=False)

    def change_staging(self, staged: bool) -> None:
        section = "Unstaged" if staged else "Staged"
        paths = self.selected_paths(section)
        if not paths:
            return

        try:
            if staged:
                self.repository.stage(paths)
            else:
                self.repository.unstage(paths)
        except GitError as error:
            QMessageBox.critical(self, "Staging failed", str(error))

        self.refresh()

    def render_hunks(self) -> None:
        old_scroll = self.hunks_scroll.verticalScrollBar().value()
        old_widget = self.hunks_scroll.takeWidget()
        if old_widget is not None:
            old_widget.deleteLater()

        content = QWidget()
        layout = QVBoxLayout(content)

        visible = [
            item for item in self.file_diffs
            if self.filter_key is None or item.key == self.filter_key
        ]

        if self.filter_key is None:
            self.hunks_title.setText("All changes")
        else:
            section, path = self.filter_key
            self.hunks_title.setText(f"{section}: {path}")

        if not visible:
            layout.addWidget(QLabel("No changes."))

        for file_diff in visible:
            heading = QLabel(f"{file_diff.section} — {file_diff.path}")
            heading.setWordWrap(True)
            heading.setTextInteractionFlags(
                Qt.TextInteractionFlag.TextSelectableByMouse)
            layout.addWidget(heading)

            hunks = extract_hunks(file_diff.text)
            if not hunks:
                note = QLabel("No text hunks: binary, empty, metadata-only, "
                              "submodule, or unresolved combined diff.")
                note.setWordWrap(True)
                layout.addWidget(note)
                continue

            for hunk in hunks:
                toolbar = QHBoxLayout()
                header = QLabel(hunk.splitlines()[0])
                header.setWordWrap(True)
                toolbar.addWidget(header, 1)

                reference_button = QPushButton("Reference hunk")
                reference_button.clicked.connect(
                    lambda checked=False, path=file_diff.path, text=hunk:
                    (self.reference_hunk(path, text)))
                toolbar.addWidget(reference_button)
                layout.addLayout(toolbar)

                editor = QsciScintilla()
                configure_editor(editor)

                lexer = QsciLexerDiff(editor)
                lexer.setDefaultFont(editor.font())
                editor.setLexer(lexer)
                editor.setText(hunk)
                editor.setReadOnly(True)
                editor.setWrapMode(QsciScintilla.WrapMode.WrapNone)

                line_height = editor.fontMetrics().height()
                height = min(
                    460,
                    max(90, (len(hunk.splitlines()) + 2) * line_height + 24),
                )
                editor.setFixedHeight(height)
                layout.addWidget(editor)

            layout.addSpacing(18)

        layout.addStretch()
        self.hunks_scroll.setWidget(content)
        self.hunks_scroll.verticalScrollBar().setValue(old_scroll)

    def reference_hunk(self, path: str, hunk: str) -> None:
        # Append rather than replacing any selected message text.
        existing = self.message.text()
        separator = ""
        if existing:
            separator = "\n" * max(
                0, 2 - (len(existing) - len(existing.rstrip("\n"))))

        block = separator + markdown_reference(path, hunk)
        self.message.beginUndoAction()
        self.message.append(block)
        self.message.endUndoAction()

        last_line = self.message.lines() - 1
        self.message.setCursorPosition(
            last_line, len(self.message.text(last_line).rstrip("\r\n")))
        self.message.ensureLineVisible(last_line)
        self.message.setFocus()

    def commit(self) -> None:
        self.refresh()

        staged_paths = {
            change.path
            for change in self.changes if change.staged
        }
        if not staged_paths:
            QMessageBox.information(self, "Nothing staged",
                                    "Stage at least one file first.")
            return

        # Flush the exact message before starting the background commit.
        self.save_timer.stop()
        self.save_message()
        self.refresh_timer.stop()

        self.worker = CommitWorker(self.repository, staged_paths, self)
        self.worker.progress.connect(self.statusBar().showMessage)
        self.worker.completed.connect(self.commit_completed)
        self.worker.finished.connect(self.worker_finished)

        self.splitter.setEnabled(False)
        self.worker.start()

    def commit_completed(self, success: bool, output: str) -> None:
        if success:
            self.message.clear()
            self.save_timer.stop()
            self.save_message()
            self.statusBar().showMessage("Commit created.", 10000)
        else:
            self.statusBar().showMessage("Commit failed.")
            dialog = QMessageBox(self)
            dialog.setIcon(QMessageBox.Icon.Critical)
            dialog.setWindowTitle("Commit failed")
            dialog.setText(
                "The commit failed. The commit message has been preserved.")
            dialog.setInformativeText(
                "Git and pre-commit output is available below.")
            dialog.setDetailedText(output)
            dialog.exec()

    def worker_finished(self) -> None:
        worker = self.worker
        self.worker = None
        if worker is not None:
            worker.deleteLater()

        self.splitter.setEnabled(True)
        self.update_commit_button()
        self.snapshot = None
        self.refresh()
        self.refresh_timer.start()

    def closeEvent(self, event) -> None:
        if self.worker is not None:
            QMessageBox.information(
                self,
                "Commit running",
                "Wait for the commit and its hooks to finish before closing.",
            )
            event.ignore()
            return

        self.save_timer.stop()
        self.save_message()
        event.accept()


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Write Markdown commit messages with selected diff hunks.")
    parser.add_argument(
        "repository",
        type=Path,
        help="Repository path, or a directory inside it.",
    )
    arguments = parser.parse_args()

    repository = Repository(arguments.repository.resolve())
    application = QApplication([sys.argv[0]])
    window = CommitWindow(repository)
    window.show()
    return application.exec()


if __name__ == "__main__":
    raise SystemExit(main())
