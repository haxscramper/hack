from beartype import beartype
from PyQt6.QtCore import QModelIndex, Qt, QTimer
from PyQt6.QtGui import (
    QAction,
    QCloseEvent,
)
from PyQt6.QtWidgets import (
    QButtonGroup,
    QHBoxLayout,
    QMainWindow,
    QPushButton,
    QSplitter,
    QTreeView,
    QVBoxLayout,
    QWidget,
)

from local_review.annotation_store import AnnotationStore
from local_review.diffview import DiffView
from local_review.file_tree_model import FileTreeModel
from local_review.models import CommentPlacement, DiffFile, DiffPresentation
from local_review.review_repository import GitReviewRepository


@beartype
class ReviewWindow(QMainWindow):

    def __init__(
        self,
        repository: GitReviewRepository,
        store: AnnotationStore,
    ) -> None:
        super().__init__()
        self.repository = repository
        self.store = store
        self.files: list[DiffFile] = []
        self.files_by_path: dict[str, DiffFile] = {}

        self.setWindowTitle(f"Local Review — {repository.worktree_path}")
        self.resize(1500, 900)

        central = QWidget(self)
        central_layout = QVBoxLayout(central)
        central_layout.setContentsMargins(0, 0, 0, 0)
        central_layout.setSpacing(0)

        controls = QHBoxLayout()
        controls.setContentsMargins(8, 6, 8, 6)

        presentation_group = QButtonGroup(self)
        presentation_group.setExclusive(True)

        unified_button = QPushButton("Unified", central)
        unified_button.setCheckable(True)
        unified_button.setChecked(True)
        unified_button.clicked.connect(self.unified_clicked)
        presentation_group.addButton(unified_button)
        controls.addWidget(unified_button)

        split_button = QPushButton("Split", central)
        split_button.setCheckable(True)
        split_button.clicked.connect(self.split_clicked)
        presentation_group.addButton(split_button)
        controls.addWidget(split_button)

        placement_group = QButtonGroup(self)
        placement_group.setExclusive(True)

        inline_button = QPushButton("Inline comments", central)
        inline_button.setCheckable(True)
        inline_button.setChecked(True)
        inline_button.clicked.connect(self.inline_clicked)
        placement_group.addButton(inline_button)
        controls.addWidget(inline_button)

        margin_button = QPushButton("Margin comments", central)
        margin_button.setCheckable(True)
        margin_button.clicked.connect(self.margin_clicked)
        placement_group.addButton(margin_button)
        controls.addWidget(margin_button)

        add_comment_button = QPushButton("Add comment", central)
        add_comment_button.clicked.connect(self.add_comment_clicked)
        controls.addWidget(add_comment_button)
        controls.addStretch(1)

        central_layout.addLayout(controls)

        splitter = QSplitter(Qt.Orientation.Horizontal, central)
        self.tree_model = FileTreeModel()
        self.tree_view = QTreeView(splitter)
        self.tree_view.setModel(self.tree_model)
        self.tree_view.setAlternatingRowColors(True)
        self.tree_view.setUniformRowHeights(True)
        self.tree_view.setColumnWidth(0, 280)
        self.tree_view.setColumnWidth(1, 55)
        self.tree_view.setColumnWidth(2, 55)
        self.tree_view.currentChanged = self.tree_view.currentChanged

        self.diff_view = DiffView(
            repository,
            store,
            self.refresh_annotation_counts,
            splitter,
        )
        splitter.addWidget(self.tree_view)
        splitter.addWidget(self.diff_view)
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 1)
        splitter.setSizes([360, 1140])
        central_layout.addWidget(splitter)

        self.setCentralWidget(central)
        self.reload_files()

        selection_model = self.tree_view.selectionModel()
        selection_model.currentChanged.connect(self.file_selection_changed)

        self.head_timer = QTimer(self)
        self.head_timer.setInterval(1500)
        self.head_timer.timeout.connect(self.check_head)
        self.head_timer.start()

        add_action = QAction(self)
        add_action.setShortcut("Ctrl+Enter")
        add_action.triggered.connect(self.add_comment_action)
        self.addAction(add_action)

    def reload_files(self) -> None:
        selected_path = (self.diff_view.diff_file.display_path
                         if self.diff_view.diff_file is not None else None)
        self.files = self.repository.load_diff()
        self.files_by_path = {
            diff_file.display_path: diff_file
            for diff_file in self.files
        }
        self.tree_model.replace_files(self.files, self.store.document.threads)
        self.tree_view.expandAll()

        if selected_path is not None:
            diff_file = self.files_by_path.get(selected_path)
            self.diff_view.set_diff_file(diff_file)

    def file_selection_changed(
        self,
        current: QModelIndex,
        previous: QModelIndex,
    ) -> None:
        path = current.siblingAtColumn(0).data(FileTreeModel.PATH_ROLE)
        if path is None:
            return

        diff_file = self.files_by_path.get(path)
        if diff_file is None:
            raise ValueError(f"Selected diff path {path!r} is not loaded")
        self.diff_view.set_diff_file(diff_file)

    def unified_clicked(self, checked: bool) -> None:
        if checked:
            self.diff_view.set_presentation(DiffPresentation.UNIFIED)

    def split_clicked(self, checked: bool) -> None:
        if checked:
            self.diff_view.set_presentation(DiffPresentation.SPLIT)

    def inline_clicked(self, checked: bool) -> None:
        if checked:
            self.diff_view.set_placement(CommentPlacement.INLINE)

    def margin_clicked(self, checked: bool) -> None:
        if checked:
            self.diff_view.set_placement(CommentPlacement.MARGIN)

    def add_comment_clicked(self, checked: bool) -> None:
        self.diff_view.add_comment_for_selection()

    def add_comment_action(self, checked: bool) -> None:
        self.diff_view.add_comment_for_selection()

    def check_head(self) -> None:
        if not self.repository.refresh_head():
            return

        if self.repository.reconcile_pending(self.store.document):
            self.store.save()

        self.reload_files()

    def closeEvent(self, event: QCloseEvent) -> None:
        self.store.save()
        event.accept()

    def refresh_annotation_counts(self) -> None:
        self.tree_model.update_annotation_counts(self.store.document.threads)
