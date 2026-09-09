from collections.abc import Callable

from beartype import beartype

from PyQt6.QtWidgets import (
    QFrame,
    QHBoxLayout,
    QPlainTextEdit,
    QPushButton,
    QSizePolicy,
    QVBoxLayout,
    QWidget,
)

from local_review.models import AnnotationThread


@beartype
class ThreadWidget(QFrame):

    def __init__(
        self,
        thread: AnnotationThread,
        update_callback: Callable[[str, str, str], None],
        reply_callback: Callable[[str], None],
        remove_callback: Callable[[str], None],
        layout_callback: Callable[[], None],
        parent: QWidget,
    ) -> None:
        super().__init__(parent)
        self.thread = thread
        self.update_callback = update_callback
        self.reply_callback = reply_callback
        self.remove_callback = remove_callback
        self.layout_callback = layout_callback

        self.setFrameShape(QFrame.Shape.StyledPanel)
        self.setAutoFillBackground(True)
        self.setSizePolicy(
            QSizePolicy.Policy.Fixed,
            QSizePolicy.Policy.Fixed,
        )

        layout = QVBoxLayout(self)
        layout.setContentsMargins(8, 8, 8, 8)
        layout.setSpacing(6)

        for comment in thread.comments:
            editor = QPlainTextEdit(self)
            editor.setPlaceholderText("Write a review comment")
            editor.setPlainText(comment.text)
            editor.setFixedHeight(self.editor_height(comment.text))
            editor.textChanged.connect(
                lambda editor=editor, comment_id=comment.id: self.
                editor_changed(editor, comment_id))
            layout.addWidget(editor)

        action_layout = QHBoxLayout()
        action_layout.addStretch(1)

        reply_button = QPushButton("Reply", self)
        reply_button.clicked.connect(self.reply_clicked)
        action_layout.addWidget(reply_button)

        remove_button = QPushButton("Delete thread", self)
        remove_button.clicked.connect(self.remove_clicked)
        action_layout.addWidget(remove_button)

        layout.addLayout(action_layout)

    def editor_height(self, text: str) -> int:
        line_count = max(1, len(text.splitlines()))
        return max(72, min(220, 34 + line_count * 20))

    def preferred_height(self) -> int:
        editor_height = sum(
            self.editor_height(comment.text)
            for comment in self.thread.comments)
        spacing = 6 * len(self.thread.comments)
        return 54 + editor_height + spacing

    def editor_changed(
        self,
        editor: QPlainTextEdit,
        comment_id: str,
    ) -> None:
        text = editor.toPlainText()
        editor.setFixedHeight(self.editor_height(text))
        self.update_callback(
            self.thread.id,
            comment_id,
            text,
        )
        self.setFixedHeight(self.preferred_height())
        self.layout_callback()

    def reply_clicked(self, checked: bool) -> None:
        self.reply_callback(self.thread.id)

    def remove_clicked(self, checked: bool) -> None:
        self.remove_callback(self.thread.id)
