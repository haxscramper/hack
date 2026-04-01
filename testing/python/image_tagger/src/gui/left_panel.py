from PySide6.QtWidgets import QWidget, QVBoxLayout, QTabWidget
from gui.image_directory_view import MixedTreeTileView
from gui.query_search import SearchTab


class LeftPanel(QWidget):
    def __init__(self, root_dir, session, parent=None):
        super().__init__(parent)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        self.tabs = QTabWidget()

        self.tree_view = MixedTreeTileView(root_dir)
        self.search_view = SearchTab(session, str(root_dir))

        self.tabs.addTab(self.tree_view, "Files")
        self.tabs.addTab(self.search_view, "Search")

        layout.addWidget(self.tabs)

        # Expose important attributes/signals to maintain API compatibility
        self.fileSelected = self.tree_view.fileSelected

    @property
    def root_node(self):
        return self.tree_view.root_node

    @property
    def selected_files(self):
        return self.tree_view.selected_files

    @selected_files.setter
    def selected_files(self, val):
        self.tree_view.selected_files = val

    @property
    def fully_annotated_files(self):
        return self.tree_view.fully_annotated_files

    @fully_annotated_files.setter
    def fully_annotated_files(self, val):
        self.tree_view.fully_annotated_files = val

    def _update_scrollbars(self):
        self.tree_view._update_scrollbars()

    def viewport(self):
        return self.tree_view.viewport()
