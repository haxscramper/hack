from beartype import beartype
from pathlib import Path
from PySide6.QtWidgets import QWidget, QVBoxLayout, QTabWidget
from PySide6.QtCore import Signal
from image_tagger.db.sorting import SortMode
from image_tagger.gui.image_directory_view import MixedTreeTileView
from image_tagger.gui.query_search import SearchTab
from image_tagger.gui.state_models import LeftPanelState


class LeftPanel(QWidget):
    fileSelected = Signal(object)

    def __init__(self, root_dir, session, parent=None):
        super().__init__(parent)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        self.tabs = QTabWidget()

        self.tree_view = MixedTreeTileView(root_dir, session=session)
        self.search_view = SearchTab(session, str(root_dir))

        self.tabs.addTab(self.tree_view, "Files")
        self.tabs.addTab(self.search_view, "Search")

        layout.addWidget(self.tabs)

        # Connect signals
        self.tree_view.fileSelected.connect(self.fileSelected.emit)
        self.search_view.thumbnail_list.list_view.doubleClicked.connect(
            self._on_search_double_clicked)
        self.search_view.thumbnail_list.list_view.clicked.connect(
            self._on_search_clicked)

    def _on_search_clicked(self, index):
        if index.isValid():
            img_path = self.search_view.thumbnail_list.model.images[
                index.row()]
            self.fileSelected.emit(str(img_path))

    def _on_search_double_clicked(self, index):
        if index.isValid():
            img_path = self.search_view.thumbnail_list.model.images[
                index.row()]
            self.fileSelected.emit(str(img_path))

    @property
    def root_node(self):
        return self.tree_view.root_node

    @property
    def selected_files(self) -> set[Path]:
        # Return selected files based on active tab
        if self.tabs.currentIndex() == 1:  # Search tab
            # Return files selected in the search results thumbnail list
            return set(self.search_view.thumbnail_list.get_selected_images())
        else:  # Files tab
            return self.tree_view.selected_files

    @selected_files.setter
    def selected_files(self, val: set[Path]):
        # Set selected files based on active tab
        if self.tabs.currentIndex() == 1:  # Search tab
            # Set selection in the search results thumbnail list
            self.search_view.thumbnail_list.set_selection(val)
        else:  # Files tab
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

    def get_state(self) -> LeftPanelState:
        from image_tagger.gui.state_models import LeftPanelState

        return LeftPanelState(
            active_tab=self.tabs.currentIndex(),
            mixed_view=self.tree_view.get_state(),
            search_tab=self.search_view.get_state(),
        )

    def set_state(self, state: LeftPanelState) -> None:
        self.tabs.setCurrentIndex(state.active_tab)
        self.tree_view.set_state(state.mixed_view)
        self.search_view.set_state(state.search_tab)
