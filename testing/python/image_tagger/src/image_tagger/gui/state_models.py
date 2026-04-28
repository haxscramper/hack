from __future__ import annotations

from pydantic import BaseModel


class WeightedTagEntry(BaseModel):
    tag_name: str
    weight: float


class MixedViewState(BaseModel):
    expanded_paths: list[str]
    zoom_factor: float
    scroll_y: int
    selected_files: list[str]
    sort_mode: str = "NAME_ASC"
    similarity_reference_path: str | None = None
    weighted_tag_entries: list[WeightedTagEntry] = []


class SearchTabState(BaseModel):
    sexp_query: str
    thumb_size: int
    scroll_y: int
    splitter_sizes: list[int]


class DirectorySelectorState(BaseModel):
    current_dir: str


class DirectoryPreviewState(BaseModel):
    selector: DirectorySelectorState


class RightPanelState(BaseModel):
    preview_widgets: list[DirectoryPreviewState]
    splitter_sizes: list[int]


class LeftPanelState(BaseModel):
    active_tab: int
    mixed_view: MixedViewState
    search_tab: SearchTabState
    weighted_tag_entries: list[WeightedTagEntry] = []


class CenterPanelState(BaseModel):
    splitter_sizes: list[int]


class PaletteState(BaseModel):
    pinned_paths: list[str] = []
    recent_paths: list[str] = []


class AppState(BaseModel):
    window_size: tuple[int, int]
    splitter_sizes: list[int]
    left_panel: LeftPanelState
    center_panel: CenterPanelState
    right_panel: RightPanelState
    palette: PaletteState = PaletteState()
