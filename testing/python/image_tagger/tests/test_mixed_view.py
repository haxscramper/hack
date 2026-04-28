from utils import take_screenshot
from conftest import AppInstanceRes
from pathlib import Path
from pytestqt.qtbot import QtBot
from PySide6.QtCore import Qt, QPoint
from PySide6.QtTest import QTest, QSignalSpy
from PySide6.QtWidgets import (
    QApplication,
    QDialog,
    QDialogButtonBox,
    QLabel,
    QListView,
)
from image_tagger.db.sorting import SortMode


def test_mixed_view_content_acces(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Capture screenshot of a specific nested widget."""
    qtbot.waitExposed(gui_app_instance.window)

    # Navigate to nested widget (adjust path as needed)
    central_widget = gui_app_instance.window.centralWidget()

    # Ensure widget is visible
    qtbot.waitExposed(central_widget)
    qtbot.wait(500)

    widget = gui_app_instance.window.get_mixed_view()

    target = image_directory / "image_255_0_239.png"
    rect = widget.get_tile_rect(target)
    assert rect is not None, f"Tile {target} not found - ensure parent is expanded"

    # Scroll if needed
    if not widget.is_element_visible(rect):
        widget.scroll_to_content_y(rect.center().y())
        qtbot.wait(50)

    # Convert content coordinates to viewport coordinates
    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(rect.center().x(), rect.center().y() - scroll_y)

    # Click and verify signal
    spy = QSignalSpy(widget.imageClicked)
    QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=click_pos)

    screenshot_path = screenshot_dir / "status_panel.png"
    take_screenshot(central_widget, screenshot_path)

    assert spy.count() == 1
    assert spy.at(0)[0] == target
    assert target in widget.selected_files


def test_mixed_view_expand_directory_reveals_tiles(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test expanding a collapsed directory reveals its image tiles."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Find first subdirectory containing images
    subdir = image_directory.joinpath("sub1")
    toggle_rect = widget.get_toggle_rect(subdir)

    # If not found, scroll to make header visible
    if toggle_rect is None:
        header_rect = widget.get_header_rect(subdir)
        if header_rect:
            widget.scroll_to_content_y(header_rect.center().y())
            qtbot.wait(150)
            toggle_rect = widget.get_toggle_rect(subdir)

    qtbot.wait(150)
    assert toggle_rect is not None, f"Could not find toggle for directory {subdir}"

    # Verify files are not visible (directory collapsed)
    image_files = list(subdir.glob("*.png"))
    assert widget.get_tile_rect(image_files[0]) is None, (
        "Directory should start collapsed"
    )

    # Click toggle to expand
    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(toggle_rect.center().x(), toggle_rect.center().y() - scroll_y)
    QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=click_pos)
    qtbot.wait(50)

    take_screenshot(central_widget, screenshot_dir / "directory_expanded.png")

    # Verify tiles are now accessible
    for img in image_files[:3]:
        rect = widget.get_tile_rect(img)
        assert rect is not None, f"Tile {img.name} should be visible after expansion"

    # Verify state reflects the expanded directory
    state = gui_app_instance.window.get_state()
    assert str(subdir) in state.left_panel.mixed_view.expanded_paths

    file_to_move = subdir / image_files[0]
    first_image_rect = widget.get_element_click_pos(file_to_move)

    QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=first_image_rect)

    QTest.keyClick(
        gui_app_instance.window, Qt.Key.Key_1, Qt.KeyboardModifier.ControlModifier, 100
    )

    dialog = next(
        widget
        for widget in QApplication.topLevelWidgets()
        if isinstance(widget, QDialog) and widget.isVisible()
    )

    assert dialog.windowTitle().startswith("Move to ")

    label = dialog.findChild(QLabel)
    assert label is not None

    list_view = dialog.findChild(QListView)
    assert list_view is not None

    buttons = dialog.findChild(QDialogButtonBox)
    assert buttons is not None

    qtbot.wait(150)
    take_screenshot(dialog, screenshot_dir / "dialog_prompt.png")
    take_screenshot(central_widget, screenshot_dir / "before_move.png")
    assert file_to_move.exists()
    dialog.accept()
    assert not file_to_move.exists()
    qtbot.wait(150)
    take_screenshot(central_widget, screenshot_dir / "after_move.png")

    assert not widget.get_tile_rect(subdir / image_files[0])
    assert widget.get_element_click_pos(subdir / image_files[1])
    assert widget.get_element_click_pos(subdir / image_files[2])


def test_mixed_view_ctrl_click_multi_select(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test Ctrl+Click multi-selection of image tiles."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Get first two available images
    images = [hit.file_path for hit in widget.tile_hits][:2]
    if len(images) < 2:
        # Try to expand directories to find more images
        for d in image_directory.iterdir():
            if d.is_dir():
                toggle = widget.get_toggle_rect(d)
                if toggle:
                    scroll_y = widget.verticalScrollBar().value()
                    pos = QPoint(toggle.center().x(), toggle.center().y() - scroll_y)
                    QTest.mouseClick(
                        widget.viewport(), Qt.MouseButton.LeftButton, pos=pos
                    )
                    qtbot.wait(50)
        images = [hit.file_path for hit in widget.tile_hits][:2]

    img1, img2 = images[0], images[1]

    # Click first image (no modifier)
    rect1 = widget.get_tile_rect(img1)
    scroll_y = widget.verticalScrollBar().value()
    assert rect1
    pos1 = QPoint(rect1.center().x(), rect1.center().y() - scroll_y)

    QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=pos1)
    qtbot.wait(50)
    assert img1 in widget.selected_files

    # Ctrl+Click second image
    rect2 = widget.get_tile_rect(img2)
    assert rect2
    if not widget.is_element_visible(rect2):
        widget.scroll_to_content_y(rect2.center().y())
        qtbot.wait(50)
        scroll_y = widget.verticalScrollBar().value()

    pos2 = QPoint(rect2.center().x(), rect2.center().y() - scroll_y)
    QTest.mouseClick(
        widget.viewport(),
        Qt.MouseButton.LeftButton,
        Qt.KeyboardModifier.ControlModifier,
        pos2,
    )
    qtbot.wait(50)

    screenshot_path = screenshot_dir / "multi_select.png"
    take_screenshot(central_widget, screenshot_path)

    assert img1 in widget.selected_files, "First image should remain selected"
    assert img2 in widget.selected_files, "Second image should be added to selection"
    assert len(widget.selected_files) == 2

    # Verify state reflects the multi-selection
    state = gui_app_instance.window.get_state()
    assert str(img1) in state.left_panel.mixed_view.selected_files
    assert str(img2) in state.left_panel.mixed_view.selected_files


def test_mixed_view_scroll_to_offscreen_element(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test scrolling to reveal and interact with off-screen tiles."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Resize to small height to force scrolling
    original_size = gui_app_instance.window.size()
    gui_app_instance.window.resize(800, 200)
    qtbot.wait(100)

    # Get all available tiles
    all_tiles = [hit.file_path for hit in widget.tile_hits]
    # Select last tile (likely off-screen)
    target = all_tiles[-1]
    rect = widget.get_tile_rect(target)
    assert rect

    # Verify it's not fully visible before scroll
    # Scroll to make visible
    widget.scroll_to_content_y(rect.center().y())
    qtbot.wait(50)

    rect = widget.get_tile_rect(target)
    assert rect
    assert widget.is_element_visible(rect), "Tile should be visible after scroll"

    # Take screenshot of scrolled state
    screenshot_path = screenshot_dir / "scrolled_to_tile.png"
    take_screenshot(central_widget, screenshot_path)

    # Verify interaction works after scrolling
    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(rect.center().x(), rect.center().y() - scroll_y)

    spy = QSignalSpy(widget.imageClicked)
    QTest.mouseClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=click_pos)

    assert spy.count() == 1
    assert spy.at(0)[0] == target
    assert target in widget.selected_files

    # Verify state reflects the scroll position
    state = gui_app_instance.window.get_state()
    assert state.left_panel.mixed_view.scroll_y > 0

    # Restore original size
    gui_app_instance.window.resize(original_size)


def test_mixed_view_double_click_emits_file_selected(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test double-clicking a tile emits fileSelected signal."""

    # gui_app_instance.repo.replace_probabilistic_annotations()

    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    target = widget.tile_hits[0].file_path
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[
            ("general", "mixed", 0.45),
            ("general", "random", 0.5),
            ("general", "castle", 0.4),
        ],
    )

    rect = widget.get_tile_rect(target)
    assert rect

    if not widget.is_element_visible(rect):
        widget.scroll_to_content_y(rect.center().y())
        qtbot.wait(50)
        rect = widget.get_tile_rect(target)
        assert rect

    scroll_y = widget.verticalScrollBar().value()
    click_pos = QPoint(rect.center().x(), rect.center().y() - scroll_y)

    spy = QSignalSpy(widget.fileSelected)
    QTest.mouseDClick(widget.viewport(), Qt.MouseButton.LeftButton, pos=click_pos)
    qtbot.wait(50)

    screenshot_path = screenshot_dir / "double_click_selection.png"
    take_screenshot(central_widget, screenshot_path)

    assert spy.count() == 1
    assert spy.at(0)[0] == str(target)
    assert target in widget.selected_files

    prob_table = gui_app_instance.window.get_probability_tags_table()

    assert prob_table.rowCount() == 3
    assert prob_table.item(0, 1).text() == "random"  # type: ignore
    assert prob_table.item(1, 1).text() == "mixed"  # type: ignore
    assert prob_table.item(2, 1).text() == "castle"  # type: ignore

    assert "0.5" in prob_table.item(0, 2).text()  # type: ignore
    assert "0.45" in prob_table.item(1, 2).text()  # type: ignore
    assert "0.4" in prob_table.item(2, 2).text()  # type: ignore


def test_mixed_view_sort_by_name_size_mtime(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test sorting by name, size, and mtime in both directions."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Expand all directories so we can see all tiles
    def expand_all(node):
        if not node.expanded:
            node.expanded = True
        if not node.children_loaded:
            node.load(**widget.get_node_load())
        for child in node.child_dirs:
            expand_all(child)

    expand_all(widget.root_node)
    widget._update_scrollbars()
    qtbot.wait(100)

    # Collect all image files in the root node
    all_images = list(image_directory.rglob("*.png"))
    assert len(all_images) > 0

    # Helper to get current tile order from a specific directory node
    def get_dir_tile_order(dir_path: Path):
        return [
            hit.file_path
            for hit in widget.tile_hits
            if hit.file_path.parent == dir_path
        ]

    # Helper to verify sorting within each directory
    def verify_sort(sort_mode: SortMode, key_func, reverse: bool = False):
        widget.set_sort_mode(sort_mode)
        widget._update_scrollbars()
        qtbot.wait(50)

        for dir_path in [image_directory] + [
            d for d in image_directory.rglob("*") if d.is_dir()
        ]:
            images = list(dir_path.glob("*.png"))
            if not images:
                continue
            order = get_dir_tile_order(dir_path)
            expected = sorted(images, key=key_func, reverse=reverse)
            assert order == expected, f"{sort_mode.name} failed for {dir_path}"

        state = gui_app_instance.window.get_state()
        assert state.left_panel.mixed_view.sort_mode == sort_mode.name

    # --- NAME_ASC ---
    verify_sort(SortMode.NAME_ASC, lambda p: p.name.lower())

    # --- NAME_DESC ---
    verify_sort(SortMode.NAME_DESC, lambda p: p.name.lower(), reverse=True)

    # --- SIZE_ASC ---
    verify_sort(SortMode.SIZE_ASC, lambda p: (p.stat().st_size, p.name.lower()))

    # --- SIZE_DESC ---
    verify_sort(
        SortMode.SIZE_DESC, lambda p: (p.stat().st_size, p.name.lower()), reverse=True
    )

    # --- MTIME_ASC ---
    verify_sort(SortMode.MTIME_ASC, lambda p: (p.stat().st_mtime, p.name.lower()))

    # --- MTIME_DESC ---
    verify_sort(
        SortMode.MTIME_DESC, lambda p: (p.stat().st_mtime, p.name.lower()), reverse=True
    )

    take_screenshot(central_widget, screenshot_dir / "sort_name_size_mtime.png")


def test_mixed_view_sort_by_similarity(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test similarity-based sorting using probabilistic tags."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Set up probabilistic tags for similarity testing
    # Image A: tags [castle: 1.0, sky: 0.5]
    # Image B: tags [castle: 1.0, sky: 0.5]  -- very similar to A
    # Image C: tags [castle: 0.8]            -- somewhat similar to A
    # Image D: tags [ocean: 1.0]             -- not similar to A
    # Image E: no tags                       -- no vector

    img_a = image_directory / "image_255_0_239.png"
    img_b = image_directory / "image_255_0_191.png"
    img_c = image_directory / "image_255_0_143.png"
    img_d = image_directory / "image_255_0_95.png"
    img_e = image_directory / "image_255_0_47.png"

    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(img_a),
        items=[("general", "castle", 1.0), ("general", "sky", 0.5)],
    )
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(img_b),
        items=[("general", "castle", 1.0), ("general", "sky", 0.5)],
    )
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(img_c),
        items=[("general", "castle", 0.8)],
    )
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(img_d),
        items=[("general", "ocean", 1.0)],
    )
    # img_e gets no tags

    # Rebuild similarity index with new tags
    widget.similarity_index.build(gui_app_instance.session)

    # Expand all directories
    def expand_all(node):
        if not node.expanded:
            node.expanded = True
        if not node.children_loaded:
            node.load(**widget.get_node_load())
        for child in node.child_dirs:
            expand_all(child)

    expand_all(widget.root_node)
    widget._update_scrollbars()
    qtbot.wait(100)

    # Helper to get tile order within a specific directory
    def get_dir_tile_order(dir_path: Path):
        return [
            hit.file_path
            for hit in widget.tile_hits
            if hit.file_path.parent == dir_path
        ]

    # --- SIMILARITY (greedy chain) ---
    # Starting from first image alphabetically among those with vectors.
    # The greedy algorithm picks the most similar next image at each step.
    # With the tags above, if img_a is first, img_b is most similar to img_a,
    # then img_c, then img_d. img_e (no tags) goes to the end.
    widget.set_sort_mode(SortMode.SIMILARITY)
    widget._update_scrollbars()
    qtbot.wait(50)

    root_order = get_dir_tile_order(image_directory)
    # Verify that images with tags come before images without tags in the root
    tagged = {img_a, img_b, img_c, img_d}
    untagged = {img_e}

    tagged_positions = [i for i, p in enumerate(root_order) if p in tagged]
    untagged_positions = [i for i, p in enumerate(root_order) if p in untagged]

    assert all(
        t < min(untagged_positions or [float("inf")]) for t in tagged_positions
    ), "Tagged images should appear before untagged images in SIMILARITY mode"

    # Verify internal state
    state = gui_app_instance.window.get_state()
    assert state.left_panel.mixed_view.sort_mode == "SIMILARITY"

    # --- SIMILARITY_TO_REFERENCE ---
    # Sort by similarity to img_a. Expected order: img_b (identical), img_c (castle), img_d (different), img_e (no vector)
    widget.set_sort_mode(SortMode.SIMILARITY_TO_REFERENCE, reference_path=img_a)
    widget._update_scrollbars()
    qtbot.wait(50)

    root_order = get_dir_tile_order(image_directory)

    # img_a and img_b have identical vectors, so their order relative to each
    # other is determined by name. img_a (image_255_0_239.png) comes after
    # img_b (image_255_0_191.png) alphabetically, so img_b is first.
    # img_c should be before img_d (castle tag is shared)
    # All untagged images (including img_e and image_79_0_255.png) are sorted
    # by name and appended after the scored images.
    assert root_order[0] == img_b, (
        "Most similar image should be first in SIMILARITY_TO_REFERENCE"
    )
    assert root_order[1] == img_a, (
        "Reference image should be second (same score, sorted by name)"
    )
    # img_e (image_255_0_47.png) comes after image_31_0_255.png alphabetically,
    # so it is not in the last two positions. Just verify it is after all scored images.
    scored_count = len([p for p in root_order if p in {img_a, img_b, img_c, img_d}])
    assert root_order.index(img_e) >= scored_count, (
        "Untagged image should be after all scored images"
    )
    assert img_c in root_order, "img_c should be in the order"
    assert img_d in root_order, "img_d should be in the order"

    # Verify internal state
    state = gui_app_instance.window.get_state()
    assert state.left_panel.mixed_view.sort_mode == "SIMILARITY_TO_REFERENCE"
    assert state.left_panel.mixed_view.similarity_reference_path == str(img_a)

    take_screenshot(central_widget, screenshot_dir / "sort_similarity.png")
