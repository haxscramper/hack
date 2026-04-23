from utils import take_screenshot
from conftest import AppInstanceRes
from pathlib import Path
from pytestqt.qtbot import QtBot
from PySide6.QtCore import Qt, QPoint
from PySide6.QtTest import QTest
from image_tagger.db.sorting import SortMode


def test_weighted_tag_sort_ui_add_remove_rows(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test adding and removing weighted tag rows in the left panel."""
    qtbot.waitExposed(gui_app_instance.window)

    weighted_sort = gui_app_instance.window.left_panel.weighted_sort

    # Initially no rows
    assert len(weighted_sort.get_entries()) == 0

    # Add a row
    weighted_sort.add_row("castle", 1.5)
    qtbot.wait(50)

    entries = weighted_sort.get_entries()
    assert len(entries) == 1
    assert entries[0] == ("castle", 1.5)

    # Add another row
    weighted_sort.add_row("sky", -0.5)
    qtbot.wait(50)

    entries = weighted_sort.get_entries()
    assert len(entries) == 2
    assert entries[1] == ("sky", -0.5)

    # Remove first row by clicking its remove button
    first_row = weighted_sort._rows[0]
    QTest.mouseClick(first_row.remove_btn, Qt.MouseButton.LeftButton)
    qtbot.wait(50)

    entries = weighted_sort.get_entries()
    assert len(entries) == 1
    assert entries[0] == ("sky", -0.5)

    take_screenshot(weighted_sort, screenshot_dir / "weighted_tag_rows.png")


def test_weighted_tag_sort_rebuilds_index_and_sorts(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that RE-sort rebuilds the similarity index and sorts by weighted tags."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    # Set up tags for testing
    # Image A: castle=1.0, sky=0.5
    # Image B: castle=0.8
    # Image C: sky=1.0
    # Image D: ocean=1.0
    img_a = image_directory / "image_255_0_239.png"
    img_b = image_directory / "image_255_0_191.png"
    img_c = image_directory / "image_255_0_143.png"
    img_d = image_directory / "image_255_0_95.png"

    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(img_a),
        items=[("general", "castle", 1.0), ("general", "sky", 0.5)],
    )
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(img_b),
        items=[("general", "castle", 0.8)],
    )
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(img_c),
        items=[("general", "sky", 1.0)],
    )
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(img_d),
        items=[("general", "ocean", 1.0)],
    )

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

    weighted_sort = gui_app_instance.window.left_panel.weighted_sort

    # Add weighted tag entries: castle=1.0, sky=0.5
    weighted_sort.add_row("castle", 1.0)
    weighted_sort.add_row("sky", 0.5)
    qtbot.wait(50)

    # Click RE-sort
    QTest.mouseClick(weighted_sort.sort_btn, Qt.MouseButton.LeftButton)
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "weighted_tag_sorted.png")

    # Get root tile order
    def get_dir_tile_order(dir_path: Path):
        return [
            hit.file_path for hit in widget.tile_hits
            if hit.file_path.parent == dir_path
        ]

    root_order = get_dir_tile_order(image_directory)

    # img_a has castle=1.0 and sky=0.5 -> highest weighted similarity
    # img_b has castle=0.8 -> second
    # img_c has sky=1.0 -> third
    # img_d has no matching tags -> last among scored
    assert root_order[0] == img_a, "img_a should be first (castle + sky)"
    assert root_order[1] == img_b, "img_b should be second (castle)"
    assert root_order[2] == img_c, "img_c should be third (sky)"

    # Verify state reflects weighted tag entries
    state = gui_app_instance.window.get_state()
    assert len(state.left_panel.weighted_tag_entries) == 2
    assert state.left_panel.weighted_tag_entries[0].tag_name == "castle"
    assert state.left_panel.weighted_tag_entries[0].weight == 1.0
    assert state.left_panel.weighted_tag_entries[1].tag_name == "sky"
    assert state.left_panel.weighted_tag_entries[1].weight == 0.5
    assert state.left_panel.mixed_view.sort_mode == "SIMILARITY_TO_WEIGHTED_TAGS"


def test_weighted_tag_sort_negative_weight(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test sorting with negative weights (dissimilarity)."""
    qtbot.waitExposed(gui_app_instance.window)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    img_a = image_directory / "image_255_0_239.png"
    img_b = image_directory / "image_255_0_191.png"

    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(img_a),
        items=[("general", "castle", 1.0)],
    )
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(img_b),
        items=[("general", "castle", 0.5)],
    )

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

    weighted_sort = gui_app_instance.window.left_panel.weighted_sort
    weighted_sort.add_row("castle", -1.0)
    qtbot.wait(50)

    QTest.mouseClick(weighted_sort.sort_btn, Qt.MouseButton.LeftButton)
    qtbot.wait(100)

    # With negative weight, higher castle score means lower overall score
    # So img_b (castle=0.5) should come before img_a (castle=1.0)
    def get_dir_tile_order(dir_path: Path):
        return [
            hit.file_path for hit in widget.tile_hits
            if hit.file_path.parent == dir_path
        ]

    root_order = get_dir_tile_order(image_directory)
    scored = [p for p in root_order if p in {img_a, img_b}]
    assert scored[0] == img_b, "img_b should be first with negative weight"
    assert scored[1] == img_a, "img_a should be second with negative weight"


def test_weighted_tag_sort_state_persistence(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that weighted tag entries persist through state save/restore."""
    qtbot.waitExposed(gui_app_instance.window)

    weighted_sort = gui_app_instance.window.left_panel.weighted_sort

    weighted_sort.add_row("castle", 2.0)
    weighted_sort.add_row("sky", -1.0)
    qtbot.wait(50)

    state = gui_app_instance.window.get_state()

    # Clear and restore
    weighted_sort.set_entries([])
    assert len(weighted_sort.get_entries()) == 0

    gui_app_instance.window.left_panel.set_state(state.left_panel)
    qtbot.wait(50)

    entries = weighted_sort.get_entries()
    assert len(entries) == 2
    assert entries[0] == ("castle", 2.0)
    assert entries[1] == ("sky", -1.0)


def test_weighted_tag_sort_empty_no_crash(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that clicking RE-sort with no tags does not crash."""
    qtbot.waitExposed(gui_app_instance.window)

    widget = gui_app_instance.window.get_mixed_view()
    qtbot.waitExposed(widget)

    weighted_sort = gui_app_instance.window.left_panel.weighted_sort

    # No rows added - click RE-sort anyway
    QTest.mouseClick(weighted_sort.sort_btn, Qt.MouseButton.LeftButton)
    qtbot.wait(100)

    # Should fall back to name sort
    assert widget.sort_mode == SortMode.SIMILARITY_TO_WEIGHTED_TAGS


def test_weighted_tag_sort_autocomplete_setup(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that tag input has a completer with tag names."""
    qtbot.waitExposed(gui_app_instance.window)

    target = image_directory / "image_255_0_239.png"
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=gui_app_instance.get_image_id(target),
        items=[("general", "castle", 1.0)],
    )

    weighted_sort = gui_app_instance.window.left_panel.weighted_sort
    row = weighted_sort.add_row()
    qtbot.wait(50)

    completer = row.tag_input.completer()
    assert completer is not None
    model = completer.model()
    assert model is not None
    names = [model.index(i, 0).data() for i in range(model.rowCount())]
    assert "castle" in names
