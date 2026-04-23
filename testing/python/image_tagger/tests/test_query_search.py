from utils import take_screenshot
from conftest import AppInstanceRes, setup_search_tab
from pathlib import Path
from pytestqt.qtbot import QtBot
from PySide6.QtTest import QSignalSpy




def test_search_by_probabilistic_tag(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching images by probabilistic tag."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[
            ("general", "castle", 0.8),
            ("general", "random", 0.3),
        ],
    )

    target2 = image_directory / "sub1" / "image_255_0_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[
            ("general", "castle", 0.6),
        ],
    )

    target3 = image_directory / "sub2" / "sub23" / "image_0_255_255.png"
    image_id3 = gui_app_instance.get_image_id(target3)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id3,
        items=[
            ("general", "castle", 0.4),
        ],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "probabilistic_tag",
        "category": "general",
        "name": "castle",
        "min_probability": 0.5,
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_prob_tag.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert str(target3) not in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()

    # Verify state reflects the search tab configuration
    state = gui_app_instance.window.get_state()
    assert state.left_panel.active_tab == 1
    assert "castle" in state.left_panel.search_tab.sexp_query
    assert state.left_panel.search_tab.thumb_size == 100


def test_search_by_regular_tag(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching images by regular tag."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id1,
        items=[("category1", "tag_a"), ("category2", "tag_b")],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id2,
        items=[("category1", "tag_a")],
    )

    target3 = image_directory / "sub2" / "sub23" / "image_0_255_159.png"
    image_id3 = gui_app_instance.get_image_id(target3)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id3,
        items=[("category1", "tag_c")],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "regular_tag",
        "category": "category1",
        "name": "tag_a",
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_regular_tag.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target1) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert str(target3) not in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()


def test_search_by_description(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching images by description text."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.set_description(
        image_id=image_id1, description="a beautiful sunset over the ocean")

    target2 = image_directory / "sub1" / "sub12" / "image_31_255_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.set_description(
        image_id=image_id2, description="a beautiful mountain landscape")

    target3 = image_directory / "sub2" / "sub23" / "image_0_255_207.png"
    image_id3 = gui_app_instance.get_image_id(target3)
    gui_app_instance.repo.set_description(image_id=image_id3,
                                          description="just a random picture")

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "description",
        "text": "beautiful",
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_description.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target1) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert str(target3) not in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()


def test_search_by_path_contains(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching images by path containing a substring."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "path_contains",
        "text": "sub12",
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_path.png")

    results = search_tab.get_result_images()
    assert len(results) == 8

    subdir = image_directory / "sub1" / "sub12"
    for img in subdir.glob("*.png"):
        assert str(img) in [str(r) for r in results]

    assert "Found 8 images" in search_tab.status_label.text()


def test_search_and_combination(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching with AND combination of conditions."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id1,
        items=[("category1", "tag_a")],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[("general", "castle", 0.7)],
    )

    target3 = image_directory / "sub2" / "sub23" / "image_0_255_159.png"
    image_id3 = gui_app_instance.get_image_id(target3)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id3,
        items=[("category1", "tag_a")],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type":
        "and",
        "children": [
            {
                "type": "probabilistic_tag",
                "category": "general",
                "name": "castle",
                "min_probability": 0.5,
            },
            {
                "type": "regular_tag",
                "category": "category1",
                "name": "tag_a",
            },
        ],
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_and.png")

    results = search_tab.get_result_images()
    assert len(results) == 1
    assert str(target1) in [str(r) for r in results]
    assert str(target2) not in [str(r) for r in results]
    assert str(target3) not in [str(r) for r in results]
    assert "Found 1 images" in search_tab.status_label.text()


def test_search_or_combination(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching with OR combination of conditions."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id2,
        items=[("category1", "tag_a")],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type":
        "or",
        "children": [
            {
                "type": "probabilistic_tag",
                "category": "general",
                "name": "castle",
                "min_probability": 0.5,
            },
            {
                "type": "regular_tag",
                "category": "category1",
                "name": "tag_a",
            },
        ],
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_or.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target1) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()


def test_search_not_condition(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test searching with NOT condition."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "not",
        "child": {
            "type": "probabilistic_tag",
            "category": "general",
            "name": "castle",
            "min_probability": 0.5,
        },
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_not.png")

    results = search_tab.get_result_images()
    result_strs = [str(r) for r in results]
    assert str(target1) not in result_strs
    assert str(target2) in result_strs
    assert "Found" in search_tab.status_label.text()


def test_search_no_results(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test search that returns no results."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "probabilistic_tag",
        "category": "nonexistent_category",
        "name": "nonexistent_tag",
        "min_probability": 0.5,
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_no_results.png")

    results = search_tab.get_result_images()
    assert len(results) == 0
    assert "No images found" in search_tab.status_label.text()


def test_search_empty_spec(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test search with no valid conditions."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_empty.png")

    results = search_tab.get_result_images()
    assert len(results) == 0
    assert "No valid filter" in search_tab.status_label.text()


def test_search_double_click_emits_file_selected(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test double-clicking a search result emits fileSelected signal."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[("general", "test_tag", 0.9)],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()

    spec = {
        "type": "probabilistic_tag",
        "category": "general",
        "name": "test_tag",
        "min_probability": 0.5,
    }
    search_tab.set_search_spec(spec)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_double_click.png")

    results = search_tab.get_result_images()
    assert len(results) == 1
    assert str(target) == str(results[0])

    spy = QSignalSpy(gui_app_instance.window.left_panel.fileSelected)

    thumbnail_list = search_tab.thumbnail_list.list_view
    index = thumbnail_list.model().index(0, 0)
    thumbnail_list.doubleClicked.emit(index)
    qtbot.wait(50)

    assert spy.count() == 1
    assert spy.at(0)[0] == str(target)


def test_search_via_add_tag_to_query(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test adding tags to query via add_tag_to_query method."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[("general", "castle", 0.7)],
    )

    target2 = image_directory / "sub1" / "image_255_0_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[("general", "castle", 0.6)],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    search_tab.add_tag_to_query("probabilistic_tag", "general", "castle")
    qtbot.wait(50)

    take_screenshot(central_widget, screenshot_dir / "search_add_tag.png")

    search_tab.execute_search()
    qtbot.wait(100)

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()


def test_search_sexp_text_input(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test entering an S-expression directly into the text field."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[("general", "castle", 0.8)],
    )

    target2 = image_directory / "sub1" / "image_255_0_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[("general", "castle", 0.6)],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    # Type S-expression directly
    sexp = "(probabilistic_tag general castle 0.5)"
    search_tab.sexp_input.setPlainText(sexp)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_text.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
    assert "Found 2 images" in search_tab.status_label.text()

    # Verify state reflects the S-expression query
    state = gui_app_instance.window.get_state()
    assert state.left_panel.search_tab.sexp_query == sexp
    assert state.left_panel.active_tab == 1


def test_search_sexp_and_expression(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test entering a compound AND S-expression."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id1,
        items=[("category1", "tag_a")],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[("general", "castle", 0.7)],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    sexp = "(and (probabilistic_tag general castle 0.5) (regular_tag category1 tag_a))"
    search_tab.sexp_input.setPlainText(sexp)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_and.png")

    results = search_tab.get_result_images()
    assert len(results) == 1
    assert str(target1) in [str(r) for r in results]


def test_search_sexp_or_expression(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test entering a compound OR S-expression."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_regular_annotations(
        image_id=image_id2,
        items=[("category1", "tag_a")],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    sexp = "(or (probabilistic_tag general castle 0.5) (regular_tag category1 tag_a))"
    search_tab.sexp_input.setPlainText(sexp)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_or.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target1) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]


def test_search_sexp_not_expression(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test entering a NOT S-expression."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id1,
        items=[("general", "castle", 0.8)],
    )

    target2 = image_directory / "sub1" / "image_255_47_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    sexp = "(not (probabilistic_tag general castle 0.5))"
    search_tab.sexp_input.setPlainText(sexp)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_not.png")

    results = search_tab.get_result_images()
    result_strs = [str(r) for r in results]
    assert str(target1) not in result_strs
    assert str(target2) in result_strs


def test_search_sexp_add_tag_rewrites_expression(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that add_tag_to_query rewrites an existing S-expression correctly."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target = image_directory / "image_255_0_239.png"
    image_id = gui_app_instance.get_image_id(target)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id,
        items=[("general", "castle", 0.7)],
    )

    target2 = image_directory / "sub1" / "image_255_0_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.replace_probabilistic_annotations(
        image_id=image_id2,
        items=[("general", "castle", 0.6)],
    )

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    # Start with one tag
    search_tab.add_tag_to_query("probabilistic_tag", "general", "castle")
    qtbot.wait(50)

    first_text = search_tab.sexp_input.toPlainText().strip()
    assert first_text == "(probabilistic_tag general castle 0.5)"

    # Add another tag - should wrap in 'and'
    search_tab.add_tag_to_query("probabilistic_tag", "general", "castle")
    qtbot.wait(50)

    second_text = search_tab.sexp_input.toPlainText().strip()
    assert (
        second_text ==
        "(and (probabilistic_tag general castle 0.5) (probabilistic_tag general castle 0.5))"
    )

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_rewrite.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]


def test_search_sexp_invalid_input(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test that invalid S-expressions show an error instead of crashing."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    search_tab.sexp_input.setPlainText("(and (incomplete")
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget, screenshot_dir / "search_sexp_invalid.png")

    assert "Parse error" in search_tab.status_label.text()
    results = search_tab.get_result_images()
    assert len(results) == 0


def test_search_sexp_description_with_quotes(
    gui_app_instance: AppInstanceRes,
    screenshot_dir: Path,
    qtbot: QtBot,
    image_directory: Path,
):
    """Test description search with quoted string in S-expression."""
    qtbot.waitExposed(gui_app_instance.window)
    central_widget = gui_app_instance.window.centralWidget()

    target1 = image_directory / "image_255_0_239.png"
    image_id1 = gui_app_instance.get_image_id(target1)
    gui_app_instance.repo.set_description(
        image_id=image_id1, description="a beautiful sunset over the ocean")

    target2 = image_directory / "sub1" / "sub12" / "image_31_255_0.png"
    image_id2 = gui_app_instance.get_image_id(target2)
    gui_app_instance.repo.set_description(
        image_id=image_id2, description="a beautiful mountain landscape")

    search_tab = setup_search_tab(gui_app_instance, qtbot)
    search_tab.clear_search()
    qtbot.wait(50)

    sexp = '(description "beautiful")'
    search_tab.sexp_input.setPlainText(sexp)
    qtbot.wait(50)

    search_tab.execute_search()
    qtbot.wait(100)

    take_screenshot(central_widget,
                    screenshot_dir / "search_sexp_description.png")

    results = search_tab.get_result_images()
    assert len(results) == 2
    assert str(target1) in [str(r) for r in results]
    assert str(target2) in [str(r) for r in results]
