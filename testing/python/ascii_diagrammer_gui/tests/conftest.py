import pytest

from ascii_diagram.model.scene_model import SceneModel
from ascii_diagram.model.enums import ShapeType


@pytest.fixture
def empty_model():
    return SceneModel()


@pytest.fixture
def rect_model():
    model = SceneModel()
    model.insert_item(ShapeType.RECTANGLE)
    return model


@pytest.fixture
def nested_model():
    model = SceneModel()
    parent_idx = model.insert_item(ShapeType.RECTANGLE)
    child_idx = model.insert_item(ShapeType.ELLIPSE, parent_idx)
    return model


@pytest.fixture
def complex_model():
    model = SceneModel()
    model.insert_item(ShapeType.RECTANGLE)
    edge_idx = model.insert_item(ShapeType.EDGE)
    ellipse_idx = model.insert_item(ShapeType.ELLIPSE)
    model.insert_item(ShapeType.TEXT)
    parent_idx = model.index(0, 0)
    model.insert_item(ShapeType.RECTANGLE, parent_idx)
    return model