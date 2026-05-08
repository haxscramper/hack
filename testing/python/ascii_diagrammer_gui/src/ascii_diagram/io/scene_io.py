from __future__ import annotations

import json
from typing import Any, Dict, List, Optional

from ascii_diagram.model.enums import ShapeType
from ascii_diagram.model.scene_item import SceneItem
from ascii_diagram.model.scene_model import SceneModel, QModelIndex
from ascii_diagram.model.shape_properties import (
    make_default_properties,
    property_from_dict,
)


def model_to_dict(model: SceneModel) -> Dict[str, Any]:
    result: Dict[str, Any] = {"shapes": []}

    def serialize_item(parent: QModelIndex) -> List[Dict[str, Any]]:
        items = []
        for row in range(model.rowCount(parent)):
            idx = model.index(row, 0, parent)
            item = model.item_from_index(idx)
            if item is None:
                continue
            entry = {
                "id": item.item_id,
                "shape_type": item.shape_type.name,
                "properties": item.properties.to_dict(),
                "children": serialize_item(idx),
            }
            items.append(entry)
        return items

    result["shapes"] = serialize_item(QModelIndex())
    return result


def model_to_json(model: SceneModel, indent: int = 2) -> str:
    return json.dumps(model_to_dict(model), indent=indent)


def dict_to_model(model: SceneModel, data: Dict[str, Any]) -> None:
    model.reset_model()

    def deserialize_item(parent_idx: QModelIndex,
                         items: List[Dict[str, Any]]) -> None:
        for entry in items:
            shape_type = ShapeType[entry["shape_type"]]
            props = property_from_dict(entry["shape_type"],
                                       entry["properties"])
            row = model.rowCount(parent_idx)
            model.beginInsertRows(parent_idx, row, row)
            parent_item = model.item_from_index(parent_idx)
            if parent_item is None:
                model.endInsertRows()
                continue
            child = SceneItem(shape_type, item_id=entry["id"])
            child.properties = props
            parent_item.add_child(child)
            model.endInsertRows()
            child_idx = model.index(
                model.rowCount(parent_idx) - 1, 0, parent_idx)
            deserialize_item(child_idx, entry.get("children", []))

    deserialize_item(QModelIndex(), data.get("shapes", []))


def json_to_model(model: SceneModel, json_str: str) -> None:
    data = json.loads(json_str)
    dict_to_model(model, data)


def save_to_file(model: SceneModel, path: str) -> None:
    with open(path, "w", encoding="utf-8") as f:
        f.write(model_to_json(model))


def load_from_file(model: SceneModel, path: str) -> None:
    with open(path, "r", encoding="utf-8") as f:
        json_to_model(model, f.read())
