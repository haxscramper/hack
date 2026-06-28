from __future__ import annotations

import base64
from datetime import datetime
import math
from pathlib import Path
from typing import Any, Callable, TypeVar

from pydantic import BaseModel, TypeAdapter
from pydantic_core import PydanticSerializationError
import PIL.TiffImagePlugin

T = TypeVar("T")

# tag used to mark custom-serialized payloads so deserialization can dispatch
_TYPE_TAG = "__type__"

# registry: type name -> (dump fn, load fn)
_DUMPERS: dict[type, tuple[str, Callable[[Any], Any]]] = {}
_LOADERS: dict[str, Callable[[Any], Any]] = {}


def register_type(
    tp: type[T],
    name: str,
    dump: Callable[[T], Any],
    load: Callable[[Any], T],
) -> None:
    _DUMPERS[tp] = (name, dump)
    _LOADERS[name] = load


register_type(
    Path,
    "Path",
    lambda it: str(Path),
    lambda it: Path(it),
)

register_type(
    datetime,
    "datetime",
    lambda it: it.isoformat(),
    lambda it: datetime.fromisoformat(it),
)

register_type(
    PIL.TiffImagePlugin.IFDRational,
    "PIL.TiffImagePlugin.IFDRational",
    lambda it: dict(
        value=it.numerator,
        denominator=it.denominator,
    ),
    lambda it: PIL.TiffImagePlugin.IFDRational(it["value"], it["denominator"]),
)


def _wrap(name: str, payload: Any) -> dict[str, Any]:
    return {_TYPE_TAG: name, "data": payload}


def to_json_safe(value: Any) -> Any:
    match value:
        case None | bool() | int() | str():
            return value

        case bytes():
            return _wrap("bytes", base64.b64encode(value).decode("ascii"))

        case float():
            if math.isnan(value) or math.isinf(value):
                return None
            return value

        case BaseModel():
            # walk fields ourselves, one level, then recurse
            result: dict[str, Any] = {}
            for field_name in type(value).model_fields:
                result[field_name] = to_json_safe(getattr(value, field_name))
            return result

        case dict():
            return {k: to_json_safe(v) for k, v in value.items()}

        case list() | tuple() | set():
            return [to_json_safe(v) for v in value]

        case _:
            # custom registered types (check by exact type then by isinstance)
            entry = _DUMPERS.get(type(value))
            if entry is None:
                for tp, e in _DUMPERS.items():
                    if isinstance(value, tp):
                        entry = e
                        break

            if entry is not None:
                name, dump = entry
                return _wrap(name, to_json_safe(dump(value)))

            raise PydanticSerializationError(
                f"no serializer registered for type {type(value)!r}")


def _restore_json_safe(data: Any) -> Any:
    if isinstance(data, dict):
        tag = data.get(_TYPE_TAG)
        if tag is not None:
            payload = _restore_json_safe(data["data"])
            if tag == "bytes":
                return base64.b64decode(payload)
            loader = _LOADERS.get(tag)
            if loader is None:
                raise ValueError(f"no loader registered for tag {tag!r}")
            return loader(payload)

        return {k: _restore_json_safe(v) for k, v in data.items()}

    if isinstance(data, list):
        return [_restore_json_safe(v) for v in data]

    return data


def from_json_safe(data: Any, target_type: type[T]) -> T:
    return TypeAdapter(target_type).validate_python(_restore_json_safe(data))


def model_to_json_data(model: BaseModel) -> Any:
    return to_json_safe(model)


def model_from_json_data(data: Any, model_type: type[T]) -> T:
    return from_json_safe(data, model_type)
