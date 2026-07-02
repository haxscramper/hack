from __future__ import annotations

import base64
import json
import math
import types as py_types
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Annotated, Any, Callable, TypeVar, Union, get_args, get_origin

import PIL.TiffImagePlugin
from pydantic import BaseModel, TypeAdapter
from pydantic_core import PydanticSerializationError

T = TypeVar("T")

# tag used to mark custom-serialized payloads so deserialization can dispatch
_TYPE_TAG = "__type__"

# registry: type name -> (dump fn, load fn)


@dataclass(frozen=True)
class _RegisteredType:
    name: str
    json_type: Any
    adapter: TypeAdapter[Any]
    dump: Callable[[Any], Any]
    load: Callable[[Any], Any]


_DUMPERS: dict[type[Any], _RegisteredType] = {}
_LOADERS: dict[str, _RegisteredType] = {}


def register_type(
    tp: type[T],
    name: str,
    json_type: Any,
    dump: Callable[[T], J],
    load: Callable[[J], T],
) -> None:
    entry = _RegisteredType(
        name=name,
        json_type=json_type,
        adapter=TypeAdapter(json_type),
        dump=dump,
        load=load,
    )
    _DUMPERS[tp] = entry
    _LOADERS[name] = entry


register_type(
    Path,
    "Path",
    str,
    dump=lambda it: str(it),
    load=lambda it: Path(it),
)

register_type(
    datetime,
    "datetime",
    str,
    dump=lambda it: it.isoformat(),
    load=lambda it: datetime.fromisoformat(it),
)


class IFDRationalJson(BaseModel):
    value: int
    denominator: int


register_type(
    PIL.TiffImagePlugin.IFDRational,
    "PIL.TiffImagePlugin.IFDRational",
    IFDRationalJson,
    dump=lambda it: IFDRationalJson(
        value=it.numerator,
        denominator=it.denominator,
    ),
    load=lambda it: PIL.TiffImagePlugin.IFDRational(it.value, it.denominator),
)


def _wrap(name: str, payload: Any) -> dict[str, Any]:
    return {_TYPE_TAG: name, "data": payload}


def _tagged_schema(name: str, payload_schema: dict[str, Any]) -> dict[str, Any]:
    return {
        "type": "object",
        "properties": {
            _TYPE_TAG: {
                "type": "string",
                "enum": [name],
            },
            "data": payload_schema,
        },
        "required": [_TYPE_TAG, "data"],
        "additionalProperties": False,
    }


def _registered_type_for_annotation(annotation: Any) -> _RegisteredType | None:
    if isinstance(annotation, type):
        entry = _DUMPERS.get(annotation)
        if entry is not None:
            return entry

        for tp, candidate in _DUMPERS.items():
            if issubclass(annotation, tp):
                return candidate

    return None


def _annotation_to_arango_schema(annotation: Any) -> dict[str, Any]:
    if annotation is Any:
        return {}

    if annotation is None or annotation is type(None):
        return {"type": "null"}

    registered = _registered_type_for_annotation(annotation)
    if registered is not None:
        return _tagged_schema(
            registered.name,
            _annotation_to_arango_schema(registered.json_type),
        )

    if annotation is bytes:
        return _tagged_schema("bytes", {"type": "string"})

    if annotation is bool:
        return {"type": "boolean"}

    if annotation is int:
        return {"type": "integer"}

    if annotation is float:
        return {"type": "number"}

    if annotation is str:
        return {"type": "string"}

    origin = get_origin(annotation)

    if origin is Annotated:
        inner, *_ = get_args(annotation)
        return _annotation_to_arango_schema(inner)

    if origin in (Union, py_types.UnionType):
        return {
            "anyOf": [_annotation_to_arango_schema(arg) for arg in get_args(annotation)]
        }

    if origin in (list, set, frozenset):
        args = get_args(annotation)
        item_type = args[0] if args else Any
        return {
            "type": "array",
            "items": _annotation_to_arango_schema(item_type),
        }

    if origin is tuple:
        args = get_args(annotation)
        if len(args) == 2 and args[1] is Ellipsis:
            return {
                "type": "array",
                "items": _annotation_to_arango_schema(args[0]),
            }

        if args:
            return {
                "type": "array",
                "items": {
                    "anyOf": [_annotation_to_arango_schema(arg) for arg in args]
                },
            }

        return {"type": "array"}

    if origin is dict:
        args = get_args(annotation)
        value_type = args[1] if len(args) == 2 else Any
        return {
            "type": "object",
            "additionalProperties": _annotation_to_arango_schema(value_type),
        }

    if isinstance(annotation, type) and issubclass(annotation, BaseModel):
        annotation.model_rebuild()

        properties: dict[str, Any] = {}
        required: list[str] = []

        for field_name, field in annotation.model_fields.items():
            properties[field_name] = _annotation_to_arango_schema(field.annotation)
            if field.is_required():
                required.append(field_name)

        schema: dict[str, Any] = {
            "type": "object",
            "properties": properties,
            "additionalProperties": False,
        }

        if required:
            schema["required"] = required

        return schema

    return {}


def arango_schema_for_model(model_type: type[BaseModel]) -> dict[str, Any]:
    model_type.model_rebuild()
    return _annotation_to_arango_schema(model_type)


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
            entry = _DUMPERS.get(type(value))
            if entry is None:
                for tp, candidate in _DUMPERS.items():
                    if isinstance(value, tp):
                        entry = candidate
                        break

            if entry is not None:
                payload = entry.adapter.validate_python(entry.dump(value))
                return _wrap(entry.name, to_json_safe(payload))

            raise PydanticSerializationError(
                f"no serializer registered for type {type(value)!r}")


def _restore_json_safe(data: Any) -> Any:
    if isinstance(data, dict):
        tag = data.get(_TYPE_TAG)
        if tag is not None:
            payload = _restore_json_safe(data["data"])
            if tag == "bytes":
                return base64.b64decode(payload)

            entry = _LOADERS.get(tag)
            if entry is None:
                raise ValueError(f"no loader registered for tag {tag!r}")

            typed_payload = entry.adapter.validate_python(payload)
            return entry.load(typed_payload)

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


def try_parse_json(value: Any):
    match value:
        case bytes():
            try:
                value = value.decode("utf-8")
            except UnicodeDecodeError:
                return value

        case str():
            try:
                # to recursively unpack JSON dumps that contain
                # json strings as field values.
                return try_parse_json(json.loads(value))
            except json.JSONDecodeError:
                return value

        case dict():
            return {k: try_parse_json(v) for k, v in value.items()}

        case list():
            return [try_parse_json(v) for v in value]

        case _:
            return value
