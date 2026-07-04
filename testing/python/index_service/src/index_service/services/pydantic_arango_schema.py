from dataclasses import dataclass, field
from beartype.typing import Annotated, Literal, Any, Union, get_args, get_origin
from pydantic import BaseModel

from index_service.services.pydantic_utils import _DUMPERS, _TYPE_TAG, _RegisteredType
import types as py_types
import logging

log = logging.getLogger(__name__)


@dataclass
class ArangoSchema:
    schema: dict[str, Any]
    definitions: dict[str, Any] = field(default_factory=dict)


class _SchemaContext:

    def __init__(self) -> None:
        self.definitions: dict[str, Any] = {}


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


def _model_ref(annotation: type[BaseModel], ctx: _SchemaContext) -> dict[str, Any]:
    name = annotation.__name__
    ref = {"$ref": f"#/definitions/{name}"}

    if name in ctx.definitions:
        return ref

    # Reserve the slot before recursing so self-referential / mutually
    # recursive models terminate on the ref instead of recursing forever.
    ctx.definitions[name] = {}

    annotation.model_rebuild()

    properties: dict[str, Any] = {}
    required: list[str] = []

    for field_name, model_field in annotation.model_fields.items():
        properties[field_name] = _annotation_to_arango_schema(model_field.annotation, ctx)
        if model_field.is_required():
            required.append(field_name)

    schema: dict[str, Any] = {
        "type": "object",
        "properties": properties,
        "additionalProperties": False,
    }

    if required:
        schema["required"] = required

    ctx.definitions[name] = schema
    return ref


def _annotation_to_arango_schema(annotation: Any, ctx: _SchemaContext) -> dict[str, Any]:
    if annotation is Any:
        return {}

    if annotation is None or annotation is type(None):
        return {"type": "null"}

    registered = _registered_type_for_annotation(annotation)
    if registered is not None:
        return _tagged_schema(
            registered.name,
            _annotation_to_arango_schema(registered.json_type, ctx),
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

    if origin is Literal:
        return {"enum": list(get_args(annotation))}

    if origin is Annotated:
        inner, *_ = get_args(annotation)
        return _annotation_to_arango_schema(inner, ctx)

    if origin in (Union, py_types.UnionType):
        return {
            "anyOf": [
                _annotation_to_arango_schema(arg, ctx) for arg in get_args(annotation)
            ]
        }

    if origin in (list, set, frozenset):
        args = get_args(annotation)
        item_type = args[0] if args else Any
        return {
            "type": "array",
            "items": _annotation_to_arango_schema(item_type, ctx),
        }

    if origin is tuple:
        args = get_args(annotation)
        if len(args) == 2 and args[1] is Ellipsis:
            return {
                "type": "array",
                "items": _annotation_to_arango_schema(args[0], ctx),
            }

        if args:
            return {
                "type": "array",
                "items": {
                    "anyOf": [_annotation_to_arango_schema(arg, ctx) for arg in args]
                },
            }

        return {"type": "array"}

    if origin is dict:
        args = get_args(annotation)
        value_type = args[1] if len(args) == 2 else Any
        return {
            "type": "object",
            "additionalProperties": _annotation_to_arango_schema(value_type, ctx),
        }

    if isinstance(annotation, type) and issubclass(annotation, BaseModel):
        return _model_ref(annotation, ctx)

    return {}


def arango_schema_for_model(annotation: Any) -> ArangoSchema:
    ctx = _SchemaContext()
    schema = _annotation_to_arango_schema(annotation, ctx)
    result = ArangoSchema(schema=schema, definitions=ctx.definitions)
    return result
