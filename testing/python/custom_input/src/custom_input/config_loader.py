from pathlib import Path
from typing import Any, TypeVar

from pydantic import BaseModel, ValidationError
from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedMap, CommentedSeq

ModelT = TypeVar("ModelT", bound=BaseModel)


class YamlValidationError(ValueError):
    pass


def _yaml_error_position(
    document: Any,
    location: tuple[str | int, ...],
) -> tuple[int, int]:
    node = document
    line = node.lc.line if hasattr(node, "lc") else 0
    column = node.lc.col if hasattr(node, "lc") else 0

    for component in location:
        if isinstance(node, CommentedMap) and component in node:
            line, column = node.lc.key(component)
            node = node[component]
            continue

        if (isinstance(node, CommentedSeq) and isinstance(component, int)
                and 0 <= component < len(node)):
            line, column = node.lc.item(component)
            node = node[component]
            continue

        break

    return line + 1, column + 1


def _format_location(location: tuple[str | int, ...]) -> str:
    result = ""

    for component in location:
        if isinstance(component, int):
            result += f"[{component}]"
        elif result:
            result += f".{component}"
        else:
            result = component

    return result


def load_yaml(model_type: type[ModelT], path: Path) -> ModelT:
    yaml = YAML(typ="rt")

    with path.open() as file:
        document = yaml.load(file)

    try:
        return model_type.model_validate(document)
    except ValidationError as error:
        messages: list[str] = []

        for detail in error.errors(include_url=False):
            location = detail["loc"]
            line, column = _yaml_error_position(document, location)
            field = _format_location(location)

            messages.append(
                f"{path}:{line}:{column}: {field}: {detail['msg']}")

        raise YamlValidationError("\n" + "\n".join(messages)) from None
