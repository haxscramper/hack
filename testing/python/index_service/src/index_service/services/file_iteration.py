from dataclasses import dataclass

from pathspec import GitIgnoreSpec
from pydantic import BaseModel, Field, field_validator
from pathlib import Path
from beartype.typing import Sequence


class DirConfig(BaseModel, extra="forbid"):
    path: Path
    ignore: list[str] = Field(default_factory=list)

    @field_validator("path")
    @classmethod
    def _normalize_paths(cls, value: Path) -> Path:
        return value.expanduser().resolve().absolute()


@dataclass(slots=True, frozen=True)
class RootFilter:
    root_path: Path
    root_str: str
    ignore_spec: GitIgnoreSpec | None

    @field_validator("root_path")
    @classmethod
    def _normalize_paths(cls, value: Path) -> Path:
        return value.expanduser().resolve().absolute()


def prepare_root_filters(root_directories: Sequence[DirConfig]) -> list[RootFilter]:
    filters: list[RootFilter] = []

    for entry in root_directories:
        root_path = entry.path.expanduser().resolve()
        root_str = root_path.as_posix().rstrip("/")
        ignore_spec = (GitIgnoreSpec.from_lines(entry.ignore) if entry.ignore else None)
        filters.append(
            RootFilter(
                root_path=root_path,
                root_str=root_str,
                ignore_spec=ignore_spec,
            ))

    filters.sort(key=lambda item: len(item.root_str), reverse=True)
    return filters


def match_root(path_str: str,
               root_filters: Sequence[RootFilter]) -> tuple[RootFilter, str] | None:
    for root in root_filters:
        if path_str == root.root_str:
            return root, ""

        prefix = root.root_str + "/"
        if path_str.startswith(prefix):
            return root, path_str[len(prefix):]

    return None
