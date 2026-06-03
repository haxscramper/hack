from dataclasses import dataclass, field
from pydantic import BaseModel, ConfigDict, Field, field_serializer, model_validator
from pathlib import Path
import igraph
from beartype.typing import Dict, List, Tuple, Optional
from enum import Enum


class BlockType(str, Enum):
    NAMESPACE_OPEN = "namespace_open"
    NAMESPACE_CLOSE = "namespace_close"
    DEFINITION = "definition"
    DECLARATION = "declaration"
    PREPROCESSOR_BLOCK = "preprocessor_block"
    CLANG_FORMAT_BLOCK = "clang_format_block"
    LOCAL_ENTRY = "local_entry"


class QualType(BaseModel):
    model_config = ConfigDict(frozen=True)

    name: str = Field(description="Entry name.")
    parent_namespaces: Tuple["QualType", ...] = Field(
        default_factory=tuple,
        description="Parent namespaces.",
    )
    parameters: Tuple["QualType", ...] = Field(
        default_factory=tuple,
        description="Type parameters and signature arguments.",
    )

    @model_validator(mode="before")
    @classmethod
    def _normalize_input(cls, value: object) -> object:
        if isinstance(value, QualType):
            return value
        if not isinstance(value, dict):
            return value
        raw_name = value.get("name", "")
        normalized_name = " ".join(str(raw_name).strip().split())
        raw_parents = value.get("parent_namespaces", ())
        raw_parameters = value.get("parameters", ())
        parents = tuple(item if isinstance(item, QualType) else QualType.
                        model_validate(item) for item in raw_parents)
        parameters = tuple(item if isinstance(item, QualType) else QualType.
                           model_validate(item) for item in raw_parameters)
        return {
            "name": normalized_name,
            "parent_namespaces": parents,
            "parameters": parameters,
        }

    def to_flat_tuple(self) -> Tuple[str, ...]:
        result: List[str] = [self.name, "<P>"]
        idx = 0
        while idx < len(self.parent_namespaces):
            result.extend(self.parent_namespaces[idx].to_flat_tuple())
            result.append("<N>")
            idx += 1
        result.append("<A>")
        idx = 0
        while idx < len(self.parameters):
            result.extend(self.parameters[idx].to_flat_tuple())
            result.append("<T>")
            idx += 1
        return tuple(result)


class CodeBlock(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)

    id: str = Field(description="Stable unique block identifier.")
    path: Path = Field(description="Path of file containing block.")
    key: str = Field(description="Semantic symbol key used for matching.")
    line_range: slice = Field(description="0-based half-open line range.")
    text: str = Field(description="Exact block source text.")
    scope: str = Field(description="Namespace scope string.")
    original_index: int = Field(description="Original order index in file.")
    block_type: BlockType = Field(description="Category of source block.")
    qualified_name: Optional[QualType] = Field(
        default=None,
        description="Flattened fully qualified identity.",
    )

    @field_serializer("line_range", when_used="json")
    def serialize_line_range(self, value: slice) -> dict:
        return {"start": value.start, "stop": value.stop, "step": value.step}


class ParsedFile(BaseModel):
    path: Path = Field(description="Parsed file path.")
    text: str = Field(description="Original file text.")
    lines: List[str] = Field(description="Original file lines with endings.")
    blocks: List[CodeBlock] = Field(
        description="Extracted reorderable blocks.")


class RepositoryParse(BaseModel):
    root: Path = Field(description="Repository root path.")
    files: list[ParsedFile] = Field(description="All parsed files.")


@dataclass
class RepositoryGraph:
    graph: igraph.Graph = field(metadata={"doc": "Directed dependency graph."})
    blocks: List[CodeBlock] = field(
        metadata={"doc": "Blocks in vertex index order."})
    id_to_index: Dict[str, int] = field(
        metadata={"doc": "Map from block id to graph vertex index."})
