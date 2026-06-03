from dataclasses import dataclass, field
from pydantic import BaseModel, ConfigDict, Field, field_serializer
from pathlib import Path
import igraph
from beartype.typing import Dict, List


class CodeBlock(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)

    id: str = Field(description="Stable unique block identifier.")
    path: Path = Field(description="Path of file containing block.")
    key: str = Field(description="Semantic symbol key used for matching.")
    line_range: slice = Field(description="0-based half-open line range.")
    text: str = Field(description="Exact block source text.")
    scope: str = Field(description="Namespace scope string.")
    original_index: int = Field(description="Original order index in file.")
    is_include_block: bool = Field(
        description="Whether this block is include-group.")
    is_header_file: bool = Field(description="Whether file is header.")
    is_protected: bool = Field(description="Inside clang-format off region.")

    @field_serializer("line_range", when_used="json")
    def serialize_line_range(self, value: slice) -> dict:
        return {"start": value.start, "stop": value.stop, "step": value.step}


class ParsedFile(BaseModel):
    path: Path = Field(description="Parsed file path.")
    text: str = Field(description="Original file text.")
    lines: list[str] = Field(description="Original file lines with endings.")
    blocks: list[CodeBlock] = Field(
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
