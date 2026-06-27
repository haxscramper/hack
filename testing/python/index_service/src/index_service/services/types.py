from __future__ import annotations
from pathlib import Path

from beartype import beartype
from beartype.typing import Annotated
from pydantic import BaseModel, ConfigDict, Field, PlainValidator

AnyModel = Annotated[BaseModel, PlainValidator(lambda v: v)]


@beartype
class MD5(BaseModel, extra="forbid"):
    model_config = ConfigDict(frozen=True)
    md5: str

    def __repr__(self) -> str:
        return self.md5


@beartype
class FileRef(BaseModel, extra="forbid"):
    model_config = ConfigDict(frozen=True)
    md5: MD5
    path: Path


@beartype
class IndexerOutput(BaseModel, extra="forbid"):
    model_config = ConfigDict(frozen=True)
    indexer_id: str
    result: AnyModel


@beartype
class ConverterOutput(BaseModel, extra="forbid"):
    model_config = ConfigDict(frozen=True)
    converter_id: str
    output_files: list[Path]
    return_value: AnyModel


@beartype
class IndexerRequest(BaseModel, extra="forbid"):
    file_ref: FileRef
    dependency_results: dict[str, IndexerOutput] = Field(default_factory=dict)


@beartype
class ConverterRequest(BaseModel, extra="forbid"):
    input_files: list[FileRef]
    param: str = ""
