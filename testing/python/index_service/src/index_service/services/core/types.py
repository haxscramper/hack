from __future__ import annotations

from pathlib import Path

from beartype import beartype
from beartype.typing import Annotated, Any, ClassVar
from pydantic import BaseModel, ConfigDict, Field, PlainSerializer, PlainValidator

AnyModel = Annotated[
    BaseModel,
    PlainValidator(lambda v: v),
    PlainSerializer(lambda v: v.model_dump(), return_type=dict),
]


class IndexLink(BaseModel, extra="forbid"):
    from_: str
    to_: str


class IndexDocument(BaseModel, extra="forbid"):
    hash: str


class MultiDocumentModel(BaseModel, extra="forbid"):
    link_type: ClassVar[Any]
    document_type: ClassVar[Any]

    links: list[IndexLink]
    documents: list[IndexDocument]


class IndexerOutputError(BaseModel, extra="forbid"):
    description: str


class IndexerNotApplicable(BaseModel, extra="forbid"):
    reason: str


@beartype
class FileHash(BaseModel, extra="forbid"):
    model_config = ConfigDict(frozen=True)
    hash: str

    def __repr__(self) -> str:
        return self.hash


@beartype
class RootRef(BaseModel, extra="forbid"):
    model_config = ConfigDict(frozen=True)
    name: str


@beartype
class FileRef(BaseModel, extra="forbid"):
    model_config = ConfigDict(frozen=True)
    hash: FileHash
    relative: str
    root: RootRef


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
    return_value: AnyModel | IndexerOutputError | IndexerNotApplicable


@beartype
class IndexerRequest(BaseModel, extra="forbid"):
    file_ref: FileRef
    dependency_results: dict[str, IndexerOutput | None] = Field(default_factory=dict)


@beartype
class ConverterRequest(BaseModel, extra="forbid"):
    input_files: list[FileRef]
    param: str = ""
