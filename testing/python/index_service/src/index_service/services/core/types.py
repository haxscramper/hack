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


class IndexEdge(BaseModel, extra="forbid"):
    file_hash: str
    from_: str
    to_: str


class IndexDocument(BaseModel, extra="forbid"):
    hash: str


class IndexMultiDocument(IndexDocument, extra="forbid"):
    file_hash: str = Field(
        description=
        "Hash of the original file -- `.hash` field in all cases refers to *this* "
        "specific document, but if the query needs to re-assemble the information "
        "extracted from the file, grouping by `file_hash` is the way to go. ")


class MultiDocumentModel(BaseModel, extra="forbid"):
    edge_type: ClassVar[Any]
    document_type: ClassVar[Any]

    edges: list[IndexEdge]
    documents: list[IndexMultiDocument]


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

    def get_hash_str(self) -> str:
        return self.file_ref.hash.hash


@beartype
class ConverterRequest(BaseModel, extra="forbid"):
    input_files: list[FileRef]
    param: str = ""
