from __future__ import annotations

from typing import Any

from beartype import beartype
from beartype.typing import Any, Annotated
from pydantic import BaseModel, ConfigDict, Field
from pydantic import BaseModel, ConfigDict, PlainValidator

AnyModel = Annotated[BaseModel, PlainValidator(lambda v: v)]


@beartype
class FileRef(BaseModel):
    model_config = ConfigDict(frozen=True)
    md5: str
    paths: list[str]


@beartype
class IndexerOutput(BaseModel):
    model_config = ConfigDict(frozen=True)
    indexer_id: str
    result_type: str
    result: AnyModel


@beartype
class ConverterOutput(BaseModel):
    model_config = ConfigDict(frozen=True)
    converter_id: str
    output_files: list[str]
    return_value: AnyModel


@beartype
class IndexerRequest(BaseModel):
    file_ref: FileRef
    dependency_results: dict[str, IndexerOutput] = Field(default_factory=dict)


@beartype
class ConverterRequest(BaseModel):
    input_files: list[str]
    param: str = ""
