import json
import logging
import struct
from collections import Counter
from pathlib import Path

from beartype.typing import Any
from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.pydantic_utils import try_parse_json
from index_service.services.core.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel

log = logging.getLogger(__name__)


class TensorInfo(BaseModel, extra="forbid"):
    name: str
    dtype: str
    shape: list[int]
    num_elements: int
    byte_size: int


class SafetensorIndexerResult(BaseModel, extra="forbid"):
    size_bytes: int
    header_size: int
    metadata: dict[str, Any]
    num_tensors: int
    total_parameters: int
    total_tensor_bytes: int
    dtype_counts: dict[str, int]
    tensors: list[TensorInfo]


def _read_header(path: Path) -> tuple[int, dict[str, object]]:
    with path.open("rb") as f:
        (header_size,) = struct.unpack("<Q", f.read(8))
        header = try_parse_json(f.read(header_size).decode("utf-8"))
    return header_size, header


def _parse(path: Path) -> SafetensorIndexerResult:
    size_bytes = path.stat().st_size
    header_size, header = _read_header(path)

    metadata: dict[str, str] = header.pop("__metadata__", {})

    tensors: list[TensorInfo] = []
    dtype_counts: Counter[str] = Counter()
    total_parameters = 0
    total_tensor_bytes = 0

    for name, info in header.items():
        dtype = info["dtype"]
        shape = list(info["shape"])
        num_elements = 1
        for dim in shape:
            num_elements *= dim
        # Prefer the actual byte span from data_offsets when present.
        start, end = info["data_offsets"]
        byte_size = end - start

        tensors.append(
            TensorInfo(
                name=name,
                dtype=dtype,
                shape=shape,
                num_elements=num_elements,
                byte_size=byte_size,
            ))
        dtype_counts[dtype] += 1
        total_parameters += num_elements
        total_tensor_bytes += byte_size

    return SafetensorIndexerResult(
        size_bytes=size_bytes,
        header_size=header_size,
        metadata=metadata,
        num_tensors=len(tensors),
        total_parameters=total_parameters,
        total_tensor_bytes=total_tensor_bytes,
        dtype_counts=dict(dtype_counts),
        tensors=tensors,
    )


class SafetensorIndexer(BaseIndexer):
    asset_name = "safetensor"
    result_model = SafetensorIndexerResult

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)

    def can_run(self, path: Path) -> bool:
        return path.suffix.lower() in [".safetensors"]

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        path = ctx.get_path(request.file_ref)
        result = IndexerOutput(indexer_id=self.asset_name, result=_parse(path))
        log.info(f"{path} OK")
        return result
