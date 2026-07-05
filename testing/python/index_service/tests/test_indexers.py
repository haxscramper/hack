import json
import os
from pathlib import Path
from pprint import pformat

import pytest
from beartype.typing import Any, TypeVar, cast
from pydantic import BaseModel
import glom

from index_service.services.converters.file_size_converter import FileSizeConverterResult
from index_service.services.core.db import IndexDatabase
from index_service.services.indexers.chunk_indexing.chunking import ChunkDocument, ChunkFile, ChunkLink
from index_service.services.indexers.chunk_indexing.file_summary import FileSummaryIndexerResult
from index_service.services.indexers.file_size import FileSizeIndexer, FileSizeIndexerResult
from index_service.services.indexers.file_stats import FileStatsIndexerResult
from index_service.services.core.types import FileHash, FileRef, IndexerOutput
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.indexers.chunk_indexing.full_text import FullTextIndexerResult
import logging
import functools

from index_service.services.pydantic_utils import dump_with_type, first_by_field_value
from index_service.services.resources.text_summary import SummaryChunk

log = logging.getLogger(__name__)

ARANGO_HOST = os.environ.get("ARANGO_HOST", "http://localhost:8529")
ARANGO_USER = os.environ.get("ARANGO_USER", "root")
ARANGO_PASSWORD = os.environ.get("ARANGO_ROOT_PASSWORD", "test")
T = TypeVar("T")


@pytest.fixture
def sample_file(tmp_path: Path) -> Path:
    path = tmp_path / "sample.txt"
    path.write_text("alpha\nbeta\ngamma\n")
    return path


@pytest.fixture
def sample_files(tmp_path: Path) -> list[Path]:
    texts = [
        "alpha beta gamma\nthis file talks about physics and light scattering\n",
        "meeting notes\n- budget approved\n- timeline pushed by 2 weeks\n",
        "recipe\nonion garlic tomato basil olive oil pasta\n",
        "log entry\nservice restarted successfully after config update\n",
        "short poem\nwind over stone\nnight over field\n",
        "release summary\nnew indexer added and integration test updated\n",
    ]
    files: list[Path] = []
    for idx, text in enumerate(texts):
        path = tmp_path / f"doc_{idx}.txt"
        path.write_text(text)
        files.append(path)
    return files


def test_file_size_indexer(runtime: IndexRuntime, sample_file: Path) -> None:
    root = runtime.db.add_root("main", sample_file.parent)
    ref = runtime.db.as_ref(root, sample_file)
    runtime.run_indexer(ref, ["file_size"])
    out = runtime.get_indexer_result(ref.hash, "file_size")

    assert out.indexer_id == "file_size"
    assert cast(FileSizeIndexerResult,
                out.result).size_bytes == sample_file.stat().st_size


def test_file_stats_indexer(runtime: IndexRuntime, sample_file: Path) -> None:
    root = runtime.db.add_root("root", sample_file)
    ref = runtime.db.as_ref(root, sample_file)
    runtime.run_indexer(ref, ["file_stats"])
    out = runtime.get_indexer_result(ref.hash, "file_stats")

    assert out.indexer_id == "file_stats"
    assert cast(FileStatsIndexerResult,
                out.result).size_bytes == sample_file.stat().st_size
    assert cast(FileStatsIndexerResult, out.result).mtime == sample_file.stat().st_mtime


def test_full_text_indexer_with_reverser(runtime: IndexRuntime,
                                         sample_file: Path) -> None:
    root = runtime.db.add_root("root", sample_file.parent)
    ref = runtime.db.as_ref(root, sample_file)
    runtime.run_indexer(ref, ["full_text"])
    out = runtime.get_indexer_result(ref, "full_text")

    assert isinstance(out.result, FullTextIndexerResult)
    text = sample_file.read_text()

    log.debug("dump with type")
    log.debug(json.dumps(dump_with_type(out), indent=2))

    def obj_has_field_value(obj: object, field: str, value: Any) -> bool:
        return getattr(obj, field) == value

    main_file = glom.glom(
        out.result.documents,
        glom.Iter().first(
            # NOTE: adding `cast`, or moving the handling to the separate function has not
            # here, the typing error from pylanc still pops up, no matter what I do.
            functools.partial(  # type: ignore
                obj_has_field_value,
                field="type",
                value="chunkFile",
            ),
            default=None))

    assert main_file is not None
    assert isinstance(main_file, ChunkFile)
    assert main_file.hash == main_file.file_hash

    chunk_document = glom.glom(
        out.result.documents,
        glom.Iter().first(
            lambda it: it.type == "chunk",  # type: ignore
            default=None))

    assert chunk_document is not None
    assert isinstance(chunk_document, ChunkDocument)
    # full text does not guarantee 100% correct recall wrt. markup formatting
    # spaces, indentation etc. -- the elements pass through several layers
    # of normalization and splitting.
    assert chunk_document.text in text
    assert chunk_document.file_hash != chunk_document.hash
    assert chunk_document.file_hash == main_file.file_hash

    link = glom.glom(
        out.result.edges,
        glom.Iter().first(
            lambda it: it.relation == "chunk",  # type: ignore
            default=None))

    assert link is not None
    assert isinstance(link, ChunkLink)
    assert link.from_ == main_file.hash
    assert link.to_ == chunk_document.hash


def test_file_size_converter(db: IndexDatabase, runtime: IndexRuntime,
                             sample_file: Path) -> None:
    root = runtime.db.add_root("root", sample_file.parent)
    out = runtime.run_converter("file_size_converter",
                                inputs=[db.as_ref(root, sample_file)])

    assert out.converter_id == "file_size_converter"
    assert cast(FileSizeConverterResult,
                out.return_value).total_size == sample_file.stat().st_size


def test_db_indexer_result_uniqueness(db: IndexDatabase, tmp_path: Path) -> None:
    root = db.add_root("root", tmp_path)
    pa = tmp_path.joinpath("a.txt")
    pa.write_text("---")
    pb = tmp_path.joinpath("b.txt")
    pb.write_text("---")
    ref_a = db.as_ref(root, pa)
    ref_b = db.as_ref(root, pb)
    db.ensure_collections([FileSizeIndexer()])
    db.truncate_all(["file_size"])
    db.store_indexer_output(
        ref_a,
        IndexerOutput(indexer_id="file_size",
                      result=FileSizeIndexerResult(
                          size_bytes=10,
                          hash=ref_a.hash.hash,
                      )))

    db.store_indexer_output(
        ref_b,
        IndexerOutput(indexer_id="file_size",
                      result=FileSizeIndexerResult(
                          size_bytes=20,
                          hash=ref_b.hash.hash,
                      )))

    record = db.get_indexer_result(
        ref_a.hash,
        "file_size",
        FileSizeIndexerResult,
    )

    assert record
    assert record.size_bytes == 20


def test_db_store_derivation(db: IndexDatabase) -> None:

    class LocalDerivation(BaseModel, extra="forbid"):
        total_size: int

    root = db.add_root("root", Path("?"))
    key = db.store_derivation(
        input=[FileRef(hash=FileHash(hash="hash"), relative="?X", root=root)],
        output_files=["sdf"],
        config={"param": str()},
        return_value=LocalDerivation(total_size=17),
    )

    assert isinstance(key, str)


def test_convert_files_job(runtime: IndexRuntime, sample_file: Path) -> None:
    root = runtime.db.add_root("root", sample_file.parent)
    out = runtime.run_converter("file_size_converter",
                                [runtime.db.as_ref(root, sample_file)])
    assert out.converter_id == "file_size_converter"


def test_file_summary_indexer_with_flm(
    runtime: IndexRuntime,
    sample_files: list[Path],
) -> None:

    root = runtime.db.add_root("root",
                               Path(os.path.commonpath(str(s) for s in sample_files)))

    for path in sample_files:
        ref = runtime.db.as_ref(root, path)
        runtime.run_indexer(ref, ["file_summary"])
        out = runtime.get_indexer_result(ref, "file_summary")
        assert isinstance(out.result, FileSummaryIndexerResult)
        log.debug(json.dumps(dump_with_type(out), indent=2))

        file = first_by_field_value(out.result.documents, "type", "chunk")
        assert isinstance(file, SummaryChunk)
        assert file.summary.strip() != ""
