import os
from pathlib import Path

import pytest
from beartype.typing import cast
from pydantic import BaseModel

from index_service.converters.file_size_converter import FileSizeConverterResult
from index_service.db import IndexDatabase
from index_service.indexers.file_size import FileSizeIndexerResult
from index_service.indexers.file_stats import FileStatsIndexerResult
from index_service.protocol import FileRef
from index_service.runtime import IndexRuntime

ARANGO_HOST = os.environ.get("ARANGO_HOST", "http://localhost:8529")
ARANGO_USER = os.environ.get("ARANGO_USER", "root")
ARANGO_PASSWORD = os.environ.get("ARANGO_ROOT_PASSWORD", "test")


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
    out = runtime.run_indexers(
        FileRef(md5="abc", path=sample_file),
        ["file_size"],
    )["file_size"]

    assert out.indexer_id == "file_size"
    assert cast(FileSizeIndexerResult,
                out.result).size_bytes == sample_file.stat().st_size


def test_file_stats_indexer(runtime: IndexRuntime, sample_file: Path) -> None:
    out = runtime.run_indexers(
        FileRef(md5="abc", path=sample_file),
        ["file_stats"],
    )["file_stats"]

    assert out.indexer_id == "file_stats"
    assert cast(FileStatsIndexerResult,
                out.result).size_bytes == sample_file.stat().st_size
    assert cast(FileStatsIndexerResult,
                out.result).mtime == sample_file.stat().st_mtime


def test_full_text_indexer_with_reverser(runtime: IndexRuntime,
                                         sample_file: Path) -> None:
    out = runtime.run_indexers(
        FileRef(md5="abc", path=sample_file),
        ["full_text"],
    )["full_text"]

    text = sample_file.read_text()
    assert out.result.text == text


def test_file_size_converter(runtime: IndexRuntime, sample_file: Path) -> None:
    out = runtime.run_converter("file_size_converter", [str(sample_file)])
    assert out.converter_id == "file_size_converter"
    assert cast(FileSizeConverterResult,
                out.return_value).total_size == sample_file.stat().st_size


def test_db_indexer_result_uniqueness(db: IndexDatabase) -> None:
    db.ensure_file("md5x", Path("/disk/a.txt"))
    db.ensure_file("md5x", Path("/disk/b.txt"))
    db.store_indexer_result("md5x", "file_size",
                            FileSizeIndexerResult(size_bytes=10))
    db.store_indexer_result("md5x", "file_size",
                            FileSizeIndexerResult(size_bytes=20))
    record = db.get_indexer_result("md5x", "file_size")
    assert record.result["size_bytes"] == 20


def test_db_store_derivation(db: IndexDatabase) -> None:

    class LocalDerivation(BaseModel, extra="forbid"):
        total_size: int

    key = db.store_derivation(["md5x"], [], {"param": ""},
                              LocalDerivation(total_size=17))
    assert isinstance(key, str)


def test_index_file_job(db: IndexDatabase, sample_file: Path) -> None:
    from index_service import dagster_defs
    from tests.conftest import MockFlmGemmaResource

    result = dagster_defs.run_index_file_job(
        arango=db,
        path=sample_file,
        resource_overrides={"flm_gemma": MockFlmGemmaResource()},
    )

    assert result.success
    size_record = db.get_indexer_result("filemd5", "file_size")
    assert size_record.result["size_bytes"] == sample_file.stat().st_size
    text_record = db.get_indexer_result("filemd5", "full_text")
    assert text_record.result["text"] == sample_file.read_text()


def test_convert_files_job(db: IndexDatabase, sample_file: Path) -> None:
    from index_service import dagster_defs

    result = dagster_defs.run_convert_files_job(
        arango=db,
        input_files=[str(sample_file)],
        input_md5s=["filemd5"],
        param="",
    )
    assert result.success


def test_file_summary_indexer_with_flm(
    runtime: IndexRuntime,
    sample_files: list[Path],
) -> None:

    for path in sample_files:
        out = runtime.run_indexers(
            FileRef(md5="batch-md5", path=path),
            ["file_summary"],
        )["file_summary"]

        summary = out.result.summary.summary
        assert summary.strip() != ""
