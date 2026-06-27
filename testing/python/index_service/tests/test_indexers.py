import os
from pathlib import Path

import pytest
from beartype.typing import cast
from pydantic import BaseModel

from index_service.services.converters.file_size_converter import FileSizeConverterResult
from index_service.services.db import IndexDatabase
from index_service.services.indexers.file_size import FileSizeIndexerResult
from index_service.services.indexers.file_stats import FileStatsIndexerResult
from index_service.services.types import MD5, FileRef
from index_service.services.runtime import IndexRuntime

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
        runtime.db.as_ref(sample_file),
        ["file_size"],
    )["file_size"]

    assert out.indexer_id == "file_size"
    assert cast(FileSizeIndexerResult,
                out.result).size_bytes == sample_file.stat().st_size


def test_file_stats_indexer(runtime: IndexRuntime, sample_file: Path) -> None:
    out = runtime.run_indexers(
        runtime.db.as_ref(sample_file),
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
        runtime.db.as_ref(sample_file),
        ["full_text"],
    )["full_text"]

    text = sample_file.read_text()
    assert out.result.text == text


def test_file_size_converter(db: IndexDatabase, runtime: IndexRuntime,
                             sample_file: Path) -> None:
    out = runtime.run_converter("file_size_converter",
                                input=[db.as_ref(sample_file)])

    assert out.converter_id == "file_size_converter"
    assert cast(FileSizeConverterResult,
                out.return_value).total_size == sample_file.stat().st_size


def test_db_indexer_result_uniqueness(db: IndexDatabase,
                                      tmp_path: Path) -> None:
    pa = tmp_path.joinpath("a.txt")
    pa.write_text("---")
    pb = tmp_path.joinpath("b.txt")
    pb.write_text("---")
    ref_a = db.as_ref(pa)
    ref_b = db.as_ref(pb)
    db.store_indexer_result(ref_a, "file_size",
                            FileSizeIndexerResult(size_bytes=10))
    db.store_indexer_result(ref_b, "file_size",
                            FileSizeIndexerResult(size_bytes=20))
    record = db.get_indexer_result(ref_a.md5, "file_size")
    assert record.result["size_bytes"] == 20


def test_db_store_derivation(db: IndexDatabase) -> None:

    class LocalDerivation(BaseModel, extra="forbid"):
        total_size: int

    key = db.store_derivation(
        input=[FileRef(md5=MD5(md5="MD5"), path=Path("?X"))],
        output_files=["sdf"],
        config={"param": str()},
        return_value=LocalDerivation(total_size=17),
    )

    assert isinstance(key, str)


def test_index_file_job(runtime: IndexRuntime, db: IndexDatabase,
                        sample_file: Path) -> None:
    ref = runtime.db.as_ref(sample_file)
    runtime.run_indexers(ref, ["file_size", "full_text"])
    size_record = db.get_indexer_result(ref.md5, "file_size")
    assert size_record.result["size_bytes"] == sample_file.stat().st_size
    text_record = db.get_indexer_result(ref.md5, "full_text")
    assert text_record.result["text"] == sample_file.read_text()


def test_convert_files_job(runtime: IndexRuntime, sample_file: Path) -> None:
    out = runtime.run_converter("file_size_converter",
                                [runtime.db.as_ref(sample_file)])
    assert out.converter_id == "file_size_converter"


def test_file_summary_indexer_with_flm(
    runtime: IndexRuntime,
    sample_files: list[Path],
) -> None:

    for path in sample_files:
        out = runtime.run_indexers(runtime.db.as_ref(path),
                                   ["file_summary"])["file_summary"]

        summary = out.result.summary.summary
        assert summary.strip() != ""
