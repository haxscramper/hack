import os
from pathlib import Path

import pytest
from index_service.db import IndexDatabase
from index_service.harness import BaseResourceActor
from index_service.protocol import ConverterOutput, FileRef, IndexerOutput
from index_service.resources.flm_gemma import FlmSummaryResult, SummarizeRequest
from index_service.runtime import IndexRuntime
from tests.conftest import RuntimeWithMockFlm

ARANGO_HOST = os.environ.get("ARANGO_HOST", "http://localhost:8529")
ARANGO_USER = os.environ.get("ARANGO_USER", "root")
ARANGO_PASSWORD = os.environ.get("ARANGO_ROOT_PASSWORD", "test")
ARANGO_DB = "index_test"


def _arango_config() -> dict[str, str]:
    return {
        "host": ARANGO_HOST,
        "db_name": ARANGO_DB,
        "username": ARANGO_USER,
        "password": ARANGO_PASSWORD,
    }


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
        FileRef(md5="abc", paths=[str(sample_file)]),
        ["file-size"],
    )["file-size"]

    assert out.indexer_id == "file-size"
    assert out.result["size_bytes"] == sample_file.stat().st_size


def test_file_stats_indexer(runtime: IndexRuntime, sample_file: Path) -> None:
    out = runtime.run_indexers(
        FileRef(md5="abc", paths=[str(sample_file)]),
        ["file-stats"],
    )["file-stats"]

    assert out.indexer_id == "file-stats"
    assert out.result["size_bytes"] == sample_file.stat().st_size
    assert out.result["mtime"] == sample_file.stat().st_mtime


def test_full_text_indexer_with_reverser(runtime: IndexRuntime,
                                         sample_file: Path) -> None:
    out = runtime.run_indexers(
        FileRef(md5="abc", paths=[str(sample_file)]),
        ["full-text"],
    )["full-text"]

    text = sample_file.read_text()
    assert out.result["text"] == text
    assert out.result["reversed_lines"] == list(reversed(text.splitlines()))


def test_file_size_converter(runtime: IndexRuntime, sample_file: Path) -> None:
    out = runtime.run_converter("file-size-converter", [str(sample_file)])
    assert out.converter_id == "file-size-converter"
    assert out.return_value["total_size"] == sample_file.stat().st_size


def test_db_indexer_result_uniqueness(db: IndexDatabase) -> None:
    db.ensure_file("md5x", ["/disk/a.txt"])
    db.ensure_file("md5x", ["/disk/b.txt"])
    db.store_indexer_result("md5x", "file-size", "file-size",
                            {"size_bytes": 10})
    db.store_indexer_result("md5x", "file-size", "file-size",
                            {"size_bytes": 20})
    record = db.get_indexer_result("md5x", "file-size")
    assert record.result["size_bytes"] == 20


def test_db_store_derivation(db: IndexDatabase) -> None:
    key = db.store_derivation(["md5x"], [], {"param": ""}, {"total_size": 17})
    assert isinstance(key, str)


def test_index_file_job(
    db: IndexDatabase,
    sample_file: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    import index_service.dagster_defs as dagster_defs

    monkeypatch.setattr(dagster_defs, "IndexRuntime", RuntimeWithMockFlm)
    result = dagster_defs.index_file_job.execute_in_process(
        run_config={
            "ops": {
                "provide_file_op": {
                    "config": {
                        "md5": "filemd5",
                        "paths": [str(sample_file)],
                    }
                }
            },
            "resources": {
                "arango": {
                    "config": _arango_config()
                }
            },
        })

    assert result.success
    size_record = db.get_indexer_result("filemd5", "file-size")
    assert size_record.result["size_bytes"] == sample_file.stat().st_size
    text_record = db.get_indexer_result("filemd5", "full-text")
    assert text_record.result["reversed_lines"] == list(
        reversed(sample_file.read_text().splitlines()))


def test_convert_files_job(
    db: IndexDatabase,
    sample_file: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    import index_service.dagster_defs as dagster_defs

    monkeypatch.setattr(dagster_defs, "IndexRuntime", RuntimeWithMockFlm)
    result = dagster_defs.convert_files_job.execute_in_process(
        run_config={
            "ops": {
                "file_size_converter_op": {
                    "config": {
                        "input_files": [str(sample_file)],
                        "input_md5s": ["filemd5"],
                        "param": "",
                    }
                }
            },
            "resources": {
                "arango": {
                    "config": _arango_config()
                }
            },
        })
    assert result.success


def test_file_summaries_indexer_with_flm(
    runtime: IndexRuntime,
    sample_files: list[Path],
) -> None:
    out = runtime.run_indexers(
        FileRef(md5="batch-md5", paths=[str(path) for path in sample_files]),
        ["file-summaries"],
    )["file-summaries"]

    assert out.indexer_id == "file-summaries"
    summaries = out.result["summaries"]
    assert len(summaries) == len(sample_files)

    for path in sample_files:
        summary = summaries[str(path)]
        assert isinstance(summary, str)
        assert summary.strip() != ""
