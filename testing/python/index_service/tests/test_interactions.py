import os
from pathlib import Path

import pytest
from beartype import beartype
from beartype.typing import Dict
from index_service.db import IndexDatabase
from index_service.orchestrator import (
    run_rpc_subprocess,
    start_resource,
    stop_resource,
)
from index_service.protocol import ConverterOutput, IndexerOutput
from index_service.registry import (
    FILE_REVERSER_MODULE,
    FILE_SIZE_CONVERTER_MODULE,
    FILE_SIZE_MODULE,
    FILE_STATS_MODULE,
    FULL_TEXT_MODULE,
)

ARANGO_HOST = os.environ.get("ARANGO_HOST", "http://localhost:8529")
ARANGO_USER = os.environ.get("ARANGO_USER", "root")
ARANGO_PASSWORD = os.environ.get("ARANGO_ROOT_PASSWORD", "")
ARANGO_DB = "index_test"


@beartype
def _arango_config() -> Dict[str, str]:
    return {
        "host": ARANGO_HOST,
        "db_name": ARANGO_DB,
        "username": ARANGO_USER,
        "password": ARANGO_PASSWORD,
    }


@pytest.fixture
def socket_dir(tmp_path: Path) -> Path:
    path = tmp_path / "sockets"
    path.mkdir()
    return path


@pytest.fixture
def sample_file(tmp_path: Path) -> Path:
    path = tmp_path / "sample.txt"
    path.write_text("alpha\nbeta\ngamma\n")
    return path


@pytest.fixture
def db() -> IndexDatabase:
    database = IndexDatabase(
        host=ARANGO_HOST,
        db_name=ARANGO_DB,
        username=ARANGO_USER,
        password=ARANGO_PASSWORD,
    )
    database.truncate_all()
    return database


def test_file_size_indexer(socket_dir: Path, sample_file: Path) -> None:
    init = {
        "md5": "abc",
        "paths": [str(sample_file)],
        "dependencies": {},
        "available_resources": {},
        "config": {},
    }
    res = run_rpc_subprocess(FILE_SIZE_MODULE, init, socket_dir, 30.0)
    out = IndexerOutput.from_dict(res.result)
    assert out.indexer_id == "file-size"
    assert out.result["size_bytes"] == sample_file.stat().st_size


def test_file_stats_indexer(socket_dir: Path, sample_file: Path) -> None:
    init = {
        "md5": "abc",
        "paths": [str(sample_file)],
        "dependencies": {},
        "available_resources": {},
        "config": {},
    }
    res = run_rpc_subprocess(FILE_STATS_MODULE, init, socket_dir, 30.0)
    out = IndexerOutput.from_dict(res.result)
    assert out.indexer_id == "file-stats"
    assert out.result["size_bytes"] == sample_file.stat().st_size
    assert out.result["mtime"] == sample_file.stat().st_mtime


def test_full_text_indexer_with_reverser(socket_dir: Path,
                                         sample_file: Path) -> None:
    handle = start_resource("file-reverser", FILE_REVERSER_MODULE, socket_dir)
    try:
        init = {
            "md5": "abc",
            "paths": [str(sample_file)],
            "dependencies": {},
            "available_resources": {
                "file-reverser": {
                    "endpoint": handle.endpoint,
                    "status": "ready"
                }
            },
            "config": {},
        }
        res = run_rpc_subprocess(FULL_TEXT_MODULE, init, socket_dir, 30.0)
        out = IndexerOutput.from_dict(res.result)
        text = sample_file.read_text()
        assert out.result["text"] == text
        assert out.result["reversed_lines"] == list(reversed(
            text.splitlines()))
    finally:
        stop_resource(handle)


def test_file_size_converter(socket_dir: Path, sample_file: Path) -> None:
    init = {"input_files": [str(sample_file)], "config": {}}
    res = run_rpc_subprocess(FILE_SIZE_CONVERTER_MODULE, init, socket_dir,
                             30.0)
    out = ConverterOutput.from_dict(res.result)
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


def test_index_file_job(db: IndexDatabase, socket_dir: Path,
                        sample_file: Path) -> None:
    from index_service.dagster_defs import index_file_job

    result = index_file_job.execute_in_process(
        run_config={
            "ops": {
                "provide_file_op": {
                    "config": {
                        "md5": "filemd5",
                        "paths": [str(sample_file)]
                    }
                }
            },
            "resources": {
                "arango": {
                    "config": _arango_config()
                },
                "socket_dir": {
                    "config": {
                        "path": str(socket_dir)
                    }
                },
                "reverser": {
                    "config": {
                        "socket_dir": str(socket_dir)
                    }
                },
            },
        })
    assert result.success
    size_record = db.get_indexer_result("filemd5", "file-size")
    assert size_record.result["size_bytes"] == sample_file.stat().st_size
    text_record = db.get_indexer_result("filemd5", "full-text")
    assert text_record.result["reversed_lines"] == list(
        reversed(sample_file.read_text().splitlines()))


def test_convert_files_job(db: IndexDatabase, socket_dir: Path,
                           sample_file: Path) -> None:
    from index_service.dagster_defs import convert_files_job

    result = convert_files_job.execute_in_process(
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
                },
                "socket_dir": {
                    "config": {
                        "path": str(socket_dir)
                    }
                },
            },
        })
    assert result.success
