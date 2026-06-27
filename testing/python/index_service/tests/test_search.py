from pathlib import Path

from index_service.protocol import FileRef
from index_service.runtime import IndexRuntime


def test_full_text_search(db, tmp_path: Path) -> None:
    path = tmp_path / "a.txt"
    path.write_text("alpha beta gamma")

    db.ensure_file("m1", [str(path)])
    runtime = IndexRuntime()
    try:
        out = runtime.run_indexers(
            FileRef(md5="m1", paths=[str(path)]),
            ["full-text"],
        )["full-text"]
        db.store_indexer_result("m1", out.indexer_id, out.result)
    finally:
        runtime.stop()

    hits = db.full_text_search("beta")
    assert len(hits) == 1
    assert hits[0]["md5"] == "m1"


def test_vector_search(db, tmp_path: Path) -> None:
    a = tmp_path / "a.txt"
    b = tmp_path / "b.txt"
    a.write_text("cat cat cat")
    b.write_text("network protocol packet")

    db.ensure_file("m1", [str(a)])
    db.ensure_file("m2", [str(b)])

    runtime = IndexRuntime()
    try:
        out1 = runtime.run_indexers(
            FileRef(md5="m1", paths=[str(a)]),
            ["file-embedding"],
        )["file-embedding"]
        out2 = runtime.run_indexers(
            FileRef(md5="m2", paths=[str(b)]),
            ["file-embedding"],
        )["file-embedding"]
        db.store_indexer_result("m1", out1.indexer_id, out1.result)
        db.store_indexer_result("m2", out2.indexer_id, out2.result)
    finally:
        runtime.stop()

    hits = db.vector_search(out1.result.vector, limit=1)
    assert hits[0]["md5"] == "m1"
