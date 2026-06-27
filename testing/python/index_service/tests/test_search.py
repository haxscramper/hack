from pathlib import Path

from index_service.protocol import FileRef
from index_service.runtime import IndexRuntime


def test_full_text_search(db, tmp_path: Path) -> None:
    path = tmp_path / "a.txt"
    path.write_text("alpha beta gamma")

    m1 = db.get_md5(path)
    runtime = IndexRuntime()
    try:
        out = runtime.run_indexers(
            FileRef(md5=m1, path=path),
            ["full_text"],
        )["full_text"]
        db.store_indexer_result(m1, out.indexer_id, out.result)
    finally:
        runtime.stop()

    hits = db.full_text_search("beta")
    assert len(hits) == 1
    assert hits[0]["md5"] == m1


def test_vector_search(db, tmp_path: Path) -> None:
    a = tmp_path / "a.txt"
    b = tmp_path / "b.txt"
    a.write_text("cat cat cat")
    b.write_text("network protocol packet")

    m1 = db.get_md5(a)
    m2 = db.get_md5(b)

    runtime = IndexRuntime()
    try:
        out1 = runtime.run_indexers(
            FileRef(md5=m1, path=a),
            ["file_embedding"],
        )["file_embedding"]
        out2 = runtime.run_indexers(
            FileRef(md5=m2, path=b),
            ["file_embedding"],
        )["file_embedding"]
        db.store_indexer_result(m1, out1.indexer_id, out1.result)
        db.store_indexer_result(m2, out2.indexer_id, out2.result)
    finally:
        runtime.stop()

    hits = db.vector_search(out1.result.vector, limit=1)
    assert hits[0]["md5"] == m1
