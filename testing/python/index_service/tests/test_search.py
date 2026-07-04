from pathlib import Path

from index_service.services.core.db import IndexDatabase
from index_service.services.core.types import FileRef
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.indexers.full_text import FullTextIndexer


def test_full_text_search(db: IndexDatabase, runtime: IndexRuntime,
                          tmp_path: Path) -> None:
    path = tmp_path / "a.txt"
    path.write_text("alpha beta gamma")

    root = db.add_root("root", tmp_path)
    ref = runtime.db.as_ref(root, path)
    runtime.run_indexer(ref, ["full_text"])
    out = runtime.get_indexer_result(ref, "full_text")
    hits = db.full_text_search("full_text", "beta")
    assert len(hits) == 1
    assert hits[0]["hash"] == ref.hash.hash


def test_vector_search(db: IndexDatabase, runtime: IndexRuntime, tmp_path: Path) -> None:
    a = tmp_path / "a.txt"
    b = tmp_path / "b.txt"
    a.write_text("cat cat cat")
    b.write_text("network protocol packet")

    root = db.add_root("root", tmp_path)
    m1 = db.as_ref(root, a)
    m2 = db.as_ref(root, b)

    runtime.run_indexer(m1, ["file_embedding"])
    out1 = runtime.get_indexer_result(m1, "file_embedding")

    runtime.run_indexer(m2, ["file_embedding"])
    out2 = runtime.get_indexer_result(m2, "file_embedding")

    db.store_indexer_output(m1, out1)
    db.store_indexer_output(m2, out2)

    hits = db.vector_search("file_embedding", out1.result.vector, limit=1)
    assert hits[0]["hash"] == m1.hash.hash
