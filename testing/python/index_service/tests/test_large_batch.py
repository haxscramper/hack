from pathlib import Path

from index_service.services.protocol import FileRef
from index_service.services.runtime import IndexRuntime


def test_large_batch_indexing(db, tmp_path: Path,
                              runtime: IndexRuntime) -> None:
    for idx in range(20):
        path = tmp_path / f"doc_{idx}.txt"
        path.write_text(f"document {idx}\nalpha beta\n")
        md5 = db.get_md5(path)

        outputs = runtime.run_indexers(
            FileRef(md5=md5, path=path),
            [
                "file_size",
                "file_stats",
                "full_text",
                "file_summary",
                "file_embedding",
            ],
        )
        for out in outputs.values():
            db.store_indexer_result(md5, out.indexer_id, out.result)

    record = db.get_indexer_result(db._md5(tmp_path.joinpath("doc_0.txt")),
                                   "full_text")
    assert "alpha beta" in record.result["text"]
