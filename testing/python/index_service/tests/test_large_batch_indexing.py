from pathlib import Path

from index_service.protocol import FileRef
from index_service.runtime import IndexRuntime


def test_large_batch_indexing(db, tmp_path: Path,
                              runtime: IndexRuntime) -> None:
    for idx in range(200):
        path = tmp_path / f"doc_{idx}.txt"
        path.write_text(f"document {idx}\nalpha beta\n")
        md5 = f"md5_{idx}"
        db.ensure_file(md5, [str(path)])

        outputs = runtime.run_indexers(
            FileRef(md5=md5, paths=[str(path)]),
            [
                "file-size",
                "file-stats",
                "full-text",
                "file-summaries",
                "file-embedding",
            ],
        )
        for out in outputs.values():
            db.store_indexer_result(md5, out.indexer_id, out.result_type,
                                    out.result)

    record = db.get_indexer_result("md5_42", "full-text")
    assert "alpha beta" in record.result["text"]
