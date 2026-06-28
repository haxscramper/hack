from pathlib import Path

from index_service.services.db import IndexDatabase
from index_service.services.types import FileRef
from index_service.services.job_runtime import IndexRuntime


def test_large_batch_indexing(db: IndexDatabase, tmp_path: Path,
                              runtime: IndexRuntime) -> None:

    root = db.add_root("root", tmp_path)
    for idx in range(20):
        path = tmp_path / f"doc_{idx}.txt"
        path.write_text(f"document {idx}\nalpha beta\n")
        ref = db.as_ref(root, path)

        outputs = runtime.run_indexer(
            ref,
            [
                "file_size",
                "file_stats",
                "full_text",
                "file_summary",
                "file_embedding",
            ],
        )
        for out in outputs.values():
            db.store_indexer_result(ref, out.indexer_id, out.result)

    record = db.get_indexer_result(
        db.as_ref(root, tmp_path.joinpath("doc_0.txt")).md5,
        "full_text",
    )

    assert "alpha beta" in record.result["text"]
