from pathlib import Path

from index_service.services.core.db import IndexDatabase
from index_service.services.indexers.full_text import FullTextIndexerResult
from index_service.services.core.types import FileRef
from index_service.services.core.job_runtime import IndexRuntime


def test_large_batch_indexing(db: IndexDatabase, tmp_path: Path,
                              runtime: IndexRuntime) -> None:

    root = db.add_root("root", tmp_path)
    for idx in range(20):
        path = tmp_path / f"doc_{idx}.txt"
        path.write_text(f"document {idx}\nalpha beta\n")
        ref = db.as_ref(root, path)

        runtime.run_indexer(
            ref,
            [
                "file_size",
                "file_stats",
                "full_text",
                "file_summary",
                "file_embedding",
            ],
        )

    record = runtime.get_indexer_result(
        db.as_ref(root, tmp_path.joinpath("doc_0.txt")),
        "full_text",
    )

    assert isinstance(record.result, FullTextIndexerResult)

    assert "alpha beta" in record.result.text
