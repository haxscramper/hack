import json
from pathlib import Path

from index_service.services.core.db import IndexDatabase
from index_service.services.core.types import FileRef
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.indexers.chunk_indexing.file_embedding import EmbeddingChunk, FileEmbeddingIndexerResult
from index_service.services.indexers.chunk_indexing.full_text import FullTextIndexer
from index_service.services.pydantic_utils import dump_with_type, first_by_field_value
import logging

log = logging.getLogger(__name__)


def test_full_text_search(db: IndexDatabase, runtime: IndexRuntime,
                          tmp_path: Path) -> None:
    path = tmp_path / "a.txt"
    path.write_text("alpha beta gamma")

    root = db.add_root("root", tmp_path)
    ref = runtime.db.as_ref(root, path)
    fts_name = FullTextIndexer.asset_name
    runtime.run_indexer(ref, [fts_name])
    out = runtime.get_indexer_result(ref, fts_name)

    db.enable_index(runtime.get_indexer(fts_name))  # type: ignore

    hits = db.full_text_search(
        fts_name,
        "beta",
        indexer=runtime.get_indexer(fts_name),  # type: ignore
    )

    assert len(hits) == 1
    assert hits[0]["hash"] == ref.hash.hash


def test_vector_search(db: IndexDatabase, runtime: IndexRuntime, tmp_path: Path) -> None:
    a = tmp_path / "a.txt"
    b = tmp_path / "b.txt"
    a.write_text("cat cat cat")
    b.write_text("network protocol packet")

    root = db.add_root("root", tmp_path)
    file_1 = db.as_ref(root, a)
    file_2 = db.as_ref(root, b)

    emb_name = "file_embedding"

    runtime.run_indexer(file_1, [emb_name])
    out1 = runtime.get_indexer_result(file_1, emb_name)

    runtime.run_indexer(file_2, [emb_name])
    out2 = runtime.get_indexer_result(file_2, emb_name)

    db.wait_indexing()

    assert isinstance(out1.result, FileEmbeddingIndexerResult)
    assert isinstance(out2.result, FileEmbeddingIndexerResult)

    chunk1 = first_by_field_value(out1.result.documents, "type", "chunk")
    assert isinstance(chunk1, EmbeddingChunk)
    assert 0 < len(chunk1.vector)

    db.enable_index(runtime.get_indexer(emb_name))  # type: ignore
    db.wait_indexing()

    hits = db.vector_search(
        emb_name,
        chunk1.vector,
        limit=1,
        indexer=runtime.get_indexer(emb_name),  #type: ignore
    )

    assert len(hits) == 1
    hit0 = hits[0][0]
    assert isinstance(hit0, EmbeddingChunk)
    assert hit0.file_hash == file_1.hash.hash
