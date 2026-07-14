import json
from pathlib import Path

from index_service.services.core.db import IndexDatabase
from index_service.services.core.indexing_flow import run_indexing_per_root_plan
from index_service.services.core.job_types import RunContext
from index_service.services.core.types import FileRef
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.indexers.chunk_indexing.file_embedding import EmbeddingChunk, FileEmbeddingIndexerResult
from index_service.services.indexers.chunk_indexing.full_text import FullTextChunk, FullTextIndexer
from index_service.services.indexers.full_document.full_document_types import Heading
from index_service.services.pydantic_utils import dump_with_type, first_by_field_value
import logging

log = logging.getLogger(__name__)
corpus = Path(__file__).parent.joinpath("corpus")


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

    hits = db.full_text_search_phrase(
        query="beta",
        indexer=runtime.get_indexer(fts_name),  # type: ignore
        wait_for_sync=True,
    )

    assert len(hits) == 1
    hit0 = hits[0][0]
    assert isinstance(hit0, FullTextChunk)
    assert hit0.file_hash == ref.hash.hash


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


def test_corpus_full_text_search(db: IndexDatabase, runtime: IndexRuntime) -> None:
    ctx = RunContext(db)
    idx = runtime.get_indexer("full_text")
    db.enable_index(idx)
    run_indexing_per_root_plan(
        db=db,
        runner=runtime,
        ctx=ctx,
        paths=(corpus,),
        indexers=tuple(v.asset_name for k, v in runtime._indexer_instances.items()),
        limit_total=10,
        limit_per_path=10,
    )

    params = db.get_full_text_search_path(idx)
    docs = list()

    aql = f"""
    FOR doc in {params.view_name}
        SEARCH PHRASE(doc.{params.safe_path}, ["history", 2, "asbestos"], "text_en")
        OPTIONS {{ waitForSync: true }}
        RETURN {{ doc: doc }}
    """

    log.debug(aql)

    for document in db.execute_query_with_conversion(
            aql,
            idx,
        ("doc.result",),
    ):
        docs.append(document)

    doc0 = docs[0][0]
    log.debug(json.dumps(dump_with_type(doc0), indent=2))
    assert isinstance(doc0, FullTextChunk), type(doc0)

    doc_idx = runtime.get_indexer("document_block")

    parent_of = db.get_inbound(doc0.spans[0].source_hash, doc_idx)
    assert len(parent_of) == 1

    graph = db.render_indexer_graphviz(doc_idx,
                                       max_outgoing_edges=5,
                                       start_vertex_ids=set(parent_of))
    graph.attr("graph", rankdir="LR")
    graph.render("/tmp/full_asbestos", format="png")
    graph.render("/tmp/full_asbestos", format="svg")

    parent_chunk = db.get_indexer_one_document(parent_of[0], doc_idx)
    assert isinstance(parent_chunk, Heading), type(parent_chunk)

    log.debug(len(docs))

    # assert False
