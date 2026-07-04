from index_service.services.indexers.chunk_indexing.chunking import (
    ChunkConfig,
    ChunkUnit,
    Chunker,
    ChunkDocument,
    chunks_to_multidoc,
)

import index_service.services.indexers.full_document.full_document_types as fd


def _build_docs(blocks, file_hash):
    """Wrap blocks in a Document/File and flatten to (documents, edges)."""
    doc = fd.build(fd.Document, file_hash=file_hash, nested=list(blocks))
    documents = [fd.File(hash=file_hash, file_hash=file_hash)]
    edges: list = []
    fd._flatten(doc, documents, edges, parent_hash=file_hash, order=0)
    return documents, edges


def _para(text, file_hash):
    return fd.build(fd.Paragraph, file_hash=file_hash, content=[fd.StyledText(text=text)])


def _heading(text, file_hash, level=1):
    return fd.build(fd.Heading,
                    file_hash=file_hash,
                    props=fd.HeadingProps(level=level),
                    content=[fd.StyledText(text=text)])


# --------------------------------------------------------------------------- #
# Flat text
# --------------------------------------------------------------------------- #
def test_flat_text_splits_and_overlaps():
    cfg = ChunkConfig(unit=ChunkUnit.CHARS,
                      max_size=100,
                      min_size=20,
                      start_overlap_min=1,
                      start_overlap_max=10)
    chunker = Chunker(cfg)
    text = "".join(f"sentence number {i}. " for i in range(80))
    chunks = chunker.chunk_text(text, "filehash")

    assert len(chunks) > 1
    assert len({c.hash for c in chunks}) == len(chunks)
    for c in chunks:
        assert c.char_count == len(c.text)
        assert c.spans
        # core span reconstructs its slice of the original text
        core = next(s for s in c.spans if s.kind == "core")
        assert text[core.source_start:core.source_end] == \
            c.text[core.chunk_start:core.chunk_end]
    assert any(any(s.kind == "start_overlap" for s in c.spans) for c in chunks[1:])


def test_flat_text_single_chunk():
    cfg = ChunkConfig(unit=ChunkUnit.CHARS, max_size=10_000)
    chunks = Chunker(cfg).chunk_text("short text", "fh")
    assert len(chunks) == 1
    assert chunks[0].text == "short text"


# --------------------------------------------------------------------------- #
# Document blocks
# --------------------------------------------------------------------------- #
def test_blocks_merge_multiple_sources_into_one_chunk():
    fh = "fh"
    blocks = [_para(f"paragraph body {i}", fh) for i in range(6)]
    documents, edges = _build_docs(blocks, fh)

    cfg = ChunkConfig(unit=ChunkUnit.CHARS, max_size=200, min_size=0)
    chunks = Chunker(cfg).chunk_blocks(documents, edges, fh)

    assert chunks
    assert any(len({s.source_hash for s in c.spans}) > 1 for c in chunks)
    # nesting reconstructed purely from edges (nested fields are empty)
    assert all(d.nested == [] for d in documents if hasattr(d, "nested"))


def test_blocks_respect_heading_barrier():
    fh = "fh"
    blocks = [
        _heading("Section A", fh),
        _para("aaaa " * 10, fh),
        _heading("Section B", fh),
        _para("bbbb " * 10, fh),
    ]
    documents, edges = _build_docs(blocks, fh)

    cfg = ChunkConfig(unit=ChunkUnit.CHARS, max_size=120, min_size=0)
    chunks = Chunker(cfg).chunk_blocks(documents, edges, fh)
    assert len(chunks) >= 2


def test_oversized_block_is_split():
    fh = "fh"
    big = _para("word " * 400, fh)  # ~2000 chars, exceeds max
    documents, edges = _build_docs([big], fh)

    cfg = ChunkConfig(unit=ChunkUnit.CHARS, max_size=300)
    chunks = Chunker(cfg).chunk_blocks(documents, edges, fh)

    assert len(chunks) > 1
    # every span points back to the oversized paragraph
    hashes = {s.source_hash for c in chunks for s in c.spans}
    assert hashes == {big.hash}


def test_chunks_to_multidoc_shape():
    fh = "fh"
    documents, edges = _build_docs([_para(f"p{i}", fh) for i in range(4)], fh)
    chunks = Chunker(ChunkConfig(unit=ChunkUnit.CHARS,
                                 max_size=50)).chunk_blocks(documents, edges, fh)

    docs, links = chunks_to_multidoc(chunks, fh, ChunkDocument)
    assert docs[0].hash == fh  # file anchor
    assert docs[0].type == "chunkFile"
    assert len(docs) == len(chunks) + 1
    assert len(links) == len(chunks)
    assert all(l.from_ == fh for l in links)
