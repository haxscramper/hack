import enum
import hashlib
import json
import logging
import math
from collections import defaultdict
from dataclasses import dataclass, field
from typing import Any, Callable, ClassVar, Literal, Optional, Union

import semchunk
import tiktoken
from pydantic import BaseModel, Field

from index_service.services.core.types import (
    IndexEdge,
    IndexMultiDocument,
)

log = logging.getLogger(__name__)


# --------------------------------------------------------------------------- #
# Configuration
# --------------------------------------------------------------------------- #
class ChunkUnit(str, enum.Enum):
    CHARS = "chars"
    TOKENS = "tokens"


class ChunkConfig(BaseModel, extra="forbid"):
    """Configuration for both flat-text and document-block chunking.

    All size / overlap numbers are expressed in `unit` (characters or tokens).
    Overlap for the start and the end of a chunk are configured independently;
    set both min and max to 0 to disable the respective side.
    """

    unit: ChunkUnit = ChunkUnit.CHARS
    max_size: int = 2000
    min_size: int | None = None
    start_overlap_min: int = 0
    start_overlap_max: int = 0
    end_overlap_min: int = 0
    end_overlap_max: int = 0
    encoding_name: str = "o200k_base"


# --------------------------------------------------------------------------- #
# Chunk schema (pydantic objects used inside indexer result models)
# --------------------------------------------------------------------------- #
class SourceSpan(BaseModel, extra="forbid"):
    """Maps a contiguous range of chunk text back to its origin.

    `source_hash` is the hash of the originating document block, or `None` when
    the chunk was produced from flat text. `source_start`/`source_end` address
    the original source, `chunk_start`/`chunk_end` address this chunk's text.
    """

    source_hash: str | None = None
    source_start: int
    source_end: int
    chunk_start: int
    chunk_end: int
    kind: Literal["core", "start_overlap", "end_overlap"] = "core"


class ChunkFile(IndexMultiDocument, extra="forbid"):
    """Anchor document representing the whole file that was chunked."""

    type: Literal["chunkFile"] = "chunkFile"


class ChunkDocument(IndexMultiDocument, extra="forbid"):
    """A single chunk. Subclass to attach per-chunk payloads (vector, summary)."""

    type: Literal["chunk"] = "chunk"
    index: int
    text: str
    char_count: int
    token_count: int
    unit: str
    spans: list[SourceSpan] = Field(default_factory=list)


class ChunkLink(IndexEdge, extra="forbid"):
    order: int
    relation: Literal["chunk"] = "chunk"


# --------------------------------------------------------------------------- #
# Internal working structures
# --------------------------------------------------------------------------- #
@dataclass
class RawChunk:
    index: int
    text: str
    hash: str
    char_count: int
    token_count: int
    unit: str
    spans: list[SourceSpan]


@dataclass
class _Unit:
    hash: str
    text: str
    size: int
    barrier: bool = False
    no_merge: bool = False
    src_start: int = 0


def chunk_hash(file_hash: str, index: int, text: str, spans: list[SourceSpan]) -> str:
    hasher = hashlib.sha256()
    payload = {
        "file_hash": file_hash,
        "index": index,
        "text": text,
        "spans": [s.model_dump() for s in spans],
    }
    hasher.update(json.dumps(payload, sort_keys=True, ensure_ascii=False).encode("utf-8"))
    return hasher.hexdigest()


# --------------------------------------------------------------------------- #
# Chunker
# --------------------------------------------------------------------------- #
class Chunker:

    def __init__(self, config: ChunkConfig) -> None:
        self.cfg = config
        self._enc = tiktoken.get_encoding(config.encoding_name)
        if config.unit == ChunkUnit.TOKENS:
            self._counter: Callable[[str], int] = lambda t: len(self._enc.encode(t))
        else:
            self._counter = len
        self._chunker = semchunk.chunkerify(self._counter, config.max_size)

    # -- helpers ----------------------------------------------------------- #
    def _size(self, text: str) -> int:
        return self._counter(text)

    def _raw(self, index: int, text: str, spans: list[SourceSpan],
             file_hash: str) -> RawChunk:
        return RawChunk(
            index=index,
            text=text,
            hash=chunk_hash(file_hash, index, text, spans),
            char_count=len(text),
            token_count=len(self._enc.encode(text)),
            unit=self.cfg.unit.value,
            spans=spans,
        )

    def _left_overlap(self, text: str, pos: int) -> int:
        omax, omin = self.cfg.start_overlap_max, self.cfg.start_overlap_min
        if omax <= 0 or pos <= 0:
            return 0
        if self.cfg.unit == ChunkUnit.CHARS:
            n = min(omax, pos)
            return n if n >= omin else 0
        toks = self._enc.encode(text[:pos])
        take = min(omax, len(toks))
        if take < omin:
            return 0
        return len(self._enc.decode(toks[len(toks) - take:]))

    def _right_overlap(self, text: str, pos: int) -> int:
        omax, omin = self.cfg.end_overlap_max, self.cfg.end_overlap_min
        if omax <= 0 or pos >= len(text):
            return 0
        if self.cfg.unit == ChunkUnit.CHARS:
            n = min(omax, len(text) - pos)
            return n if n >= omin else 0
        toks = self._enc.encode(text[pos:])
        take = min(omax, len(toks))
        if take < omin:
            return 0
        return len(self._enc.decode(toks[:take]))

    # -- flat text --------------------------------------------------------- #
    def chunk_text(self, text: str, file_hash: str) -> list[RawChunk]:
        if not text:
            return []
        _, offsets = self._chunker(text, offsets=True)
        results: list[RawChunk] = []
        for i, (s, e) in enumerate(offsets):
            lo = self._left_overlap(text, s)
            ro = self._right_overlap(text, e)
            start, end = s - lo, e + ro
            ctext = text[start:end]
            spans: list[SourceSpan] = []
            if lo:
                spans.append(
                    SourceSpan(source_start=start,
                               source_end=s,
                               chunk_start=0,
                               chunk_end=lo,
                               kind="start_overlap"))
            spans.append(
                SourceSpan(source_start=s,
                           source_end=e,
                           chunk_start=lo,
                           chunk_end=lo + (e - s),
                           kind="core"))
            if ro:
                spans.append(
                    SourceSpan(source_start=e,
                               source_end=end,
                               chunk_start=lo + (e - s),
                               chunk_end=len(ctext),
                               kind="end_overlap"))
            results.append(self._raw(i, ctext, spans, file_hash))
        return results

    # -- document blocks --------------------------------------------------- #
    def chunk_blocks(self, documents, edges, file_hash: str) -> list[RawChunk]:
        units = self._render_units(documents, edges)
        if not units:
            return []
        groups = self._partition_units(units)
        sep = "\n\n"
        results: list[RawChunk] = []
        for i, group in enumerate(groups):
            spans: list[SourceSpan] = []
            parts_text = ""

            def add(u: _Unit, kind: str) -> None:
                nonlocal parts_text
                if parts_text:
                    parts_text += sep
                start = len(parts_text)
                parts_text += u.text
                spans.append(
                    SourceSpan(
                        source_hash=u.hash,
                        source_start=u.src_start,
                        source_end=u.src_start + len(u.text),
                        chunk_start=start,
                        chunk_end=len(parts_text),
                        kind=kind,
                    ))

            if i > 0:
                for u in self._overlap_prev(groups[i - 1], units):
                    add(u, "start_overlap")
            for idx in group:
                add(units[idx], "core")
            if i < len(groups) - 1:
                for u in self._overlap_next(groups[i + 1], units):
                    add(u, "end_overlap")

            results.append(self._raw(i, parts_text, spans, file_hash))
        return results

    # -- partitioning ------------------------------------------------------ #
    def _partition_units(self, units: list[_Unit]) -> list[list[int]]:
        total = sum(u.size for u in units)
        maxs = self.cfg.max_size
        mins = self.cfg.min_size or 0
        n = max(1, math.ceil(total / maxs))
        target = total / n

        groups: list[list[int]] = []
        cur: list[int] = []
        cur_size = 0
        for i, u in enumerate(units):
            if u.no_merge:
                if cur:
                    groups.append(cur)
                    cur, cur_size = [], 0
                groups.append([i])
                continue
            close = False
            if cur:
                if cur_size + u.size > maxs:
                    close = True
                elif u.barrier and cur_size >= mins:
                    close = True
                elif cur_size >= target and cur_size >= mins:
                    close = True
            if close:
                groups.append(cur)
                cur, cur_size = [], 0
            cur.append(i)
            cur_size += u.size
        if cur:
            groups.append(cur)
        return groups

    def _overlap_prev(self, prev_group: list[int], units: list[_Unit]) -> list[_Unit]:
        omax, omin = self.cfg.start_overlap_max, self.cfg.start_overlap_min
        if omax <= 0:
            return []
        picked: list[_Unit] = []
        size = 0
        for idx in reversed(prev_group):
            u = units[idx]
            if u.no_merge or size + u.size > omax:
                break
            picked.append(u)
            size += u.size
        picked.reverse()
        return picked if size >= omin else []

    def _overlap_next(self, next_group: list[int], units: list[_Unit]) -> list[_Unit]:
        omax, omin = self.cfg.end_overlap_max, self.cfg.end_overlap_min
        if omax <= 0:
            return []
        picked: list[_Unit] = []
        size = 0
        for idx in next_group:
            u = units[idx]
            if u.no_merge or size + u.size > omax:
                break
            picked.append(u)
            size += u.size
        return picked if size >= omin else []

    # -- rendering --------------------------------------------------------- #
    def _render_units(self, documents, edges) -> list[_Unit]:
        by_hash = {d.hash: d for d in documents}
        children: dict[str, list[tuple[int, str]]] = defaultdict(list)
        has_parent: set[str] = set()
        for e in edges:
            children[e.from_].append((getattr(e, "order", 0), e.to_))
            has_parent.add(e.to_)
        roots = [d for d in documents if d.hash not in has_parent]

        units: list[_Unit] = []

        def walk(h: str, depth: int) -> None:
            block = by_hash.get(h)
            if block is None:
                return
            btype = getattr(block, "type", None)
            if btype == "table":
                self._emit(units,
                           block.hash,
                           self._render_table(block, children, by_hash),
                           barrier=True)
                return  # table rendered whole; do not descend into rows
            own = self._render_own(block, depth)
            if own:
                self._emit(units, block.hash, own, barrier=(btype == "heading"))
            for _, child in sorted(children.get(h, [])):
                walk(child, depth + 1)

        for r in roots:
            walk(r.hash, 0)
        return units

    def _emit(self,
              units: list[_Unit],
              hash_: str,
              text: str,
              barrier: bool = False) -> None:
        if not text:
            return
        size = self._size(text)
        if size <= self.cfg.max_size:
            units.append(_Unit(hash=hash_, text=text, size=size, barrier=barrier))
            return
        # Oversized single block: only split when unavoidable.
        _, offsets = self._chunker(text, offsets=True)
        for s, e in offsets:
            piece = text[s:e]
            units.append(
                _Unit(hash=hash_,
                      text=piece,
                      size=self._size(piece),
                      barrier=barrier,
                      no_merge=True,
                      src_start=s))

    def _inline(self, content) -> str:
        if not content:
            return ""
        parts: list[str] = []
        for node in content:
            if getattr(node, "type", None) == "link":
                parts.append(
                    f"[{self._inline(getattr(node, 'content', []))}]({node.href})")
                continue
            text = getattr(node, "text", "")
            styles = getattr(node, "styles", None)
            markup = set()
            if styles is not None:
                markup = {getattr(m, "value", m) for m in styles.markup}
            if "code" in markup:
                text = f"`{text}`"
            if "bold" in markup:
                text = f"**{text}**"
            if "italic" in markup:
                text = f"*{text}*"
            if "strike" in markup:
                text = f"~~{text}~~"
            parts.append(text)
        return "".join(parts)

    def _render_own(self, block, depth: int) -> str | None:
        btype = getattr(block, "type", None)
        if btype in ("document", "file", "div", "tableRow", "tableCell"):
            return None
        content = getattr(block, "content", None)
        if btype == "heading":
            level = getattr(getattr(block, "props", None), "level", 1)
            return "#" * level + " " + self._inline(content)
        if btype == "paragraph":
            return self._inline(content)
        if btype == "quote":
            return "> " + self._inline(content)
        if btype == "codeBlock":
            lang = getattr(getattr(block, "props", None), "language", "")
            return f"```{lang}\n{self._inline(content)}\n```"
        if btype == "math":
            return f"$$\n{self._inline(content)}\n$$"
        if btype == "rawBlock":
            return getattr(block, "content", "") or ""
        if btype == "bulletListItem":
            return "  " * depth + "- " + self._inline(content)
        if btype == "numberedListItem":
            return "  " * depth + "1. " + self._inline(content)
        if content is not None:
            return self._inline(content)
        return None

    def _render_table(self, table, children, by_hash) -> str:
        rows: list[list[str]] = []
        for _, row_h in sorted(children.get(table.hash, [])):
            cells: list[str] = []
            for _, cell_h in sorted(children.get(row_h, [])):
                cell = by_hash.get(cell_h)
                cells.append(self._inline(getattr(cell, "content", [])))
            rows.append(cells)
        if not rows:
            return ""
        header = rows[0]
        lines = [
            "| " + " | ".join(header) + " |",
            "| " + " | ".join("---" for _ in header) + " |"
        ]
        for r in rows[1:]:
            lines.append("| " + " | ".join(r) + " |")
        return "\n".join(lines)


# --------------------------------------------------------------------------- #
# MultiDocumentModel assembly helper
# --------------------------------------------------------------------------- #
def chunks_to_multidoc(
    chunks: list[RawChunk],
    file_hash: str,
    chunk_cls: type[ChunkDocument],
    file_cls: type[ChunkFile] = ChunkFile,
    link_cls: type[ChunkLink] = ChunkLink,
    per_chunk: Optional[Callable[[RawChunk], dict]] = None,
    file_kwargs: Optional[dict] = None,
):
    per_chunk = per_chunk or (lambda c: {})
    file_doc = file_cls(hash=file_hash, file_hash=file_hash, **(file_kwargs or {}))
    documents: list = [file_doc]
    edges: list = []
    for c in chunks:
        documents.append(
            chunk_cls(
                hash=c.hash,
                file_hash=file_hash,
                index=c.index,
                text=c.text,
                char_count=c.char_count,
                token_count=c.token_count,
                unit=c.unit,
                spans=c.spans,
                **per_chunk(c),
            ))
        edges.append(
            link_cls(file_hash=file_hash, from_=file_hash, to_=c.hash, order=c.index))
    return documents, edges
