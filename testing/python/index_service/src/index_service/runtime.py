import math
from pathlib import Path

import pykka
from beartype import beartype
from beartype.typing import Dict, List

from index_service.contracts import BaseIndexer, FileRef, IndexerContext
from index_service.protocol import ConverterOutput, IndexerOutput


class FileReverserActor(pykka.ThreadingActor):

    def reverse_lines(self, lines: List[str]) -> List[str]:
        return list(reversed(lines))


class MockSummaryActor(pykka.ThreadingActor):

    def summarize(self, text: str) -> str:
        words = text.split()
        head = " ".join(words[:20]).strip()
        if head == "":
            return "empty file"
        return head


class MockEmbeddingActor(pykka.ThreadingActor):

    def embed(self, text: str) -> List[float]:
        buckets = [0.0] * 8
        for ch in text.lower():
            buckets[ord(ch) % 8] += 1.0
        norm = math.sqrt(sum(x * x for x in buckets))
        if norm == 0.0:
            return buckets
        return [x / norm for x in buckets]


@beartype
class FileSizeIndexer(BaseIndexer):
    indexer_id = "file-size"

    def run(self, file_ref: FileRef, ctx: IndexerContext) -> IndexerOutput:
        size = Path(file_ref.paths[0]).stat().st_size
        return IndexerOutput(
            indexer_id=self.indexer_id,
            result_type=self.indexer_id,
            result={"size_bytes": size},
        )


@beartype
class FileStatsIndexer(BaseIndexer):
    indexer_id = "file-stats"

    def run(self, file_ref: FileRef, ctx: IndexerContext) -> IndexerOutput:
        st = Path(file_ref.paths[0]).stat()
        return IndexerOutput(
            indexer_id=self.indexer_id,
            result_type=self.indexer_id,
            result={
                "size_bytes": st.st_size,
                "mode": st.st_mode,
                "mtime": st.st_mtime,
                "ctime": st.st_ctime,
            },
        )


@beartype
class FullTextIndexer(BaseIndexer):
    indexer_id = "full-text"

    def __init__(self, reverser_proxy) -> None:
        self._reverser = reverser_proxy

    def run(self, file_ref: FileRef, ctx: IndexerContext) -> IndexerOutput:
        text = Path(file_ref.paths[0]).read_text()
        reversed_lines = self._reverser.reverse_lines(text.splitlines()).get()
        return IndexerOutput(
            indexer_id=self.indexer_id,
            result_type=self.indexer_id,
            result={
                "text": text,
                "reversed_lines": reversed_lines
            },
        )


@beartype
class FileSummariesIndexer(BaseIndexer):
    indexer_id = "file-summaries"

    def __init__(self, summary_proxy) -> None:
        self._summary = summary_proxy

    def run(self, file_ref: FileRef, ctx: IndexerContext) -> IndexerOutput:
        summaries: Dict[str, str] = {}
        for raw_path in file_ref.paths:
            text = Path(raw_path).read_text()
            summaries[str(raw_path)] = self._summary.summarize(text).get()
        return IndexerOutput(
            indexer_id=self.indexer_id,
            result_type=self.indexer_id,
            result={
                "summaries": summaries,
                "file_count": len(summaries)
            },
        )


@beartype
class FileEmbeddingIndexer(BaseIndexer):
    indexer_id = "file-embedding"

    def __init__(self, embedding_proxy) -> None:
        self._embedding = embedding_proxy

    def run(self, file_ref: FileRef, ctx: IndexerContext) -> IndexerOutput:
        text = Path(file_ref.paths[0]).read_text()
        vector = self._embedding.embed(text).get()
        return IndexerOutput(
            indexer_id=self.indexer_id,
            result_type=self.indexer_id,
            result={
                "vector": vector,
                "dim": len(vector)
            },
        )


@beartype
class IndexRuntime:

    def __init__(self) -> None:
        self._reverser_ref = FileReverserActor.start()
        self._summary_ref = MockSummaryActor.start()
        self._embedding_ref = MockEmbeddingActor.start()

        self._indexers: Dict[str, BaseIndexer] = {
            "file-size": FileSizeIndexer(),
            "file-stats": FileStatsIndexer(),
            "full-text": FullTextIndexer(self._reverser_ref.proxy()),
            "file-summaries": FileSummariesIndexer(self._summary_ref.proxy()),
            "file-embedding":
            FileEmbeddingIndexer(self._embedding_ref.proxy()),
        }

    def stop(self) -> None:
        self._embedding_ref.stop()
        self._summary_ref.stop()
        self._reverser_ref.stop()

    def run_indexers(
        self,
        file_ref: FileRef,
        names: List[str],
    ) -> Dict[str, IndexerOutput]:
        done: Dict[str, IndexerOutput] = {}
        for name in names:
            out = self._indexers[name].run(
                file_ref,
                IndexerContext(dependency_results=done),
            )
            done[name] = out
        return done

    def run_converter(
        self,
        converter_name: str,
        input_files: List[str],
        param: str = "",
    ) -> ConverterOutput:
        del param
        if converter_name != "file-size-converter":
            raise KeyError(converter_name)

        sizes = {path: Path(path).stat().st_size for path in input_files}
        total = sum(sizes.values())

        return ConverterOutput(
            converter_id="file-size-converter",
            output_files=[],
            return_value={
                "sizes": sizes,
                "total_size": total,
            },
        )
