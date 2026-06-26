from pathlib import Path

from index_service.harness import BaseIndexerActor
from index_service.protocol import IndexerOutput, IndexerRequest
from index_service.resources.file_reverser import ReverseLinesRequest, ReverserResult


class FullTextIndexerActor(BaseIndexerActor):
    actor_id = "full-text"
    required_resources = ("file-reverser", )

    def handle(self, request: IndexerRequest) -> IndexerOutput:
        path = Path(request.file_ref.paths[0])
        text = path.read_text()
        reverse_result = self.request_resource(
            "file-reverser",
            ReverseLinesRequest(lines=text.splitlines()),
            ReverserResult,
        )
        return IndexerOutput(
            indexer_id=self.actor_id,
            result_type=self.actor_id,
            result={
                "text": text,
                "reversed_lines": reverse_result.lines
            },
        )
