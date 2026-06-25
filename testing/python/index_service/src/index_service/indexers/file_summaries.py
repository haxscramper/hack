from pathlib import Path

from beartype import beartype
from beartype.typing import Dict
from index_service.harness import RpcContext, RpcHarness
from index_service.protocol import IndexerOutput


@beartype
def handle(payload: Dict[str, object], ctx: RpcContext) -> IndexerOutput:
    summaries: Dict[str, str] = {}

    for raw_path in payload["paths"]:
        path = Path(raw_path)
        text = path.read_text()
        reply = ctx.resource_request("flm-gemma", {"text": text})
        summaries[str(path)] = reply.payload["summary"]

    return IndexerOutput(
        indexer_id="file-summaries",
        result_type="file-summaries",
        result={
            "summaries": summaries,
            "file_count": len(summaries),
        },
    )


if __name__ == "__main__":
    RpcHarness(handle).run()
