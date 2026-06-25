from pathlib import Path

from beartype import beartype
from beartype.typing import Dict
from index_service.harness import RpcContext, RpcHarness
from index_service.protocol import IndexerOutput


@beartype
def handle(payload: Dict[str, object], ctx: RpcContext) -> IndexerOutput:
    path = Path(payload["paths"][0])
    text = path.read_text()
    lines = text.splitlines()
    reply = ctx.resource_request("file-reverser", {"lines": lines})
    reversed_lines = reply.payload["lines"]
    return IndexerOutput(
        indexer_id="full-text",
        result_type="full-text",
        result={
            "text": text,
            "reversed_lines": reversed_lines
        },
    )


if __name__ == "__main__":
    RpcHarness(handle).run()
