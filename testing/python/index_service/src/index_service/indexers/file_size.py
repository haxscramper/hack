from pathlib import Path

from beartype import beartype
from beartype.typing import Dict
from index_service.harness import RpcContext, RpcHarness
from index_service.protocol import IndexerOutput


@beartype
def handle(payload: Dict[str, object], ctx: RpcContext) -> IndexerOutput:
    path = Path(payload["paths"][0])
    size = path.stat().st_size
    return IndexerOutput(
        indexer_id="file-size",
        result_type="file-size",
        result={"size_bytes": size},
    )


if __name__ == "__main__":
    RpcHarness(handle).run()
