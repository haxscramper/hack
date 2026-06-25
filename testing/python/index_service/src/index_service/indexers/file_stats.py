from datetime import datetime
from pathlib import Path

from beartype import beartype
from beartype.typing import Dict
from index_service.harness import RpcContext, RpcHarness
from index_service.protocol import IndexerOutput


@beartype
def handle(payload: Dict[str, object], ctx: RpcContext) -> IndexerOutput:
    path = Path(payload["paths"][0])
    st = path.stat()
    return IndexerOutput(
        indexer_id="file-stats",
        result_type="file-stats",
        result={
            "size_bytes": st.st_size,
            "mode": st.st_mode,
            "mtime": st.st_mtime,
            "ctime": st.st_ctime,
            "modification_time":
            datetime.fromtimestamp(st.st_mtime).isoformat(),
        },
    )


if __name__ == "__main__":
    RpcHarness(handle).run()
