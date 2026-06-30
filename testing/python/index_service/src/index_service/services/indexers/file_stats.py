from datetime import datetime
from pathlib import Path

from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FileStatsIndexerResult(BaseModel, extra="forbid"):
    size_bytes: int
    mode: int
    mtime: float
    ctime: float
    modification_time: str


class FileStatsIndexer(BaseIndexer):
    asset_name = "file_stats"
    result_model = FileStatsIndexerResult

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)

    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        st = ctx.get_path(request.file_ref).stat()
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileStatsIndexerResult(
                size_bytes=st.st_size,
                mode=st.st_mode,
                mtime=st.st_mtime,
                ctime=st.st_ctime,
                modification_time=datetime.fromtimestamp(
                    st.st_mtime).isoformat(),
            ),
        )
