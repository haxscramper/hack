from datetime import datetime
from pathlib import Path

from index_service.harness import BaseIndexer
from index_service.protocol import IndexerOutput, IndexerRequest
from pydantic import BaseModel


class FileStatsIndexerResult(BaseModel):
    size_bytes: int
    mode: int
    mtime: float
    ctime: float
    modification_time: str


class FileStatsIndexer(BaseIndexer):
    asset_name = "file-stats"
    result_model = FileStatsIndexerResult

    def run(self, request: IndexerRequest,
            **resources: object) -> IndexerOutput:
        st = Path(request.file_ref.paths[0]).stat()
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
