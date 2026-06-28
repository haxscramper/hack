from datetime import datetime
from pathlib import Path

from beartype.typing import cast

from index_service.services.indexers.comfy_input_indexer import ComfyInputIndexer
from index_service.services.indexers.exif_metadata import ExifMetadataIndexer
from index_service.services.job_types import BaseIndexer, RunContext, cache_indexer_run
from index_service.services.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel
import logging

log = logging.getLogger(__name__)


class GenerationParamsIndexerResult(BaseModel, extra="forbid"):
    positive: str
    negative: str
    width: int
    height: int
    sampler: str
    sampler_mode: str
    steps: int
    cfg: float


class GenerationParamsIndexer(BaseIndexer):
    asset_name = "generation_params"
    result_model = GenerationParamsIndexerResult
    required_assets = (
        ExifMetadataIndexer.asset_name,
        ComfyInputIndexer.asset_name,
    )

    def can_run(self, path: Path) -> bool:
        return path.suffix.lower() in [".png", ".webp", ".jpg", ".jpeg"]

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        result = GenerationParamsIndexerResult()

        return IndexerOutput(indexer_id=self.asset_name, result=result)
