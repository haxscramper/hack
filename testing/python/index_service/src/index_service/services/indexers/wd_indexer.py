from datetime import datetime
from pathlib import Path

from beartype.typing import cast
from sqlalchemy import Engine
from index_service.services.core.job_types import BaseIndexer, BaseIndexerConfig, RunContext
from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.resources.wd_tagger import WdTag, WdTagger
from index_service.services.core.types import IndexDocument, IndexerOutput, IndexerRequest
from PIL import Image
from pydantic import BaseModel


class WdTagIndexerResult(IndexDocument, extra="forbid"):
    tags: list[WdTag]


class WdTagIndexer(BaseIndexer):
    asset_name = "wd_tags"
    result_model = WdTagIndexerResult
    required_resources = ("wd_tagger",)

    def __init__(self, config: BaseIndexerConfig, database: Engine) -> None:
        super().__init__(config=config, database=database)

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
        tagger: WdTagger = cast(WdTagger, resources["wd_tagger"])
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=WdTagIndexerResult(
                hash=request.get_hash_str(),
                tags=tagger.tag_image(ctx, ctx.get_path(request.file_ref)),
            ),
        )
