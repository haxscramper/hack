from datetime import datetime
from pathlib import Path

from beartype.typing import cast

from index_service.services.harness import BaseIndexer
from index_service.services.protocol import IndexerOutput, IndexerRequest
from pydantic import BaseModel

from index_service.services.resources.wd_tagger import WdTag, WdTagger
from PIL import Image


class WdTagIndexerResult(BaseModel, extra="forbid"):
    tags: list[WdTag]


class WdTagIndexer(BaseIndexer):
    asset_name = "wd_tags"
    result_model = WdTagIndexerResult
    required_resources = ("wd_tagger", )

    def can_run(self, path: Path) -> bool:
        return path.suffix in Image.registered_extensions()

    def run(
        self,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        tagger: WdTagger = cast(WdTagger, resources["wd_tagger"])
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=WdTagIndexerResult(
                tags=tagger.tag_image(request.file_ref.path)),
        )
