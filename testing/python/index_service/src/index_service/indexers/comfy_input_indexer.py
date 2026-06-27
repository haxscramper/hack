from pathlib import Path

from index_service.harness import BaseIndexer
from index_service.protocol import IndexerOutput, IndexerRequest
from pydantic import BaseModel
from beartype.typing import Any
import logging

log = logging.getLogger(__name__)


class ComfyInput(BaseModel, extra="forbid"):
    node: str
    inputs: dict[str, Any]


class ComfyInputIndexerResult(BaseModel, extra="forbid"):
    inputs: list[ComfyInput]


class ComfyInputIndexer(BaseIndexer):
    asset_name = "comfy_input"
    result_model = ComfyInputIndexerResult
    dependencies = ("exif_metadata", )

    def can_run(self, path: Path) -> bool:
        return path.suffix in [".png", ".jpg", ".webp", ".jpeg"]

    def run(self, request: IndexerRequest,
            **resources: object) -> IndexerOutput:

        log.info(f"{resources}")

        return IndexerOutput(indexer_id=self.asset_name,
                             result=ComfyInputIndexerResult(inputs=[]))
