from pathlib import Path

from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.core.types import IndexDocument, IndexerOutput, IndexerRequest
import imagehash
from PIL import Image
import logging

log = logging.getLogger(__name__)


class ImageHashIndexerResult(IndexDocument, extra="forbid"):
    average: str
    perceptual: str
    difference: str
    wavelet: str


class ImageHashIndexer(BaseIndexer):
    asset_name = "image_hash"
    result_model = ImageHashIndexerResult
    max_parallel = 16

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)

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
        path = ctx.get_path(request.file_ref)
        assert path.exists(), path
        log.debug(f"Image hash for '{path}'")
        try:
            image = Image.open(path)
            image.load()

            return IndexerOutput(
                indexer_id=self.asset_name,
                result=ImageHashIndexerResult(
                    average=str(imagehash.average_hash(image)),
                    perceptual=str(imagehash.phash(image)),
                    difference=str(imagehash.dhash(image)),
                    wavelet=str(imagehash.whash(image)),
                    hash=request.get_hash_str(),
                ),
            )

        except OSError:
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=ImageHashIndexerResult(
                    average="",
                    perceptual="",
                    difference="",
                    wavelet="",
                    hash=request.get_hash_str(),
                ),
            )

            pass
