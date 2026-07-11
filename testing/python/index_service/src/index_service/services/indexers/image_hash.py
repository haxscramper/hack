from pathlib import Path

from index_service.services.core.job_types import BaseIndexer, RunContext, cache_indexer_run
from index_service.services.core.types import IndexDocument, IndexerOutput, IndexerRequest
import imagehash
from PIL import Image


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
        image = Image.open(ctx.get_path(request.file_ref))
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
