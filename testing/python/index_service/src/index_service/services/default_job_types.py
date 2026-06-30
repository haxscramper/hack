from index_service.services.converters.file_size_converter import FileSizeConverter
from index_service.services.core.job_types import BaseConverter, BaseIndexer, BaseResource
from index_service.services.indexers.file_embedding import FileEmbeddingIndexer
from index_service.services.indexers.file_size import FileSizeIndexer
from index_service.services.indexers.file_stats import FileStatsIndexer
from index_service.services.indexers.file_summary import FileSummaryIndexer
from index_service.services.indexers.full_text import FullTextIndexer
from index_service.services.indexers.comfy_input_indexer import ComfyInputIndexer
from index_service.services.resources.file_reverser import FileReverserResource
from index_service.services.resources.flm_gemma import FlmGemmaResource
from index_service.services.indexers.exif_metadata import ExifMetadataIndexer

DEFAULT_RESOURCE_TYPES: list[type[BaseResource]] = [
    FileReverserResource,
    FlmGemmaResource,
]

DEFAULT_INDEXER_TYPES: list[type[BaseIndexer]] = [
    FileSizeIndexer,
    FileStatsIndexer,
    FullTextIndexer,
    FileSummaryIndexer,
    FileEmbeddingIndexer,
]

DEFAULT_CONVERTER_TYPES: list[type[BaseConverter]] = [
    FileSizeConverter,
]
