from index_service.converters.file_size_converter import FileSizeConverter
from index_service.harness import BaseConverter, BaseIndexer, BaseResource
from index_service.indexers.file_embedding import FileEmbeddingIndexer
from index_service.indexers.file_size import FileSizeIndexer
from index_service.indexers.file_stats import FileStatsIndexer
from index_service.indexers.file_summary import FileSummaryIndexer
from index_service.indexers.full_text import FullTextIndexer
from index_service.indexers.comfy_input_indexer import ComfyInputIndexer
from index_service.resources.file_reverser import FileReverserResource
from index_service.resources.flm_gemma import FlmGemmaResource
from index_service.indexers.exif_metadata import ExifMetadataIndexer

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
    ExifMetadataIndexer,
    ComfyInputIndexer,
]

DEFAULT_CONVERTER_TYPES: list[type[BaseConverter]] = [
    FileSizeConverter,
]
