from index_service.converters.file_size_converter import FileSizeConverter
from index_service.indexers.file_embedding import FileEmbeddingIndexer
from index_service.indexers.file_size import FileSizeIndexer
from index_service.indexers.file_stats import FileStatsIndexer
from index_service.indexers.file_summaries import FileSummariesIndexer
from index_service.indexers.full_text import FullTextIndexer
from index_service.resources.file_reverser import FileReverserResource
from index_service.resources.flm_gemma import FlmGemmaResource

DEFAULT_RESOURCE_TYPES = [
    FileReverserResource,
    FlmGemmaResource,
]

DEFAULT_INDEXER_TYPES = [
    FileSizeIndexer,
    FileStatsIndexer,
    FullTextIndexer,
    FileSummariesIndexer,
    FileEmbeddingIndexer,
]

DEFAULT_CONVERTER_TYPES = [
    FileSizeConverter,
]
