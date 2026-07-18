from index_service.services.core.job_types import (
    BaseIndexer,
    BaseResource,
)
from index_service.services.indexers.chunk_indexing.file_embedding import FileEmbeddingIndexer
from index_service.services.indexers.file_size import FileSizeIndexer
from index_service.services.indexers.file_stats import FileStatsIndexer
from index_service.services.indexers.chunk_indexing.file_summary import FileSummaryIndexer
from index_service.services.indexers.full_document.full_document import DocumentBlockIndexer
from index_service.services.indexers.chunk_indexing.full_text import FullTextIndexer
from index_service.services.resources.file_reverser import FileReverserResource
from index_service.services.resources.text_summary import TextSummaryResource

DEFAULT_RESOURCE_TYPES: list[type[BaseResource]] = [
    FileReverserResource,
    TextSummaryResource,
]

DEFAULT_INDEXER_TYPES: list[type[BaseIndexer]] = [
    FileSizeIndexer,
    FileStatsIndexer,
    FullTextIndexer,
    FileSummaryIndexer,
    FileEmbeddingIndexer,
    DocumentBlockIndexer,
]
