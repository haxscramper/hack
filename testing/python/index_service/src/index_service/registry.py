from index_service.converters.file_size_converter import FileSizeConverterActor
from index_service.indexers.file_embedding import FileEmbeddingIndexerActor
from index_service.indexers.file_size import FileSizeIndexerActor
from index_service.indexers.file_stats import FileStatsIndexerActor
from index_service.indexers.file_summaries import FileSummariesIndexerActor
from index_service.indexers.full_text import FullTextIndexerActor
from index_service.resources.file_reverser import FileReverserResourceActor
from index_service.resources.flm_gemma import FlmGemmaResourceActor

DEFAULT_ACTOR_TYPES = [
    FileReverserResourceActor,
    FlmGemmaResourceActor,
    FileSizeIndexerActor,
    FileStatsIndexerActor,
    FullTextIndexerActor,
    FileSummariesIndexerActor,
    FileEmbeddingIndexerActor,
    FileSizeConverterActor,
]
