from index_service.services.core.db_impl.base import DatabaseBase
from index_service.services.core.db_impl.contracts import (
    BaseIndexProtocol,
    FullTextSearchAccessParams,
    IndexerResultRecord,
)
from index_service.services.core.db_impl.files import FileReferenceMixin
from index_service.services.core.db_impl.graphviz import GraphvizMixin
from index_service.services.core.db_impl.schema import SchemaMixin
from index_service.services.core.db_impl.search import SearchMixin
from index_service.services.core.db_impl.storage import StorageMixin


class IndexDatabase(
        GraphvizMixin,
        SearchMixin,
        StorageMixin,
        SchemaMixin,
        FileReferenceMixin,
        DatabaseBase,
):
    """
    Public database API.

    Implementation details are split across ``db_impl`` modules while this
    class preserves the original import path and public method surface.
    """

    FullTextSearchAccessParams = FullTextSearchAccessParams


__all__ = [
    "BaseIndexProtocol",
    "FullTextSearchAccessParams",
    "IndexDatabase",
    "IndexerResultRecord",
]
