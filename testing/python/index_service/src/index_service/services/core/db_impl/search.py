import logging
from typing import Any

import glom

from index_service.services.core.db_impl.contracts import (
    BaseIndexProtocol,
    FullTextSearchAccessParams,
)
from index_service.services.core.types import (
    FullTextIndexConfig,
    IndexDocument,
    VectorIndexConfig,
)

log = logging.getLogger(__name__)


class SearchMixin:

    def get_full_text_search_path(
        self,
        indexer: BaseIndexProtocol,
    ) -> FullTextSearchAccessParams:
        base = next(
            (document_base for document_base in indexer.get_document_type_bases()
             if document_base.full_text_index is not None),
            None,
        )

        assert base is not None, (
            f"No full text index configured for indexer {indexer.asset_name}")

        index: FullTextIndexConfig = base.full_text_index
        view_name = self._view_name(indexer.asset_name, base, index)
        safe_path = ".".join(f"`{part}`" for part in index.index_path.split("."))

        return FullTextSearchAccessParams(
            safe_path=safe_path,
            view_name=view_name,
            index=index,
        )

    def execute_query_with_conversion(
        self,
        query: str,
        indexer: BaseIndexProtocol,
        glom_paths: tuple[str, ...],
        bind_vars: dict[str, object] | None = None,
    ):
        cursor = self._db.aql.execute(query, bind_vars=bind_vars or {})

        for entry in cursor:
            values = [glom.glom(entry, path) for path in glom_paths]
            values[0] = self.validate_indexer_result_document(values[0], indexer)
            yield tuple(values)

    def full_text_search_phrase(
        self,
        query: str,
        indexer: BaseIndexProtocol,
        limit: int = 20,
        bm25: bool = True,
        wait_for_sync: bool = False,
    ) -> list[tuple[IndexDocument, float]]:
        params = self.get_full_text_search_path(indexer)
        sync = "OPTIONS { waitForSync: true }" if wait_for_sync else ""
        score_function = "BM25" if bm25 else "TFIDF"

        aql = f"""
        FOR d IN `{params.view_name}`
            SEARCH ANALYZER(
                PHRASE(d.{params.safe_path}, @query, @analyzer),
                @analyzer
            )
            {sync}
            LET score = {score_function}(d)
            SORT score DESC
            LIMIT @limit
            RETURN {{ doc: d, score: score }}
        """

        result: list[tuple[IndexDocument, float]] = []
        for document, score in self.execute_query_with_conversion(
                aql,
                indexer,
            ("doc.result", "score"),
            {
                "query": query,
                "analyzer": params.index.analyzer,
                "limit": limit,
            },
        ):
            result.append((document, float(score)))

        return result

    def vector_search(
        self,
        collection: str,
        vector: list[float],
        indexer: BaseIndexProtocol,
        limit: int = 20,
    ) -> list[tuple[IndexDocument, float]]:
        base = next(
            (document_base for document_base in indexer.get_document_type_bases()
             if document_base.vector_index is not None),
            None,
        )

        assert base is not None, (
            f"No vector index configured for indexer {indexer.asset_name}")

        vector_index: VectorIndexConfig = base.vector_index
        assert len(vector) == vector_index.vector_dimensions

        metric_function, sort_direction = {
            "cosine": ("APPROX_NEAR_COSINE", "DESC"),
            "l2": ("APPROX_NEAR_L2", "ASC"),
            "ip": ("APPROX_NEAR_INNER_PRODUCT", "DESC"),
        }[vector_index.vector_metric]

        query = f"""
        FOR d IN {collection}
            LET score = {metric_function}(d.{vector_index.index_path}, @query_vector)
            SORT score {sort_direction}
            LIMIT @limit
            RETURN {{ doc: d, score: score }}
        """

        result: list[tuple[IndexDocument, float]] = []
        for document, score in self.execute_query_with_conversion(
                query,
                indexer,
            ("doc.result", "score"),
            {
                "query_vector": vector,
                "limit": limit,
            },
        ):
            result.append((document, float(score)))

        return result
