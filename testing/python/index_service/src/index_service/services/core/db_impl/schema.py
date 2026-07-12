from beartype import beartype
from beartype.typing import Any, Optional, Dict

from arango.database import StandardDatabase

from index_service.services.core.db_impl.contracts import BaseIndexProtocol
from index_service.services.core.types import (
    FullTextIndexConfig,
    IndexDocument,
    MultiDocumentModel,
)
from index_service.services.pydantic_arango_schema import (
    ArangoSchema,
    arango_schema_for_model,
)


@beartype
class SchemaMixin:
    _db: StandardDatabase
    _indexer_hashes: Dict[str, set[str]]

    def get_edge_name(self, indexer) -> str:
        ...

    def _load_indexer_hashes_from_db(self) -> None:
        for collection in self._db.collections():  # type: ignore
            name = collection["name"]

            if name.startswith("_") or name == "files":
                continue

            self._indexer_hashes[name] = set(
                self._db.aql.execute(  # type: ignore
                    "FOR document IN @@collection RETURN document._key",
                    bind_vars={"@collection": name},
                ))

    def _arango_collection_schema_for_indexer(
        self,
        indexer: BaseIndexProtocol,
    ) -> tuple[dict[str, Any], Optional[dict[str, Any]]]:

        def collection_schema(payload: ArangoSchema, message: str) -> dict[str, Any]:
            rule: dict[str, Any] = {
                "type": "object",
                "properties": {
                    "indexer_id": {
                        "type": "string"
                    },
                    "result": payload.schema,
                },
                "required": ["indexer_id", "result"],
                "additionalProperties": True,
            }

            if payload.definitions:
                rule["definitions"] = payload.definitions

            return {
                "level": "strict",
                "message": message,
                "rule": rule,
            }

        if issubclass(indexer.result_model, MultiDocumentModel):
            document_payload = arango_schema_for_model(indexer.result_model.document_type)
            document_schema = collection_schema(
                document_payload,
                f"invalid document shape for {indexer.result_model.__name__}",
            )

            edge_type = indexer.result_model.edge_type
            if edge_type is None:
                return document_schema, None

            edge_payload = arango_schema_for_model(edge_type)
            edge_schema = collection_schema(
                edge_payload,
                f"invalid edge shape for {indexer.result_model.__name__}",
            )
            return document_schema, edge_schema

        result_payload = arango_schema_for_model(indexer.result_model)
        return (
            collection_schema(
                result_payload,
                f"invalid document shape for {indexer.result_model.__name__}",
            ),
            None,
        )

    def _ensure_collection_with_schema(
        self,
        name: str,
        expected_schema: dict[str, Any],
        *,
        edge: bool = False,
    ) -> None:
        expected_schema = {
            "type": "json",
            **expected_schema,
        }

        if not self._db.has_collection(name):
            self._db.create_collection(
                name,
                edge=edge,
                schema=expected_schema,
            )
            return

        collection = self._db.collection(name)
        actual_schema = collection.properties().get("schema")

        if actual_schema != expected_schema:
            raise RuntimeError(f"schema mismatch for collection {name!r}: "
                               f"expected {expected_schema!r}, got {actual_schema!r}")

    def ensure_collections(
        self,
        indexers: list[BaseIndexProtocol],
    ) -> None:

        for name in ["roots", "files", "derivations"]:
            if not self._db.has_collection(name):
                self._db.create_collection(name)

        files = self._db.collection("files")
        if not any(index.get("name") == "idx_paths_suffix" for index in files.indexes()):
            files.add_index({
                "type": "persistent",
                "fields": ["paths[*].suffix"],
                "name": "idx_paths_suffix",
            })

        for indexer in indexers:
            document_schema, edge_schema = self._arango_collection_schema_for_indexer(
                indexer)
            self._ensure_collection_with_schema(
                indexer.asset_name,
                document_schema,
            )

            if edge_schema is None:
                continue

            edge_name = self.get_edge_name(indexer)
            self._ensure_collection_with_schema(
                edge_name,
                edge_schema,
                edge=True,
            )

            graph_name = self.get_graph_name(indexer)
            edge_definition = {
                "edge_collection": edge_name,
                "from_vertex_collections": [indexer.asset_name],
                "to_vertex_collections": [indexer.asset_name],
            }

            if not self._db.has_graph(graph_name):
                self._db.create_graph(
                    graph_name,
                    edge_definitions=[edge_definition],
                )
                continue

            definitions = self._db.graph(graph_name).properties()["edge_definitions"]
            if len(definitions) != 1:
                raise RuntimeError(
                    f"graph {graph_name!r} must contain exactly one edge definition, "
                    f"got {definitions!r}")

            if definitions[0] != edge_definition:
                raise RuntimeError(
                    f"graph edge definition mismatch for {graph_name!r}: "
                    f"expected {edge_definition!r}, got {definitions[0]!r}")

    def truncate_all(self, names: list[str]) -> None:
        for name in ["roots", "files", "derivations", *names]:
            self._db.collection(name).truncate()

    def enable_index(self, indexer: BaseIndexProtocol) -> None:
        for base in indexer.get_document_type_bases():
            assert isinstance(base, type), (f"{base} for indexer {indexer.asset_name}")
            assert issubclass(
                base, IndexDocument), ("Cannot enable index, the type "
                                       f"{base} is not derived from IndexDocument")

            if base.vector_index is not None:
                vector_index = base.vector_index
                self._db.collection(indexer.asset_name).add_index({
                    "name": (f"vector_{base.__name__}_"
                             f"{vector_index.index_path.replace('.', '_')}"),
                    "type": "vector",
                    "fields": [vector_index.index_path],
                    "params": {
                        "dimension": vector_index.vector_dimensions,
                        "metric": vector_index.vector_metric,
                        "nLists": vector_index.n_lists,
                    },
                    "sparse": vector_index.sparse,
                })

            if base.full_text_index is not None:
                full_text_index: FullTextIndexConfig = base.full_text_index
                view_name = self._view_name(
                    indexer.asset_name,
                    base,
                    full_text_index,
                )

                parts = full_text_index.index_path.split(".")
                nested_fields = {parts[-1]: {"analyzers": [full_text_index.analyzer]}}

                for part in reversed(parts[:-1]):
                    nested_fields = {part: {"fields": nested_fields}}

                view_props = {
                    "links": {
                        indexer.asset_name: {
                            "fields": nested_fields,
                            "includeAllFields": False,
                        }
                    }
                }

                existing_views = {view["name"] for view in self._db.views()}
                if view_name in existing_views:
                    self._db.view(view_name).update_properties(view_props)
                else:
                    self._db.create_arangosearch_view(
                        view_name,
                        properties=view_props,
                    )
