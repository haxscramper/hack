import hashlib
import json
import logging
import math
from dataclasses import dataclass
from pathlib import Path

from arango import ArangoClient, DocumentInsertError
from arango.aql import AQL
from arango.database import StandardDatabase
from beartype import beartype
from beartype.typing import Any, ClassVar, Dict, List, Optional, TypeVar, cast, Sequence, Protocol
from openai import BaseModel
from pydantic import TypeAdapter
from arango.exceptions import IndexCreateError
import time

from index_service.services.core.types import (
    AnyModel,
    FileHash,
    FileRef,
    IndexDocument,
    IndexEdge,
    IndexMultiDocument,
    IndexerOutput,
    MultiDocumentModel,
    RootRef,
    VectorIndexConfig,
)

from index_service.services.pydantic_arango_schema import ArangoSchema, arango_schema_for_model
from index_service.services.pydantic_utils import (
    model_from_json_data,
    model_to_json_data,
)
from index_service.services.utils import ExceptionContextNote, ExceptionDump

T = TypeVar("T")

log = logging.getLogger(__name__)


@beartype
class BaseIndexProtocol(Protocol):
    asset_name: str
    result_model: type[BaseModel]
    edge_collection_name: str

    def get_document_type_bases(self) -> list[Any]:
        ...


@beartype
@dataclass
class IndexerResultRecord:
    hash: FileHash
    indexer_id: str
    result: Dict[str, object]


@beartype
class IndexDatabase:

    def __init__(
        self,
        host: str,
        db_name: str,
        username: str,
        password: str,
    ) -> None:
        super().__init__()
        client = ArangoClient(hosts=host)
        sys_db = client.db("_system", username=username, password=password)
        if not sys_db.has_database(db_name):
            sys_db.create_database(db_name)
        self._db = client.db(db_name, username=username, password=password)
        self._db_name = db_name
        self.roots: Dict[str, Path] = dict()
        self.ensure_collections([])
        self._load_roots_from_db()

    @property
    def db(self) -> StandardDatabase:
        return self._db

    @property
    def db_name(self) -> str:
        return self._db_name

    def wait_indexing(self, timeout: float = 30.0, interval: float = 0.1) -> None:
        import time

        deadline = time.monotonic() + timeout
        collections = self._db.collections()

        while time.monotonic() < deadline:
            all_ready = True

            for collection_info in collections:
                name = collection_info["name"]
                if name.startswith("_"):
                    continue

                collection = self._db.collection(name)
                for index in collection.indexes():
                    if index.get("type") == "vector":
                        if index.get("inBackground", False) or index.get(
                                "isBuilding", False):
                            all_ready = False
                            break

                if not all_ready:
                    break

            if all_ready:
                return

            time.sleep(interval)

        raise TimeoutError(
            f"Timed out waiting for vector indexes in database {self._db_name}")

    def _load_roots_from_db(self):
        for row in self._db.aql.execute("FOR doc IN roots return doc"):  # type: ignore
            self.roots[row["_key"]] = Path(row["path"])

    def add_root(self, name: str, root: Path) -> RootRef:
        assert name not in self.roots or self.roots[name] == root, (
            f"Duplicate root name {name} with a different path, stored {self.roots[name]}, trying to set {root}"
        )

        db_roots = self._db.collection("roots")
        if name in self.roots:
            assert db_roots.has(name), (
                f"logical error, roots cache is populated, but the DB does not have the value for {name}"
            )

        else:
            self.roots[name] = root
            db_roots.insert({"_key": name, "path": str(root)})

        return RootRef(name=name)

    def get_root(self, name: RootRef) -> Path:
        return self.roots[name.name]

    def get_path(self, ref: FileRef) -> Path:
        return self.roots[ref.root.name].joinpath(ref.relative)

    def get_edge_name(self, indexer: str) -> str:
        return f"{indexer}_edge"

    def enable_index(self, indexer: BaseIndexProtocol):
        for base in indexer.get_document_type_bases():
            assert isinstance(base, type), f"{base} for indexer {indexer.asset_name}"
            assert issubclass(base, IndexDocument)
            if base.vector_index is not None:
                collection = self._db.collection(indexer.asset_name)
                vector_index = base.vector_index

                index_def = {
                    "name":
                        f"vector_{base.__name__}_{vector_index.index_path.replace('.', '_')}",
                    "type":
                        "vector",
                    "fields": [vector_index.index_path],
                    "params": {
                        "dimension": vector_index.vector_dimensions,
                        "metric": vector_index.vector_metric,
                        "nLists": vector_index.n_lists,
                    },
                    "sparse":
                        vector_index.sparse,
                }

                collection.add_index(index_def)

    def ensure_collections(self, indexers: Sequence[BaseIndexProtocol]) -> None:

        def _ensure_collection_with_schema(
            name: str,
            expected_schema: dict[str, Any],
            *,
            edge: bool = False,
        ) -> None:
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

        graph_name = "indexer_graph"

        for name in ["roots", "files", "derivations"]:
            if not self._db.has_collection(name):
                self._db.create_collection(name)

        edge_definitions: list[dict[str, Any]] = []

        for indexer in indexers:
            document_schema, edge_schema = self._arango_collection_schema_for_indexer(
                indexer)

            _ensure_collection_with_schema(
                indexer.asset_name,
                document_schema,
                edge=False,
            )

            if edge_schema is not None:
                edge_name = self.get_edge_name(indexer.asset_name)

                _ensure_collection_with_schema(
                    edge_name,
                    edge_schema,
                    edge=True,
                )

                edge_definitions.append({
                    "edge_collection": edge_name,
                    "from_vertex_collections": [indexer.asset_name],
                    "to_vertex_collections": [indexer.asset_name],
                })

        files = self._db.collection("files")
        if not any(idx.get("name") == "idx_paths_suffix" for idx in files.indexes()):
            files.add_index({
                "type": "persistent",
                "fields": ["paths[*].suffix"],
                "name": "idx_paths_suffix",
            })

        if edge_definitions:
            if not self._db.has_graph(graph_name):
                self._db.create_graph(
                    graph_name,
                    edge_definitions=edge_definitions,
                )
            else:
                graph = self._db.graph(graph_name)
                existing = {
                    definition["edge_collection"]: definition
                    for definition in graph.properties()["edge_definitions"]
                }

                for definition in edge_definitions:
                    edge_collection = definition["edge_collection"]
                    current = existing.get(edge_collection)
                    if current is None:
                        graph.create_edge_definition(
                            edge_collection=definition["edge_collection"],
                            from_vertex_collections=definition["from_vertex_collections"],
                            to_vertex_collections=definition["to_vertex_collections"],
                        )
                    elif current != definition:
                        raise RuntimeError(
                            f"graph edge definition mismatch for {edge_collection!r}: "
                            f"expected {definition!r}, got {current!r}")

    def truncate_all(self, names: list[str]) -> None:
        for name in ["roots", "files", "derivations"] + names:
            self._db.collection(name).truncate()

    def _hash(self, path: Path) -> FileHash:
        with path.open("rb") as f:
            digest = hashlib.file_digest(f, "sha256")

        return FileHash(hash=digest.hexdigest())

    def get_all_refs(self, hash: FileHash) -> List[FileRef]:
        fdoc = self._db.collection("files").get(hash.hash)
        path_refs = fdoc.get("paths", []) if fdoc else []
        return [
            FileRef(
                hash=FileHash.model_validate(p["hash"],),
                relative=p["relative"],
                root=RootRef.model_validate(p["root"]),
            ) for p in path_refs
        ]

    def as_ref(self, root: RootRef, path: Path) -> FileRef:
        assert root.name in self.roots, (
            f"Unknown root for file ref: '{root}', register root with `add_root()` first")

        relative = str(path.relative_to(self.get_root(root)))

        files = self._db.collection("files")
        _path = self.get_root(root).joinpath(relative)
        hash = self._hash(_path)

        result = FileRef(
            hash=hash,
            relative=relative,
            root=root,
        )

        result_js = result.model_dump()
        result_js["suffix"] = _path.suffix
        result_js["name"] = _path.name

        if files.has(hash.hash):
            doc = files.get(hash.hash)
            known = doc["paths"]  # type: ignore
            known.extend([result_js])
            files.update({"_key": hash.hash, "paths": known})

        else:
            files.insert({"_key": hash.hash, "paths": [result_js]})

        return result

    def has_indexer_result(self, ref: FileRef, indexer_id: str) -> bool:
        col = self._db.collection(indexer_id)
        return col.has(ref.hash.hash)

    def _arango_collection_schema_for_indexer(
            self, indexer: BaseIndexProtocol
    ) -> tuple[dict[str, Any], Optional[dict[str, Any]]]:

        def _collection_schema(payload: ArangoSchema, message: str) -> dict[str, Any]:
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
            document_schema = _collection_schema(
                document_payload,
                f"invalid document shape for {indexer.result_model.__name__}",
            )

            edge_schema: Optional[dict[str, Any]] = None
            edge_type = indexer.result_model.edge_type
            if edge_type is not None:
                edge_payload = arango_schema_for_model(edge_type)
                edge_schema = _collection_schema(
                    edge_payload,
                    f"invalid edge shape for {indexer.result_model.__name__}",
                )

            return document_schema, edge_schema

        else:
            result_payload = arango_schema_for_model(indexer.result_model)
            document_schema = _collection_schema(
                result_payload,
                f"invalid document shape for {indexer.result_model.__name__}",
            )
            return document_schema, None

    def _diagnose_document(self, doc):
        try:
            text = json.dumps(doc, allow_nan=False, indent=2)
        except Exception as e:
            raise RuntimeError(f"json.dumps failed: {e}") from e

        try:
            json.loads(text)
        except json.JSONDecodeError as e:
            start = max(0, e.pos - 120)
            end = min(len(text), e.pos + 120)
            snippet = text[start:end]
            raise RuntimeError(
                f"Invalid JSON at line {e.lineno}, column {e.colno}, pos {e.pos}\n"
                f"{snippet}") from e

        return text

    def _store_indexer_document_one(self, key: str, indexer_id: str, result: AnyModel):
        from index_service.services.core.types import IndexDocument
        assert isinstance(result, IndexDocument), (
            "Final documents inserted into the arango collection must "
            f"be derived from the IndexDocument, but type {type(result)} does not match")

        col = self._db.collection(indexer_id)
        result_j = model_to_json_data(result)
        doc = {
            "_key": key,
            "indexer_id": indexer_id,
            "result": result_j,
        }

        with ExceptionContextNote(lambda: self._diagnose_document(doc)):
            try:
                if col.has(key):
                    col.replace(doc)
                else:
                    col.insert(doc)

            except DocumentInsertError as err:
                raise err from None

    def _store_indexer_edge_one(self, indexer_id: str, result: AnyModel):
        from index_service.services.core.types import IndexEdge

        assert isinstance(result, IndexEdge), (
            "Final edges inserted into the arango collection must "
            f"be derived from the IndexLink, but type {type(result)} does not match")

        col = self._db.collection(self.get_edge_name(indexer_id))
        result_j = model_to_json_data(result)
        from_ = f"{indexer_id}/{result.from_}"
        to_ = f"{indexer_id}/{result.to_}"
        doc = {
            "_from": from_,
            "_to": to_,
            "indexer_id": indexer_id,
            "result": result_j,
        }

        query = f"""
    FOR e IN {self.get_edge_name(indexer_id)}
        FILTER e._from == @_from
        FILTER e._to == @_to
        LIMIT 1
        RETURN e._key
    """

        bind_vars = {
            "_from": from_,
            "_to": to_,
        }

        with ExceptionContextNote(lambda: self._diagnose_document(doc)):
            try:
                cursor = self._db.aql.execute(query, bind_vars=bind_vars)
                existing_key = next(cursor, None)

                if existing_key is not None:
                    doc["_key"] = existing_key
                    col.replace(doc)
                else:
                    col.insert(doc)

            except DocumentInsertError as err:
                raise err from None

    def store_indexer_output(
        self,
        ref: FileRef,
        out: IndexerOutput,
    ) -> None:
        if isinstance(out.result, MultiDocumentModel):
            for document in out.result.documents:
                self._store_indexer_document_one(
                    key=document.hash,
                    indexer_id=out.indexer_id,
                    result=document,
                )

            for edge in out.result.edges:
                self._store_indexer_edge_one(out.indexer_id, edge)

        else:
            self._store_indexer_document_one(key=ref.hash.hash,
                                             indexer_id=out.indexer_id,
                                             result=out.result)

    def get_indexer_result(self, hash: FileHash, indexer_id: str,
                           model_type: type[T]) -> T:
        if issubclass(model_type, MultiDocumentModel):
            assert issubclass(model_type.edge_type, IndexEdge), str(model_type.edge_type)

            document_adapter = TypeAdapter(model_type.document_type)
            edge_adapter = TypeAdapter(model_type.edge_type)

            documents: list[IndexMultiDocument] = []
            edges: list[IndexEdge] = []
            log.debug(f"extracting full document index for hash {hash.hash}")

            for doc in self._db.aql.execute(  # type: ignore
                    f"""
                FOR doc IN {indexer_id}
                FILTER doc.result.file_hash == @hash
                RETURN doc
                """,
                    bind_vars={"hash": hash.hash},  # type: ignore
            ):
                documents.append(
                    document_adapter.validate_python(
                        model_from_json_data(doc["result"], model_type.document_type)))

            for edge in self._db.aql.execute(  # type: ignore
                    f"""
                FOR edge IN {self.get_edge_name(indexer_id)}
                FILTER edge.result.file_hash == @hash
                RETURN edge
                """,
                    bind_vars={"hash": hash.hash},  # type: ignore
            ):
                edges.append(
                    edge_adapter.validate_python(
                        model_from_json_data(edge["result"], model_type.edge_type)))

            return model_type(documents=documents, edges=edges)  # type: ignore

        else:
            doc = cast(dict, self._db.collection(indexer_id).get(hash.hash))
            return model_type.model_validate(
                model_from_json_data(doc["result"], model_type))

    def store_derivation(
        self,
        input: list[FileRef],
        output_files: List[str],
        config: Dict[str, object],
        return_value: AnyModel,
    ) -> str:
        col = self._db.collection("derivations")
        meta = col.insert({
            "input_hashes": [f.hash.hash for f in input],
            "output_files": output_files,
            "config": config,
            "return_value": return_value.model_dump(),
        })
        return meta["_key"]  # type: ignore

    @staticmethod
    def reset_database(host: str, db_name: str, username: str, password: str) -> None:
        client = ArangoClient(hosts=host)
        sys_db = client.db("_system", username=username, password=password)
        if sys_db.has_database(db_name):
            sys_db.delete_database(db_name)
        sys_db.create_database(db_name)

    @property
    def aql(self) -> AQL:
        return self.db.aql

    def full_text_search(self,
                         collection: str,
                         query: str,
                         limit: int = 20) -> List[Dict[str, Any]]:
        cursor = self._db.aql.execute(
            f"""
            FOR doc IN {collection}
              FILTER doc.indexer_id == "full_text"
              FILTER CONTAINS(LOWER(doc.result.text), LOWER(@query))
              LIMIT @limit
              RETURN {{hash: doc._key, text: doc.result.text}}
            """,
            bind_vars={  # type: ignore
                "query": query,
                "limit": limit,
            },
        )

        return list(cursor)  # type: ignore

    def vector_search(
        self,
        collection: str,
        vector: List[float],
        indexer: BaseIndexProtocol,
        limit: int = 20,
    ) -> list[tuple[IndexDocument, float]]:
        vector_base = next(
            (base for base in indexer.get_document_type_bases()
             if base.vector_index is not None),
            None,
        )

        assert vector_base is not None, (
            f"No vector index configured for indexer {indexer.asset_name}, "
            f"none of the document bases {indexer.get_document_type_bases()} "
            f"have vector index type set")

        vector_index: VectorIndexConfig = vector_base.vector_index
        assert vector_index is not None

        assert len(vector) == vector_index.vector_dimensions

        log.debug(
            f"Using index path {vector_index.index_path} in collection {collection}")

        metric_fn, sort_dir = {
            "cosine": ("APPROX_NEAR_COSINE", "DESC"),
            "l2": ("APPROX_NEAR_L2", "ASC"),
            "ip": ("APPROX_NEAR_INNER_PRODUCT", "DESC"),
        }[vector_index.vector_metric]

        query = f"""
        FOR d IN {collection}
            LET score = {metric_fn}(d.{vector_index.index_path}, @query_vector)
            SORT score {sort_dir}
            LIMIT @limit
            RETURN {{
                doc: d,
                score: score
            }}
        """

        log.debug(query)

        cursor = self._db.aql.execute(
            query,
            bind_vars={  # type: ignore
                "query_vector": vector,
                "limit": limit,
            },
        )

        result = list()

        for entry in cursor:  # type: ignore
            if issubclass(indexer.result_model, MultiDocumentModel):
                doc_type = indexer.result_model.document_type

            else:
                doc_type = indexer.result_model

            result.append((TypeAdapter(doc_type).validate_python(
                model_from_json_data(entry["doc"]["result"],
                                     doc_type)), float(entry["score"])))

        return result
