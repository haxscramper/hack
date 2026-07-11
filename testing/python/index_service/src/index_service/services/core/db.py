import hashlib
import json
import logging
import math
from dataclasses import dataclass
from pathlib import Path
import textwrap

from arango import ArangoClient, DocumentInsertError
from arango.aql import AQL
from arango.database import StandardDatabase
from beartype import beartype
from beartype.typing import Any, Dict, List, Optional, TypeVar, cast, Sequence, Protocol
from openai import BaseModel
from pydantic import TypeAdapter
import pydantic
import glom
import html
import json
import graphviz

from index_service.services.core.types import (
    AnyModel,
    FileHash,
    FileRef,
    FullTextIndexConfig,
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
from index_service.services.utils import ExceptionContextNote

T = TypeVar("T")

log = logging.getLogger(__name__)


@beartype
class BaseIndexProtocol(Protocol):
    asset_name: str
    result_model: type[pydantic.BaseModel]
    edge_collection_name: Optional[str]

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
        collections = cast(list[dict], self._db.collections())

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

    @staticmethod
    def _view_name(asset_name: str, base: type,
                   full_text_index: FullTextIndexConfig) -> str:
        return (f"fts_{asset_name}_{base.__name__}_"
                f"{full_text_index.index_path.replace('.', '_')}")

    def get_collection_name(self, indexer: BaseIndexProtocol | str) -> str:
        return indexer if isinstance(indexer, str) else indexer.asset_name

    def get_edge_name(self, indexer: BaseIndexProtocol | str) -> str:
        return f"{self.get_collection_name(indexer)}_edge"

    def get_graph_name(self, indexer: BaseIndexProtocol | str) -> str:
        return f"{self.get_collection_name(indexer)}_graph"

    def enable_index(self, indexer: BaseIndexProtocol):
        for base in indexer.get_document_type_bases():
            assert isinstance(base, type), f"{base} for indexer {indexer.asset_name}"
            assert issubclass(
                base, IndexDocument
            ), f"Cannot enable index, the type {base} is not derived from IndexDocument"
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

            if base.full_text_index is not None:
                full_text_index = base.full_text_index
                view_name = self._view_name(indexer.asset_name, base, full_text_index)

                # 1) Convert dot-notation path into ArangoDB nested fields dictionary
                parts = full_text_index.index_path.split('.')

                # Start from the innermost field
                nested_fields = {parts[-1]: {"analyzers": [full_text_index.analyzer],}}

                # Wrap it with "fields": {...} for every parent segment
                for part in reversed(parts[:-1]):
                    nested_fields = {part: {"fields": nested_fields}}

                view_props = {
                    "links": {
                        indexer.asset_name: {
                            "fields": nested_fields,
                            "includeAllFields": False,
                        },
                    },
                }

                existing = {v["name"] for v in self._db.views()}
                if view_name in existing:
                    self._db.view(view_name).update_properties(view_props)
                else:
                    self._db.create_arangosearch_view(view_name, properties=view_props)

    def _format_html_label(
        self,
        data: dict,
        json_labels: bool,
        wrap_width: int = 120,
    ) -> str:

        def wrap_text(text: str) -> str:
            wrapped_lines = []
            for line in text.split("\n"):
                if len(line) <= wrap_width:
                    wrapped_lines.append(line)
                    continue
                indent = len(line) - len(line.lstrip(" "))
                pad = " " * indent
                wrapped_lines.extend(
                    textwrap.wrap(
                        line,
                        width=wrap_width,
                        subsequent_indent=pad,
                        break_long_words=True,
                        break_on_hyphens=False,
                    ) or [line])
            return "\n".join(wrapped_lines)

        def escape_html(text: str) -> str:
            escaped = html.escape(wrap_text(text))
            return escaped.replace(" ", "&nbsp;").replace("\n", '<br align="left"/>')

        if json_labels:
            text = json.dumps(data, indent=2, default=str)
            return f'<<font face="monospace">{escape_html(text)}<br align="left"/></font>>'

        def format_value(value) -> str:
            text = json.dumps(value, indent=2, default=str)
            return f'<font face="monospace">{escape_html(text)}</font>'

        rows = []
        for key, value in data.items():
            k = html.escape(str(key))
            v = format_value(value)
            rows.append(f'<tr><td align="left" valign="top"><b>{k}</b></td>'
                        f'<td align="left" balign="left">{v}</td></tr>')
        table = "".join(rows)
        return (f'<<table border="0" cellborder="1" cellspacing="0">{table}</table>>')

    def render_indexer_graphviz(
        self,
        indexer: BaseIndexProtocol,
        start_vertex_ids: set[str],
        *,
        max_nodes: int = int(1E10),
        max_depth: int = int(1E10),
        max_outgoing_edges: int = int(1E10),
        json_labels: bool = False,
    ) -> graphviz.Digraph:
        graph = graphviz.Digraph(name=self.get_graph_name(indexer.asset_name))
        edge_collection = self.get_edge_name(indexer.asset_name)

        node_shape = "box" if json_labels else "plaintext"

        visited: set[str] = set()
        added_nodes: set[str] = set()

        def add_document_node(doc: dict, *, shape: str) -> None:
            node_id = doc["_id"]
            if node_id in added_nodes:
                return
            graph.node(
                node_id,
                label=self._format_html_label(doc, json_labels),
                shape=shape,
                color="red",
            )
            added_nodes.add(node_id)

        # DFS stack of (vertex _id, depth)
        vertex_collection = indexer.asset_name

        def to_full_id(vid: str) -> str:
            return vid if "/" in vid else f"{vertex_collection}/{vid}"

        stack: list[tuple[str, int]] = [(to_full_id(vid), 0) for vid in start_vertex_ids]

        query = """
        FOR v, e IN 1..1 OUTBOUND @start_id @@edge_collection
            LIMIT @max_outgoing_edges
            RETURN {vertex: v, edge: e}
        """

        while stack and len(added_nodes) < max_nodes:
            current_id, depth = stack.pop()
            if current_id in visited:
                continue

            visited.add(current_id)

            current_doc = self._db.document(current_id)
            add_document_node(current_doc, shape=node_shape)

            if depth >= max_depth:
                continue

            cursor = self._db.aql.execute(
                query,
                bind_vars={ # type: ignore
                    "start_id": current_id,
                    "@edge_collection": edge_collection,
                    "max_outgoing_edges": max_outgoing_edges,
                },
            )

            for result in cursor:
                vertex = result["vertex"]
                edge = result["edge"]

                add_document_node(vertex, shape=node_shape)

                # Intermediate rectangle node carrying the edge label.
                edge_node_id = edge["_id"]
                if edge_node_id not in added_nodes:
                    graph.node(
                        edge_node_id,
                        label=self._format_html_label(edge, json_labels),
                        shape="box",
                        color="blue",
                    )
                    added_nodes.add(edge_node_id)

                graph.edge(current_id, edge_node_id)
                graph.edge(edge_node_id, vertex["_id"])

                if vertex["_id"] not in visited:
                    stack.append((vertex["_id"], depth + 1))

        return graph

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

        for name in ["roots", "files", "derivations"]:
            if not self._db.has_collection(name):
                self._db.create_collection(name)

        files = self._db.collection("files")
        if not any(idx.get("name") == "idx_paths_suffix" for idx in files.indexes()):
            files.add_index({
                "type": "persistent",
                "fields": ["paths[*].suffix"],
                "name": "idx_paths_suffix",
            })

        for indexer in indexers:
            document_schema, edge_schema = self._arango_collection_schema_for_indexer(
                indexer)

            _ensure_collection_with_schema(
                indexer.asset_name,
                document_schema,
                edge=False,
            )

            if edge_schema is None:
                continue

            edge_name = self.get_edge_name(indexer.asset_name)

            _ensure_collection_with_schema(
                edge_name,
                edge_schema,
                edge=True,
            )

            graph_name = self.get_graph_name(indexer.asset_name)
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

            graph = self._db.graph(graph_name)
            definitions = graph.properties()["edge_definitions"]

            if len(definitions) != 1:
                raise RuntimeError(
                    f"graph {graph_name!r} must contain exactly one edge definition, "
                    f"got {definitions!r}")

            current = definitions[0]
            if current != edge_definition:
                raise RuntimeError(f"graph edge definition mismatch for {graph_name!r}: "
                                   f"expected {edge_definition!r}, got {current!r}")

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

    def get_file_hash(self, ref: FileHash | FileRef) -> str:
        if isinstance(ref, FileHash):
            return ref.hash
        else:
            return ref.hash.hash

    def has_indexer_result(self, ref: FileHash | FileRef,
                           indexer: BaseIndexProtocol | str) -> bool:
        col = self._db.collection(self.get_collection_name(indexer))
        return cast(bool, col.has(self.get_file_hash(ref)))

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

    def validate_indexer_result_document(self, result: dict[str, Any],
                                         indexer: BaseIndexProtocol) -> Any:
        if issubclass(indexer.result_model, MultiDocumentModel):
            doc_type = indexer.result_model.document_type
        else:
            doc_type = indexer.result_model

        adapter = TypeAdapter(doc_type)

        return adapter.validate_python(model_from_json_data(result, doc_type))

    def get_indexer_one_document(self, document_id: str,
                                 indexer: BaseIndexProtocol) -> Any:
        for doc in self._db.aql.execute(  # type: ignore
                f"""
            FOR doc IN {indexer.asset_name}
            FILTER doc._key == @hash
            RETURN doc.result
            """,
                bind_vars={"hash": document_id},
        ):
            return self.validate_indexer_result_document(doc, indexer)

        raise KeyError(
            f"Could not find document with ID {document_id} in the indexer collection {indexer.asset_name}"
        )

    def get_indexer_result(self, hash: FileHash, indexer: BaseIndexProtocol) -> Any:
        model_type = indexer.result_model
        indexer_id = indexer.asset_name
        if issubclass(model_type, MultiDocumentModel):
            assert issubclass(model_type.edge_type, IndexEdge), str(model_type.edge_type)
            edge_adapter = TypeAdapter(model_type.edge_type)

            documents: list[IndexMultiDocument] = []
            edges: list[IndexEdge] = []

            for doc in self._db.aql.execute(  # type: ignore
                    f"""
                FOR doc IN {indexer_id}
                FILTER doc.result.file_hash == @hash
                RETURN doc.result
                """,
                    bind_vars={"hash": hash.hash},  # type: ignore
            ):
                documents.append(self.validate_indexer_result_document(doc, indexer))

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

    def get_inbound(self, doc_id: str, indexer: BaseIndexProtocol) -> list[str]:
        result = list()
        for doc in self.aql.execute(  # type: ignore
                f""" 
        FOR v, e in 1..1 INBOUND @doc_id @@edge_collection
            RETURN v
        """, bind_vars={
            "doc_id": f"{indexer.asset_name}/{doc_id}",
            "@edge_collection": self.get_edge_name(indexer.asset_name),
        }):
            result.append(doc["_key"])

        return result

    class FullTextSearchAccessParams(BaseModel, extra="forbid"):
        safe_path: str
        view_name: str
        index: FullTextIndexConfig

    def get_full_text_search_path(
            self, indexer: BaseIndexProtocol) -> FullTextSearchAccessParams:
        full_text_base = next(
            (base for base in indexer.get_document_type_bases()
             if base.full_text_index is not None),
            None,
        )

        assert full_text_base is not None, (
            f"No full text index configured for indexer {indexer.asset_name}, "
            f"none of the document bases {indexer.get_document_type_bases()} "
            f"have full text index type set")

        full_text_index = full_text_base.full_text_index
        assert full_text_index is not None

        view_name = self._view_name(indexer.asset_name, full_text_base, full_text_index)
        log.debug(f"Using view {view_name} field {full_text_index.index_path} "
                  f"for collection {indexer.asset_name}")

        # 2) Safely format path keys with backticks (handles dashes/reserved words gracefully)
        # E.g. "result.text" -> "`result`.`text`"
        safe_path = ".".join(f"`{p}`" for p in full_text_index.index_path.split("."))

        return IndexDatabase.FullTextSearchAccessParams(
            safe_path=safe_path,
            view_name=view_name,
            index=full_text_index,
        )

    def execute_query_with_conversion(
        self,
        query: str,
        indexer: BaseIndexProtocol,
        glom_paths: tuple[str, ...],
        bind_vars: dict[str, object] | None = None,
    ):
        cursor = self._db.aql.execute(
            query,
            bind_vars=bind_vars or {},  # type: ignore
        )

        for entry in cursor:  # type: ignore
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
        score_fn = "BM25" if bm25 else "TFIDF"

        aql = f"""
        FOR d IN `{params.view_name}`
            SEARCH ANALYZER(
                PHRASE(d.{params.safe_path}, @query, @analyzer),
                @analyzer
            )
            {sync}
            LET score = {score_fn}(d)
            SORT score DESC
            LIMIT @limit
            RETURN {{
                doc: d,
                score: score
            }}
        """

        log.debug(aql)
        log.debug(f"@query = {query}")
        log.debug(f"@analyzer = {params.index.analyzer}")
        log.debug(f"@limit = {limit}")

        result = list()
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
