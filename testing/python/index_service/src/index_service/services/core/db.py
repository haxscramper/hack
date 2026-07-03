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
from beartype.typing import Any, Dict, List, Optional, cast, Sequence, Protocol
from openai import BaseModel

from index_service.services.core.types import (
    AnyModel,
    FileHash,
    FileRef,
    IndexerOutput,
    MultiDocumentModel,
    RootRef,
)
from index_service.services.pydantic_utils import (
    arango_schema_for_model,
    model_from_json_data,
    model_to_json_data,
)
from index_service.services.utils import ExceptionContextNote

log = logging.getLogger(__name__)


@beartype
class BaseIndexProtocol(Protocol):
    asset_name: str
    result_model: type[BaseModel]
    edge_collection_name: str


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

    def ensure_collections(self, indexers: Sequence[BaseIndexProtocol]) -> None:
        for name in ["roots", "files", "derivations"]:
            if not self._db.has_collection(name):
                self._db.create_collection(name)

        for indexer in indexers:
            expected_schema = self._arango_collection_schema_for_indexer(indexer)

            if not self._db.has_collection(indexer.asset_name):
                self._db.create_collection(
                    indexer.asset_name,
                    schema=expected_schema,
                )
            else:
                collection = self._db.collection(indexer.asset_name)
                actual_schema = collection.properties().get("schema")
                if actual_schema != expected_schema:
                    raise RuntimeError(
                        f"schema mismatch for collection {indexer.asset_name!r}: "
                        f"expected {expected_schema!r}, got {actual_schema!r}")

        files = self._db.collection("files")
        if not any(idx.get("name") == "idx_paths_suffix" for idx in files.indexes()):
            files.add_index({
                "type": "persistent",
                "fields": ["paths[*].suffix"],
                "name": "idx_paths_suffix",
            })

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
            self, indexer: BaseIndexProtocol) -> dict[str, Any]:
        if issubclass(indexer.result_model, MultiDocumentModel):
            result_schema = arango_schema_for_model(indexer.result_model.document_type)

        else:
            result_schema = arango_schema_for_model(indexer.result_model)

        return {
            "level": "strict",
            "message": f"invalid document shape for {indexer.result_model.__name__}",
            "rule": {
                "type": "object",
                "properties": {
                    "indexer_id": {
                        "type": "string"
                    },
                    "result": result_schema,
                },
                "required": ["indexer_id", "result"],
                "additionalProperties": True,
            },
        }

    def _store_indexer_document_one(self, key: str, indexer_id: str, result: AnyModel):
        col = self._db.collection(indexer_id)
        result_j = model_to_json_data(result)
        doc = {
            "_key": key,
            "indexer_id": indexer_id,
            "result": result_j,
        }

        # log.info(doc)
        # import json, math
        # log.debug(json.dumps(doc["result"]))
        # log.debug([v for v in doc["result"]["vector"] if not math.isfinite(v)])

        def diagnose_document(doc):
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

        with ExceptionContextNote(lambda: diagnose_document(doc)):
            try:
                if col.has(key):
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

        else:
            self._store_indexer_document_one(key=ref.hash.hash,
                                             indexer_id=out.indexer_id,
                                             result=out.result)

    def get_indexer_result_type(self, hash: FileHash, indexer_id: str,
                                type) -> Optional[AnyModel]:
        doc = cast(dict, self._db.collection(indexer_id).get(hash.hash))
        if doc:
            return type.model_validate(model_from_json_data(doc["result"], type))

        else:
            return None

    def get_indexer_result(self, hash: FileHash, indexer_id: str) -> IndexerResultRecord:
        doc = cast(dict, self._db.collection(indexer_id).get(hash.hash))

        assert doc, f"Cannot get evaluation results for {indexer_id}({hash})"
        return IndexerResultRecord(
            hash=FileHash(hash=doc["_key"]),
            indexer_id=doc["indexer_id"],
            result=doc["result"],
        )

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
        limit: int = 20,
    ) -> List[Dict[str, Any]]:
        cursor = self._db.aql.execute(f"""
            FOR doc IN {collection}
              FILTER doc.indexer_id == "file_embedding"
              RETURN {{hash: doc._key, vector: doc.result.vector}}
            """)

        rows = list(cursor)  # type: ignore

        def cosine(a: List[float], b: List[float]) -> float:
            dot = sum(x * y for x, y in zip(a, b))
            na = math.sqrt(sum(x * x for x in a))
            nb = math.sqrt(sum(y * y for y in b))
            if na == 0.0 or nb == 0.0:
                return 0.0
            return dot / (na * nb)

        scored = [{
            "hash": row["hash"],
            "score": cosine(vector, row["vector"])
        } for row in rows]
        scored.sort(key=lambda item: item["score"], reverse=True)
        return scored[:limit]
