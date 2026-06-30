import hashlib
import json
import math
from dataclasses import dataclass
from pathlib import Path

from arango import ArangoClient
from arango.aql import AQL
from arango.database import StandardDatabase
from beartype import beartype
from beartype.typing import Any, Dict, List, Optional, cast
from openai import BaseModel
from index_service.services.pydantic_utils import model_from_json_data, model_to_json_data
from index_service.services.core.types import FileHash, AnyModel, FileRef, RootRef
from index_service.services.utils import ExceptionContextNote
import logging

log = logging.getLogger(__name__)


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
        for row in self._db.aql.execute(
                "FOR doc IN roots return doc"):  # type: ignore
            self.roots[row["_key"]] = Path(row["path"])

    def add_root(self, name: str, root: Path) -> RootRef:
        assert name not in self.roots or self.roots[
            name] == root, f"Duplicate root name {name} with a different path, stored {self.roots[name]}, trying to set {root}"

        db_roots = self._db.collection("roots")
        if name in self.roots:
            assert db_roots.has(
                name
            ), f"logical error, roots cache is populated, but the DB does not have the value for {name}"

        else:
            self.roots[name] = root
            db_roots.insert({"_key": name, "path": str(root)})

        return RootRef(name=name)

    def get_root(self, name: RootRef) -> Path:
        return self.roots[name.name]

    def get_path(self, ref: FileRef) -> Path:
        return self.roots[ref.root.name].joinpath(ref.relative)

    def ensure_collections(self, names: List[str]) -> None:
        for name in ["roots", "files", "derivations"] + names:
            if not self._db.has_collection(name):
                self._db.create_collection(name)

        files = self._db.collection("files")
        if "idx_paths_suffix" not in files.indexes():  # type: ignore
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
                hash=FileHash.model_validate(p["hash"], ),
                relative=p["relative"],
                root=RootRef.model_validate(p["root"]),
            ) for p in path_refs
        ]

    def as_ref(self, root: RootRef, path: Path) -> FileRef:
        assert root.name in self.roots, f"Unknown root for file ref: '{root}', register root with `add_root()` first"

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

    def store_indexer_result(
        self,
        ref: FileRef,
        indexer_id: str,
        result: AnyModel,
    ) -> None:
        key = ref.hash.hash
        col = self._db.collection(indexer_id)
        result_j = model_to_json_data(result)
        doc = {
            "_key": key,
            "hash": ref.hash.hash,
            "indexer_id": indexer_id,
            "result": result_j,
        }

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
            if col.has(key):
                col.replace(doc)
            else:
                col.insert(doc)

    def get_indexer_result_type(self, hash: FileHash, indexer_id: str,
                                type) -> Optional[AnyModel]:
        doc = cast(dict, self._db.collection(indexer_id).get(hash.hash))
        if doc:
            return type.model_validate(
                model_from_json_data(doc["result"], type))

        else:
            return None

    def get_indexer_result(self, hash: FileHash,
                           indexer_id: str) -> IndexerResultRecord:
        doc = cast(dict, self._db.collection(indexer_id).get(hash.hash))

        assert doc, f"Cannot get evaluation results for {indexer_id}({hash})"
        return IndexerResultRecord(
            hash=FileHash(hash=doc["hash"]),
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
    def reset_database(host: str, db_name: str, username: str,
                       password: str) -> None:
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
              RETURN {{hash: doc.hash, text: doc.result.text}}
            """,
            bind_vars={  # type: ignore
                "query": query,
                "limit": limit
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
              RETURN {{hash: doc.hash, vector: doc.result.vector}}
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
