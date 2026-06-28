import base64
from datetime import datetime
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
from pydantic import BaseModel
from abc import ABC, abstractmethod

from pydantic_core import PydanticSerializationError, to_jsonable_python

from index_service.services.pydantic_utils import model_to_json_data
from index_service.services.types import MD5, AnyModel, FileRef
from index_service.services.utils import ExceptionContextNote


@beartype
@dataclass
class IndexerResultRecord:
    md5: MD5
    indexer_id: str
    result: Dict[str, object]


@beartype
class IndexDatabaseCommon(ABC):

    @abstractmethod
    def ensure_collections(self, names) -> None:
        ...

    @abstractmethod
    def as_ref(self, path: Path) -> FileRef:
        ...

    @abstractmethod
    def store_indexer_result(self, ref, indexer_id, result) -> None:
        ...

    @abstractmethod
    def store_derivation(self, input_files, output_files, config,
                         return_value):
        ...


@beartype
class IndexDatabase(IndexDatabaseCommon):

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
        self.ensure_collections([])

    @property
    def db(self) -> StandardDatabase:
        return self._db

    @property
    def db_name(self) -> str:
        return self._db_name

    def ensure_collections(self, names: List[str]) -> None:
        for name in ["files", "derivations"] + names:
            if not self._db.has_collection(name):
                self._db.create_collection(name)

    def truncate_all(self, names: list[str]) -> None:
        for name in ["files", "derivations"] + names:
            self._db.collection(name).truncate()

    def _md5(self, path: Path) -> MD5:
        digest = hashlib.md5()
        digest.update(path.read_bytes())
        return MD5(md5=digest.hexdigest())

    def _get_md5(self, path: Path) -> MD5:
        files = self._db.collection("files")
        md5 = self._md5(path)
        if files.has(md5.md5):
            doc = files.get(md5.md5)
            known = set(doc["paths"])
            known.update([str(path)])
            files.update({"_key": md5.md5, "paths": sorted(known)})
        else:
            files.insert({"_key": md5.md5, "paths": [str(path)]})

        return md5

    def as_ref(self, path: Path) -> FileRef:
        return FileRef(md5=self._get_md5(path), path=path)

    def store_indexer_result(
        self,
        ref: FileRef,
        indexer_id: str,
        result: AnyModel,
    ) -> None:
        key = ref.md5.md5
        col = self._db.collection(indexer_id)
        result_j = model_to_json_data(result)
        doc = {
            "_key": key,
            "md5": ref.md5.md5,
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

    def get_indexer_result(self, md5: MD5,
                           indexer_id: str) -> IndexerResultRecord:
        doc = cast(dict, self._db.collection(indexer_id).get(md5.md5))

        assert doc, f"Cannot get evaluation results for {indexer_id}({md5})"
        return IndexerResultRecord(
            md5=MD5(md5=doc["md5"]),
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
            "input_md5s": [f.md5.md5 for f in input],
            "output_files": output_files,
            "config": config,
            "return_value": return_value.model_dump(),
        })
        return meta["_key"]

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
              RETURN {{md5: doc.md5, text: doc.result.text}}
            """,
            bind_vars={
                "query": query,
                "limit": limit
            },
        )

        return list(cursor)

    def vector_search(
        self,
        collection: str,
        vector: List[float],
        limit: int = 20,
    ) -> List[Dict[str, Any]]:
        cursor = self._db.aql.execute(f"""
            FOR doc IN {collection}
              FILTER doc.indexer_id == "file_embedding"
              RETURN {{md5: doc.md5, vector: doc.result.vector}}
            """)

        rows = list(cursor)

        def cosine(a: List[float], b: List[float]) -> float:
            dot = sum(x * y for x, y in zip(a, b))
            na = math.sqrt(sum(x * x for x in a))
            nb = math.sqrt(sum(y * y for y in b))
            if na == 0.0 or nb == 0.0:
                return 0.0
            return dot / (na * nb)

        scored = [{
            "md5": row["md5"],
            "score": cosine(vector, row["vector"])
        } for row in rows]
        scored.sort(key=lambda item: item["score"], reverse=True)
        return scored[:limit]
