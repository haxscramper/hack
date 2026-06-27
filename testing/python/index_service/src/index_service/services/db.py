import hashlib
import math
from dataclasses import dataclass
from pathlib import Path

from arango import ArangoClient
from beartype import beartype
from beartype.typing import Any, Dict, List, Optional
from pydantic import BaseModel

from index_service.services.protocol import AnyModel


@beartype
@dataclass
class IndexerResultRecord:
    md5: str
    indexer_id: str
    result: Dict[str, object]


@beartype
class IndexDatabase:

    def __init__(self, host: str, db_name: str, username: str,
                 password: str) -> None:
        client = ArangoClient(hosts=host)
        sys_db = client.db("_system", username=username, password=password)
        if not sys_db.has_database(db_name):
            sys_db.create_database(db_name)
        self._db = client.db(db_name, username=username, password=password)
        self._db_name = db_name
        self._ensure_collections()

    @property
    def db_name(self) -> str:
        return self._db_name

    def _ensure_collections(self) -> None:
        for name in ["files", "indexer_results", "derivations"]:
            if not self._db.has_collection(name):
                self._db.create_collection(name)

    def truncate_all(self) -> None:
        for name in ["files", "indexer_results", "derivations"]:
            self._db.collection(name).truncate()

    def _md5(self, path: Path) -> str:
        digest = hashlib.md5()
        digest.update(path.read_bytes())
        return digest.hexdigest()

    def get_md5(self, path: Path) -> str:
        files = self._db.collection("files")
        md5 = self._md5(path)
        if files.has(md5):
            doc = files.get(md5)
            known = set(doc["paths"])
            known.update([str(path)])
            files.update({"_key": md5, "paths": sorted(known)})
        else:
            files.insert({"_key": md5, "paths": [str(path)]})

        return md5

    def store_indexer_result(
        self,
        md5: str,
        indexer_id: str,
        result: AnyModel,
    ) -> None:
        key = f"{md5}__{indexer_id}"
        col = self._db.collection("indexer_results")
        doc = {
            "_key": key,
            "md5": md5,
            "indexer_id": indexer_id,
            "result": result.model_dump(mode="json"),
        }

        if col.has(key):
            col.replace(doc)
        else:
            col.insert(doc)

    def get_indexer_result(self, md5: str,
                           indexer_id: str) -> IndexerResultRecord:
        doc = self._db.collection("indexer_results").get(
            f"{md5}__{indexer_id}")
        assert doc, f"Cannot get evaluation results for {indexer_id}({md5})"
        return IndexerResultRecord(
            md5=doc["md5"],
            indexer_id=doc["indexer_id"],
            result=doc["result"],
        )

    def get_indexer_result_optional(
        self,
        md5: str,
        indexer_id: str,
    ) -> Optional[IndexerResultRecord]:
        doc = self._db.collection("indexer_results").get(
            f"{md5}__{indexer_id}")
        if doc is None:
            return None
        return IndexerResultRecord(
            md5=doc["md5"],
            indexer_id=doc["indexer_id"],
            result=doc["result"],
        )

    def store_derivation(
        self,
        input_md5s: List[str],
        output_files: List[str],
        config: Dict[str, object],
        return_value: AnyModel,
    ) -> str:
        col = self._db.collection("derivations")
        meta = col.insert({
            "input_md5s": input_md5s,
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

    def full_text_search(self,
                         query: str,
                         limit: int = 20) -> List[Dict[str, Any]]:
        cursor = self._db.aql.execute(
            """
            FOR doc IN indexer_results
              FILTER doc.indexer_id == "full_text"
              FILTER CONTAINS(LOWER(doc.result.text), LOWER(@query))
              LIMIT @limit
              RETURN {md5: doc.md5, text: doc.result.text}
            """,
            bind_vars={
                "query": query,
                "limit": limit
            },
        )
        return list(cursor)

    def vector_search(
        self,
        vector: List[float],
        limit: int = 20,
    ) -> List[Dict[str, Any]]:
        cursor = self._db.aql.execute("""
            FOR doc IN indexer_results
              FILTER doc.indexer_id == "file_embedding"
              RETURN {md5: doc.md5, vector: doc.result.vector}
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
