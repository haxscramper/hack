import logging
from pathlib import Path
from typing import cast

from arango import ArangoClient
from arango.aql import AQL
from arango.database import StandardDatabase
from beartype.typing import Dict

from index_service.services.core.hash_cache import HashCache
from index_service.services.core.types import FileRef

log = logging.getLogger(__name__)


class DatabaseBase:

    def __init__(
        self,
        host: str,
        db_name: str,
        username: str,
        password: str,
        hash_cache: HashCache,
        only_short_curcuit_checks: bool = False,
    ) -> None:
        client = ArangoClient(hosts=host)
        sys_db = client.db("_system", username=username, password=password)

        if not sys_db.has_database(db_name):
            sys_db.create_database(db_name)

        self._db = client.db(db_name, username=username, password=password)
        self._db_name = db_name
        self.roots: Dict[str, Path] = {}
        self.file_refs: set[FileRef] = set()
        self._indexer_hashes: Dict[str, set[str]] = {}
        self.hash_cache = hash_cache
        self.only_short_curcuit_checks = only_short_curcuit_checks

        self.ensure_collections([])
        self._load_roots_from_db()
        self._load_file_refs_from_db()
        self._load_indexer_hashes_from_db()

    @property
    def db(self) -> StandardDatabase:
        return self._db

    @property
    def db_name(self) -> str:
        return self._db_name

    @property
    def aql(self) -> AQL:
        return self.db.aql

    @staticmethod
    def reset_database(
        host: str,
        db_name: str,
        username: str,
        password: str,
    ) -> None:
        client = ArangoClient(hosts=host)
        sys_db = client.db("_system", username=username, password=password)

        if sys_db.has_database(db_name):
            sys_db.delete_database(db_name)

        sys_db.create_database(db_name)

    def wait_indexing(
        self,
        timeout: float = 30.0,
        interval: float = 0.1,
    ) -> None:
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
                    if (index.get("type") == "vector" and
                        (index.get("inBackground", False) or
                         index.get("isBuilding", False))):
                        all_ready = False
                        break

                if not all_ready:
                    break

            if all_ready:
                return

            time.sleep(interval)

        raise TimeoutError(
            f"Timed out waiting for vector indexes in database {self._db_name}")

    @staticmethod
    def _view_name(asset_name: str, base: type, full_text_index) -> str:
        path = full_text_index.index_path.replace(".", "_")
        return f"fts_{asset_name}_{base.__name__}_{path}"

    def get_collection_name(self, indexer) -> str:
        return indexer if isinstance(indexer, str) else indexer.asset_name

    def get_edge_name(self, indexer) -> str:
        return f"{self.get_collection_name(indexer)}_edge"

    def get_graph_name(self, indexer) -> str:
        return f"{self.get_collection_name(indexer)}_graph"
