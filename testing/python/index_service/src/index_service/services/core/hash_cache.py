import hashlib
import logging
import os
from pathlib import Path

from beartype import beartype
from sqlalchemy import BigInteger, String, Text, URL, create_engine, event, select
from sqlalchemy.dialects.sqlite import insert as sqlite_insert
from sqlalchemy.engine import Engine
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column

from index_service.services.core.types import FileHash

log = logging.getLogger(__name__)


class Base(DeclarativeBase):
    pass


class CachedFileHash(Base):
    __tablename__ = "file_hashes"

    path: Mapped[str] = mapped_column(Text, primary_key=True)
    size: Mapped[int] = mapped_column(BigInteger, nullable=False)
    mtime_ns: Mapped[int] = mapped_column(BigInteger, nullable=False)
    digest: Mapped[str] = mapped_column(String(64), nullable=False)


@beartype
class HashCache:

    def __init__(self, database_path: Path, *, flush_every: int = 1_000) -> None:
        database_path = database_path.expanduser().resolve()
        database_path.parent.mkdir(parents=True, exist_ok=True)
        database_path.touch(exist_ok=True)
        assert database_path.is_file()

        self._engine = self._create_engine(database_path)
        Base.metadata.create_all(self._engine)

        self._connection = self._engine.connect()
        self._flush_every = flush_every
        self._pending: dict[str, tuple[int, int, str]] = {}

        statement = select(
            CachedFileHash.path,
            CachedFileHash.size,
            CachedFileHash.mtime_ns,
            CachedFileHash.digest,
        )
        self._lookup_cache: dict[tuple[str, int, int], str] = {
            (path, size, mtime_ns): digest
            for path, size, mtime_ns, digest in self._connection.execute(statement)
        }

    @staticmethod
    def _create_engine(database_path: Path) -> Engine:
        engine = create_engine(
            URL.create("sqlite", database=str(database_path)),
            connect_args={"check_same_thread": False},
        )

        @event.listens_for(engine, "connect")
        def configure_sqlite(dbapi_connection, _connection_record) -> None:
            cursor = dbapi_connection.cursor()
            cursor.execute("PRAGMA busy_timeout = 30000")
            cursor.execute("PRAGMA journal_mode = WAL")
            cursor.execute("PRAGMA synchronous = NORMAL")
            cursor.close()

        return engine

    def hash(self, path: Path) -> FileHash:
        absolute_path = os.path.abspath(os.fspath(path))
        stat = os.stat(absolute_path)

        digest = self._lookup(absolute_path, stat.st_size, stat.st_mtime_ns)
        if digest is None:
            size, mtime_ns, digest = self._calculate(absolute_path)
            self._store(absolute_path, size, mtime_ns, digest)

        return FileHash(hash=digest)

    def _lookup(self, path: str, size: int, mtime_ns: int) -> str | None:
        return self._lookup_cache.get((path, size, mtime_ns))

    @staticmethod
    def _calculate(path: str) -> tuple[int, int, str]:
        stat = os.stat(path)

        with open(path, "rb") as file:
            digest = hashlib.file_digest(file, "sha256").hexdigest()

        return stat.st_size, stat.st_mtime_ns, digest

    def _store(
        self,
        path: str,
        size: int,
        mtime_ns: int,
        digest: str,
    ) -> None:
        for key in self._lookup_cache:
            if key[0] == path:
                del self._lookup_cache[key]

        self._lookup_cache[(path, size, mtime_ns)] = digest
        self._pending[path] = (size, mtime_ns, digest)

        if len(self._pending) >= self._flush_every:
            self._flush()

    def flush(self) -> None:
        self._flush()

    def _flush(self) -> None:
        if not self._pending:
            return

        rows = [{
            "path": path,
            "size": size,
            "mtime_ns": mtime_ns,
            "digest": digest,
        } for path, (size, mtime_ns, digest) in self._pending.items()]

        table = CachedFileHash.__table__
        statement = sqlite_insert(table).values(rows)
        statement = statement.on_conflict_do_update(
            index_elements=[table.c.path],
            set_={
                "size": statement.excluded.size,
                "mtime_ns": statement.excluded.mtime_ns,
                "digest": statement.excluded.digest,
            },
        )

        self._connection.execute(statement)
        self._connection.commit()
        self._pending.clear()

    def close(self) -> None:
        self._flush()
        self._connection.close()
        self._engine.dispose()
