from __future__ import annotations

import hashlib
import logging
import shutil
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path

from beartype import beartype
from beartype.typing import Optional, Sequence
from sqlalchemy import DateTime, Integer, String, create_engine, select
from sqlalchemy.engine import Engine
from sqlalchemy.orm import DeclarativeBase, Mapped, Session, mapped_column

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
)
logger = logging.getLogger(__name__)


class Base(DeclarativeBase):
    pass


class OperationRow(Base):
    __tablename__ = "operations"

    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    batch_id: Mapped[str] = mapped_column(String, nullable=False, index=True)
    position: Mapped[int] = mapped_column(Integer, nullable=False)
    kind: Mapped[str] = mapped_column(String, nullable=False)
    src_path: Mapped[str] = mapped_column(String, nullable=False)
    dest_path: Mapped[Optional[str]] = mapped_column(String, nullable=True)
    status: Mapped[str] = mapped_column(String, nullable=False)
    execution_hash: Mapped[str] = mapped_column(String, nullable=False)
    started_at: Mapped[Optional[datetime]] = mapped_column(DateTime(timezone=True),
                                                           nullable=True)
    finished_at: Mapped[Optional[datetime]] = mapped_column(DateTime(timezone=True),
                                                            nullable=True)
    reverted_at: Mapped[Optional[datetime]] = mapped_column(DateTime(timezone=True),
                                                            nullable=True)


@dataclass
class _ActionFields:
    kind: str
    src_path: Path
    dest_path: Optional[Path]


@beartype
def create_sqlite_engine(db_path: Path) -> Engine:
    return create_engine(f"sqlite+pysqlite:///{db_path}", future=True)


@beartype
def init_db(engine: Engine) -> None:
    Base.metadata.create_all(engine)


@beartype
def _now() -> datetime:
    return datetime.now(timezone.utc)


@beartype
def _extract_action_fields(action: object) -> _ActionFields:
    kind = str(getattr(action, "kind"))
    file_node = getattr(action, "file")
    src_path = Path(getattr(file_node, "path"))
    match kind:
        case "move":
            return _ActionFields(kind=kind,
                                 src_path=src_path,
                                 dest_path=Path(getattr(action, "dest")))
        case "trash":
            return _ActionFields(kind=kind, src_path=src_path, dest_path=None)
        case _:
            raise ValueError(f"Unsupported action kind: {kind}")


@beartype
def _operation_hash(fields: _ActionFields, trash_dir: Path) -> str:
    match fields.kind:
        case "move":
            payload = f"move|{fields.src_path}|{fields.dest_path}"
        case "trash":
            payload = f"trash|{fields.src_path}|{trash_dir}"
        case _:
            raise ValueError(f"Unsupported action kind: {fields.kind}")
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


@beartype
def verify_actions_consistency(actions: Sequence[object]) -> None:
    move_counts: dict[Path, int] = {}
    trash_paths: set[Path] = set()
    for action in actions:
        fields = _extract_action_fields(action)
        match fields.kind:
            case "move":
                move_counts[fields.src_path] = move_counts.get(fields.src_path, 0) + 1
            case "trash":
                trash_paths.add(fields.src_path)
            case _:
                raise ValueError(f"Unsupported action kind: {fields.kind}")

    for path, count in move_counts.items():
        if 1 < count:
            raise ValueError(f"Multiple move actions from the same source path: {path}")
        if path in trash_paths:
            raise ValueError(f"Conflicting move and trash action for source path: {path}")


@beartype
def register_actions(
    engine: Engine,
    actions: Sequence[object],
    trash_dir: Path,
    batch_id: str = "default",
) -> None:
    verify_actions_consistency(actions)
    with Session(engine) as session:
        for position, action in enumerate(actions):
            fields = _extract_action_fields(action)
            row = OperationRow(
                batch_id=batch_id,
                position=position,
                kind=fields.kind,
                src_path=str(fields.src_path),
                dest_path=str(fields.dest_path) if fields.dest_path is not None else None,
                status="pending",
                execution_hash=_operation_hash(fields, trash_dir),
                started_at=None,
                finished_at=None,
                reverted_at=None,
            )
            session.add(row)
        session.commit()


@beartype
def execute_pending(
    engine: Engine,
    trash_dir: Path,
    batch_id: str = "default",
    max_operations: Optional[int] = None,
) -> int:
    trash_dir.mkdir(parents=True, exist_ok=True)
    executed = 0
    with Session(engine) as session:
        statement = (select(OperationRow).where(OperationRow.batch_id == batch_id).where(
            OperationRow.status == "pending").order_by(OperationRow.position.asc(),
                                                       OperationRow.id.asc()))
        rows = list(session.scalars(statement))
        for row in rows:
            if max_operations is not None and max_operations <= executed:
                break
            row.started_at = _now()
            session.commit()
            logger.debug(f"Executing operation id={row.id} kind={row.kind}")
            match row.kind:
                case "move":
                    assert row.dest_path is not None
                    shutil.move(row.src_path, row.dest_path)
                case "trash":
                    src = Path(row.src_path)
                    target = Path(
                        row.dest_path
                    ) if row.dest_path is not None else trash_dir / f"{row.id}_{src.name}"
                    target.parent.mkdir(parents=True, exist_ok=True)
                    if row.dest_path is None:
                        row.dest_path = str(target)
                        session.commit()
                    shutil.move(row.src_path, str(target))
                case _:
                    raise ValueError(f"Unsupported operation kind: {row.kind}")
            row.status = "done"
            row.finished_at = _now()
            session.commit()
            executed += 1
    return executed


@beartype
def resume_execution(engine: Engine, trash_dir: Path, batch_id: str = "default") -> int:
    return execute_pending(engine=engine, trash_dir=trash_dir, batch_id=batch_id)


@beartype
def revert_done(engine: Engine, batch_id: str = "default") -> int:
    reverted = 0
    with Session(engine) as session:
        statement = (select(OperationRow).where(
            OperationRow.batch_id == batch_id).where(OperationRow.status == "done").where(
                OperationRow.reverted_at.is_(None)).order_by(OperationRow.position.desc(),
                                                             OperationRow.id.desc()))
        rows = list(session.scalars(statement))
        for row in rows:
            logger.debug(f"Reverting operation id={row.id} kind={row.kind}")
            assert row.dest_path is not None
            src = Path(row.src_path)
            src.parent.mkdir(parents=True, exist_ok=True)
            match row.kind:
                case "move":
                    shutil.move(row.dest_path, row.src_path)
                case "trash":
                    shutil.move(row.dest_path, row.src_path)
                case _:
                    raise ValueError(f"Unsupported operation kind: {row.kind}")
            row.reverted_at = _now()
            session.commit()
            reverted += 1
    return reverted
