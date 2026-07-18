from __future__ import annotations

import hashlib
import logging
import shutil
from abc import ABC, abstractmethod
from datetime import datetime, timezone
from pathlib import Path

from beartype import beartype
from beartype.typing import Optional, Sequence
from pydantic import BaseModel
from sqlalchemy import DateTime, Integer, String, create_engine, select
from sqlalchemy.orm import DeclarativeBase, Mapped, Session, mapped_column

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
)
logger = logging.getLogger(__name__)


class ActionExecutionConfig(BaseModel):
    trash_root: Path
    sqlite_path: Path
    dry_run: bool = False


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


class ActionHandler(ABC):

    @abstractmethod
    def do_action(self, row: OperationRow) -> None:
        raise NotImplementedError

    @abstractmethod
    def undo_action(self, row: OperationRow) -> None:
        raise NotImplementedError

    @abstractmethod
    def get_hash(self, action: object) -> str:
        raise NotImplementedError

    @abstractmethod
    def verify_consistency_single(self, action: object) -> None:
        raise NotImplementedError


@beartype
def _now() -> datetime:
    return datetime.now(timezone.utc)


@beartype
def _action_kind(action: object) -> str:
    return str(getattr(action, "kind"))


@beartype
def _action_src_path(action: object) -> Path:
    file_node = getattr(action, "file")
    return Path(getattr(file_node, "path"))


@beartype
def _action_dest_path(action: object) -> Optional[Path]:
    kind = _action_kind(action)
    match kind:
        case "move":
            return Path(getattr(action, "dest"))
        case "trash":
            return None
        case _:
            raise ValueError(f"Unsupported action kind: {kind}")


class MoveActionHandler(ActionHandler):

    @beartype
    def __init__(self, dry_run: bool) -> None:
        self.dry_run = dry_run

    @beartype
    def do_action(self, row: OperationRow) -> None:
        assert row.dest_path is not None
        if self.dry_run:
            return
        shutil.move(row.src_path, row.dest_path)

    @beartype
    def undo_action(self, row: OperationRow) -> None:
        assert row.dest_path is not None
        if self.dry_run:
            return
        src = Path(row.src_path)
        src.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(row.dest_path, row.src_path)

    @beartype
    def get_hash(self, action: object) -> str:
        src = _action_src_path(action)
        dest = _action_dest_path(action)
        assert dest is not None
        payload = f"move|{src}|{dest}"
        return hashlib.sha256(payload.encode("utf-8")).hexdigest()

    @beartype
    def verify_consistency_single(self, action: object) -> None:
        src = _action_src_path(action)
        dest = _action_dest_path(action)
        assert dest is not None
        if src == dest:
            raise ValueError(f"Move source and destination must differ: {src}")


class TrashActionHandler(ActionHandler):

    @beartype
    def __init__(self, trash_root: Path, dry_run: bool) -> None:
        self.trash_root = trash_root
        self.dry_run = dry_run

    @beartype
    def do_action(self, row: OperationRow) -> None:
        src = Path(row.src_path)
        dest = self.trash_root / f"{row.id}_{src.name}"
        dest.parent.mkdir(parents=True, exist_ok=True)
        row.dest_path = str(dest)
        if self.dry_run:
            return
        shutil.move(row.src_path, str(dest))

    @beartype
    def undo_action(self, row: OperationRow) -> None:
        assert row.dest_path is not None
        if self.dry_run:
            return
        src = Path(row.src_path)
        src.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(row.dest_path, row.src_path)

    @beartype
    def get_hash(self, action: object) -> str:
        src = _action_src_path(action)
        payload = f"trash|{src}|{self.trash_root}"
        return hashlib.sha256(payload.encode("utf-8")).hexdigest()

    @beartype
    def verify_consistency_single(self, action: object) -> None:
        src = _action_src_path(action)
        if src == self.trash_root:
            raise ValueError(f"Trash source cannot be trash root: {src}")


class ActionExecutor:

    @beartype
    def __init__(self, config: ActionExecutionConfig) -> None:
        self.config = config
        self.engine = create_engine(f"sqlite+pysqlite:///{self.config.sqlite_path}",
                                    future=True)
        self.handlers: dict[str, ActionHandler] = {
            "move":
                MoveActionHandler(dry_run=self.config.dry_run),
            "trash":
                TrashActionHandler(trash_root=self.config.trash_root,
                                   dry_run=self.config.dry_run),
        }

    @beartype
    def init_db(self) -> None:
        Base.metadata.create_all(self.engine)

    @beartype
    def verify_actions_consistency(self, actions: Sequence[object]) -> None:
        move_counts: dict[Path, int] = {}
        trash_paths: set[Path] = set()

        for action in actions:
            kind = _action_kind(action)
            if kind not in self.handlers:
                raise ValueError(f"Unsupported action kind: {kind}")
            self.handlers[kind].verify_consistency_single(action)
            src = _action_src_path(action)

            match kind:
                case "move":
                    move_counts[src] = move_counts.get(src, 0) + 1
                case "trash":
                    trash_paths.add(src)
                case _:
                    raise ValueError(f"Unsupported action kind: {kind}")

        for src, count in move_counts.items():
            if 1 < count:
                raise ValueError(
                    f"Multiple move actions from the same source path: {src}")
            if src in trash_paths:
                raise ValueError(
                    f"Conflicting move and trash action for source path: {src}")

    @beartype
    def register_actions(self,
                         actions: Sequence[object],
                         batch_id: str = "default") -> None:
        self.verify_actions_consistency(actions)
        with Session(self.engine) as session:
            for position, action in enumerate(actions):
                kind = _action_kind(action)
                src = _action_src_path(action)
                dest = _action_dest_path(action)
                row = OperationRow(
                    batch_id=batch_id,
                    position=position,
                    kind=kind,
                    src_path=str(src),
                    dest_path=str(dest) if dest is not None else None,
                    status="pending",
                    execution_hash=self.handlers[kind].get_hash(action),
                    started_at=None,
                    finished_at=None,
                    reverted_at=None,
                )
                session.add(row)
            session.commit()

    @beartype
    def execute_pending(self,
                        batch_id: str = "default",
                        max_operations: Optional[int] = None) -> int:
        self.config.trash_root.mkdir(parents=True, exist_ok=True)
        executed = 0
        with Session(self.engine) as session:
            rows = list(
                session.scalars(
                    select(OperationRow).where(OperationRow.batch_id == batch_id).where(
                        OperationRow.status == "pending").order_by(
                            OperationRow.position.asc(), OperationRow.id.asc())))
            for row in rows:
                if max_operations is not None and max_operations <= executed:
                    break
                row.started_at = _now()
                session.commit()
                self.handlers[row.kind].do_action(row)
                row.status = "done"
                row.finished_at = _now()
                session.commit()
                executed += 1
        return executed

    @beartype
    def resume_execution(self, batch_id: str = "default") -> int:
        return self.execute_pending(batch_id=batch_id)

    @beartype
    def revert_done(self, batch_id: str = "default") -> int:
        reverted = 0
        with Session(self.engine) as session:
            rows = list(
                session.scalars(
                    select(OperationRow).where(OperationRow.batch_id == batch_id).where(
                        OperationRow.status == "done").where(
                            OperationRow.reverted_at.is_(None)).order_by(
                                OperationRow.position.desc(), OperationRow.id.desc())))
            for row in rows:
                self.handlers[row.kind].undo_action(row)
                row.reverted_at = _now()
                session.commit()
                reverted += 1
        return reverted
