from __future__ import annotations

import importlib
import logging
from datetime import datetime, timezone
from pathlib import Path

from beartype import beartype
from beartype.typing import Any, Optional, Sequence, TypeVar
from pydantic import BaseModel, field_validator
from sqlalchemy import create_engine, select
from sqlalchemy.orm import Session

from index_service.gui.file_tree.actions.action_db import OperationRow
from index_service.gui.file_tree.actions.action_handler import ActionHandler
from index_service.gui.file_tree.actions.action_list_model import BaseAction
from index_service.gui.file_tree.actions.action_move_file import MoveActionHandler
from index_service.gui.file_tree.actions.action_trash_file import TrashActionHandler
from index_service.services.pydantic_utils import model_to_json_data, model_from_json_data

log = logging.getLogger(__name__)

T = TypeVar("T", bound=BaseModel)


@beartype
def _model_type_to_name(model_type: type[BaseAction]) -> str:
    return f"{model_type.__module__}:{model_type.__qualname__}"


_PENDING_STR = "pending"
_DONE_STR = "done"


@beartype
def _model_type_from_name(type_name: str) -> type[BaseAction]:
    module_name, qualname = type_name.split(":", maxsplit=1)
    module = importlib.import_module(module_name)

    obj: Any = module
    for attr in qualname.split("."):
        obj = getattr(obj, attr)

    if not isinstance(obj, type) or not issubclass(obj, BaseAction):
        raise TypeError(f"Resolved type is not a BaseAction subclass: {type_name}")

    return obj


class ActionExecutionConfig(BaseModel):
    trash_root: Path
    sqlite_path: Path
    dry_run: bool = True

    @field_validator("trash_root")
    @classmethod
    def _normalize_trash_root_file(cls, value: Path | None) -> Path | None:
        if value is None:
            return None
        return value.expanduser().resolve().absolute()

    @field_validator("sqlite_path")
    @classmethod
    def _normalize_sqlite_path_file(cls, value: Path | None) -> Path | None:
        if value is None:
            return None
        return value.expanduser().resolve().absolute()


@beartype
def _now() -> datetime:
    return datetime.now(timezone.utc)


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
        from index_service.gui.file_tree.actions.action_db import Base
        Base.metadata.create_all(self.engine)

    @beartype
    def verify_actions_consistency(self, actions: Sequence[BaseAction]) -> None:
        move_counts: dict[Path, int] = {}
        trash_paths: set[Path] = set()

        for action in actions:
            kind = action.kind
            if action.kind not in self.handlers:
                raise ValueError(f"Unsupported action kind: {kind}")
            self.handlers[kind].verify_consistency_single(action)
            src = action.file.path

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
    def register_actions(self, actions: Sequence[BaseAction]) -> None:
        self.verify_actions_consistency(actions)

        # Deduplicate incoming actions by hash and ensure type consistency in-batch.
        incoming_by_hash: dict[str, tuple[BaseAction, str, str]] = {}
        for action in actions:
            kind = action.kind
            action_type = _model_type_to_name(type(action))
            execution_hash = self.handlers[kind].get_hash(action)

            existing_in_batch = incoming_by_hash.get(execution_hash)
            if existing_in_batch is not None:
                _, existing_type, _ = existing_in_batch
                if existing_type != action_type:
                    raise ValueError(
                        f"Conflicting action types for hash '{execution_hash}': "
                        f"'{existing_type}' vs '{action_type}'")
                continue

            incoming_by_hash[execution_hash] = (action, action_type, kind)

        if not incoming_by_hash:
            return

        with Session(self.engine) as session:
            existing_rows = session.execute(
                select(OperationRow.execution_hash, OperationRow.action_type).where(
                    OperationRow.execution_hash.in_(list(
                        incoming_by_hash.keys())))).all()
            existing_by_hash = {
                row.execution_hash: row.action_type for row in existing_rows
            }

            for execution_hash, existing_type in existing_by_hash.items():
                _, incoming_type, _ = incoming_by_hash[execution_hash]
                if existing_type != incoming_type:
                    raise ValueError(
                        f"Hash '{execution_hash}' already exists with type '{existing_type}', "
                        f"cannot register action of type '{incoming_type}'")

            for execution_hash, (action, action_type, kind) in incoming_by_hash.items():
                if execution_hash in existing_by_hash:
                    continue

                session.add(
                    OperationRow(
                        kind=kind,
                        action_type=action_type,
                        action_data=model_to_json_data(action),
                        status=_PENDING_STR,
                        execution_hash=execution_hash,
                        started_at=None,
                        finished_at=None,
                        reverted_at=None,
                    ))

            session.commit()

    @beartype
    def _load_action(self, row: OperationRow) -> BaseAction:
        model_type = _model_type_from_name(row.action_type)
        return model_from_json_data(row.action_data, model_type)

    @beartype
    def execute_pending(self, max_operations: Optional[int] = None) -> int:
        self.config.trash_root.mkdir(parents=True, exist_ok=True)
        executed = 0
        with Session(self.engine) as session:
            rows = list(
                session.scalars(
                    select(OperationRow).where(
                        OperationRow.status == _PENDING_STR).order_by(
                            OperationRow.id.asc())))
            for row in rows:
                if max_operations is not None and max_operations <= executed:
                    break
                action = self._load_action(row)
                row.started_at = _now()
                session.commit()
                self.handlers[row.kind].do_action(row, action)
                row.status = _DONE_STR
                row.finished_at = _now()
                session.commit()
                executed += 1
        return executed

    @beartype
    def revert_done(self) -> int:
        reverted = 0
        with Session(self.engine) as session:
            rows = list(
                session.scalars(
                    select(OperationRow).where(OperationRow.status == _DONE_STR).order_by(
                        OperationRow.id.desc())))
            for row in rows:
                action = self._load_action(row)
                self.handlers[row.kind].undo_action(row, action)
                row.status = _PENDING_STR
                row.started_at = None
                row.finished_at = None
                session.commit()
                reverted += 1
        return reverted
