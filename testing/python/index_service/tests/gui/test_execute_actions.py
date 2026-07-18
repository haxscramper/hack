from __future__ import annotations

from pathlib import Path

from beartype.typing import Annotated, Literal, Optional, Union
from pydantic import BaseModel, Field
from sqlalchemy import select
from sqlalchemy.orm import Session

from index_service.gui.file_tree.actions.action_execute import create_sqlite_engine, init_db, register_actions, \
    execute_pending, OperationRow, resume_execution, revert_done
from index_service.gui.file_tree.actions.action_list_model import MoveAction, TrashAction, Action
from index_service.gui.file_tree.columns.file_tree_column import FileTreeNode


def test_execute_move_and_trash(tmp_path: Path) -> None:
    source_move = tmp_path / "a.txt"
    source_trash = tmp_path / "b.txt"
    move_dest = tmp_path / "moved" / "a.txt"
    trash_dir = tmp_path / "trash"
    db_path = tmp_path / "ops.sqlite"

    source_move.parent.mkdir(parents=True, exist_ok=True)
    source_move.write_text("move", encoding="utf-8")
    source_trash.write_text("trash", encoding="utf-8")
    move_dest.parent.mkdir(parents=True, exist_ok=True)

    actions: list[Action] = [
        MoveAction(file=FileTreeNode(path=source_move, is_directory=False),
                   dest=str(move_dest)),
        TrashAction(file=FileTreeNode(path=source_trash, is_directory=False)),
    ]

    engine = create_sqlite_engine(db_path)
    init_db(engine)
    register_actions(engine=engine,
                     actions=actions,
                     trash_dir=trash_dir,
                     batch_id="batch")

    executed = execute_pending(engine=engine, trash_dir=trash_dir, batch_id="batch")
    assert executed == 2
    assert move_dest.exists()
    assert source_move.exists() is False
    assert source_trash.exists() is False

    with Session(engine) as session:
        rows = list(
            session.scalars(
                select(OperationRow).where(OperationRow.batch_id == "batch").order_by(
                    OperationRow.position.asc(), OperationRow.id.asc())))
        assert len(rows) == 2
        assert rows[0].status == "done"
        assert rows[1].status == "done"
        assert rows[0].started_at is not None
        assert rows[0].finished_at is not None
        assert rows[1].started_at is not None
        assert rows[1].finished_at is not None
        assert rows[1].dest_path is not None
        assert Path(rows[1].dest_path).exists()


def test_resume_execution(tmp_path: Path) -> None:
    source_move = tmp_path / "resume_a.txt"
    source_trash = tmp_path / "resume_b.txt"
    move_dest = tmp_path / "resume_moved" / "resume_a.txt"
    trash_dir = tmp_path / "resume_trash"
    db_path = tmp_path / "resume.sqlite"

    source_move.write_text("move", encoding="utf-8")
    source_trash.write_text("trash", encoding="utf-8")
    move_dest.parent.mkdir(parents=True, exist_ok=True)

    actions: list[Action] = [
        MoveAction(file=FileTreeNode(path=source_move, is_directory=False),
                   dest=str(move_dest)),
        TrashAction(file=FileTreeNode(path=source_trash, is_directory=False)),
    ]

    engine = create_sqlite_engine(db_path)
    init_db(engine)
    register_actions(engine=engine,
                     actions=actions,
                     trash_dir=trash_dir,
                     batch_id="batch")

    first_executed = execute_pending(engine=engine,
                                     trash_dir=trash_dir,
                                     batch_id="batch",
                                     max_operations=1)
    assert first_executed == 1

    with Session(engine) as session:
        rows = list(
            session.scalars(
                select(OperationRow).where(OperationRow.batch_id == "batch").order_by(
                    OperationRow.position.asc(), OperationRow.id.asc())))
        assert rows[0].status == "done"
        assert rows[1].status == "pending"

    resumed = resume_execution(engine=engine, trash_dir=trash_dir, batch_id="batch")
    assert resumed == 1
    assert move_dest.exists()
    assert source_move.exists() is False
    assert source_trash.exists() is False


def test_revert_done(tmp_path: Path) -> None:
    source_move = tmp_path / "rev_a.txt"
    source_trash = tmp_path / "rev_b.txt"
    move_dest = tmp_path / "rev_moved" / "rev_a.txt"
    trash_dir = tmp_path / "rev_trash"
    db_path = tmp_path / "revert.sqlite"

    source_move.write_text("move", encoding="utf-8")
    source_trash.write_text("trash", encoding="utf-8")
    move_dest.parent.mkdir(parents=True, exist_ok=True)

    actions: list[Action] = [
        MoveAction(file=FileTreeNode(path=source_move, is_directory=False),
                   dest=str(move_dest)),
        TrashAction(file=FileTreeNode(path=source_trash, is_directory=False)),
    ]

    engine = create_sqlite_engine(db_path)
    init_db(engine)
    register_actions(engine=engine,
                     actions=actions,
                     trash_dir=trash_dir,
                     batch_id="batch")
    execute_pending(engine=engine, trash_dir=trash_dir, batch_id="batch")

    reverted = revert_done(engine=engine, batch_id="batch")
    assert reverted == 2
    assert source_move.exists()
    assert source_trash.exists()
    assert move_dest.exists() is False

    with Session(engine) as session:
        rows = list(
            session.scalars(
                select(OperationRow).where(OperationRow.batch_id == "batch").order_by(
                    OperationRow.position.asc(), OperationRow.id.asc())))
        assert rows[0].reverted_at is not None
        assert rows[1].reverted_at is not None
        assert rows[1].dest_path is not None
        assert Path(rows[1].dest_path).exists() is False
