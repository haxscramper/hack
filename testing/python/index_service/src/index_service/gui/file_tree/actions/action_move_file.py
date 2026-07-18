import hashlib
import shutil
from pathlib import Path

from beartype import beartype

from index_service.gui.file_tree.actions.action_db import OperationRow
from index_service.gui.file_tree.actions.action_handler import ActionHandler
from index_service.gui.file_tree.actions.action_list_model import MoveAction, BaseAction


class MoveActionHandler(ActionHandler):

    @beartype
    def __init__(self, dry_run: bool) -> None:
        self.dry_run = dry_run

    @beartype
    def do_action(self, row: OperationRow, action: BaseAction) -> None:
        assert isinstance(action, MoveAction)
        _ = row
        dest = action.dest
        assert dest is not None
        if self.dry_run:
            return
        shutil.move(str(action.file.path), str(dest))

    @beartype
    def undo_action(self, row: OperationRow, action: BaseAction) -> None:
        assert isinstance(action, MoveAction)
        _ = row
        if self.dry_run:
            return

        dest = action.file.path
        src = Path(action.dest)

        src.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(str(src), str(dest))

    @beartype
    def get_hash(self, action: BaseAction) -> str:
        assert isinstance(action, MoveAction)
        src = action.file.path
        dest = action.dest
        assert dest is not None
        payload = f"move|{src}|{dest}"
        return hashlib.sha256(payload.encode("utf-8")).hexdigest()

    @beartype
    def verify_consistency_single(self, action: BaseAction) -> None:
        assert isinstance(action, MoveAction)
        if action.dest is None:
            raise ValueError(f"Destination is set to None")

        dest = action.dest
        src = action.file.path

        if not Path(src).exists():
            raise ValueError(f"Move source path does not exist")

        if src == dest:
            raise ValueError(f"Move source and destination must differ: {src}")
