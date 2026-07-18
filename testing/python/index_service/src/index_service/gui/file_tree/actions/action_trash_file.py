import hashlib
import shutil
from pathlib import Path

from beartype import beartype

from index_service.gui.file_tree.actions.action_db import OperationRow
from index_service.gui.file_tree.actions.action_handler import ActionHandler
from index_service.gui.file_tree.actions.action_list_model import BaseAction, TrashAction

import logging

log = logging.getLogger(__name__)


class TrashActionHandler(ActionHandler):

    @beartype
    def __init__(self, trash_root: Path, dry_run: bool) -> None:
        self.trash_root = trash_root
        self.dry_run = dry_run

    @beartype
    def do_action(self, row: OperationRow, action: BaseAction) -> None:
        assert isinstance(action, TrashAction)
        src = Path(action.file.path).absolute()
        dest = (self.trash_root / f"{row.id}_{src.name}").absolute()
        dest.parent.mkdir(parents=True, exist_ok=True)

        log.info(f"do trash: executing move({src} -> {dest})")

        if self.dry_run:
            return

        shutil.move(str(src), str(dest))

    @beartype
    def undo_action(self, row: OperationRow, action: BaseAction) -> None:
        assert isinstance(action, TrashAction)
        src = Path(action.file.path).absolute()
        dest = Path(self.trash_root / f"{row.id}_{src.name}").absolute()

        log.info(f"undo trash: executing move({dest} -> {src})")

        if self.dry_run:
            return

        src.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(str(dest), str(src))

    @beartype
    def get_hash(self, action: BaseAction) -> str:
        assert isinstance(action, TrashAction)
        src = action.file.path
        payload = f"trash|{src}|{self.trash_root}"
        return hashlib.sha256(payload.encode("utf-8")).hexdigest()

    @beartype
    def verify_consistency_single(self, action: BaseAction) -> None:
        assert isinstance(action, TrashAction)
        src = action.file.path
        if src == self.trash_root:
            raise ValueError(f"Trash source cannot be trash root: {src}")
