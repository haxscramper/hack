import json
from dataclasses import dataclass
from pathlib import Path

from beartype import beartype
from beartype.typing import Optional, Sequence
from pydantic import BaseModel
from sqlalchemy import MetaData, Column, Text, Table, select
from sqlalchemy.dialects.sqlite import insert as sqlite_insert

from index_service.gui.file_tree.columns.file_tree_column import FileTreeColumnSpec, FileTreeNode
from index_service.services.pydantic_utils import model_to_json_data, model_from_json_data
from index_service.services.utils import create_cache_engine

CACHE_USER_EDIT_TABLE = "file_tree_user_edits"


@dataclass(slots=True, frozen=True)
class _UserEditRow:
    root: str
    relative: str
    column_name: str
    json_data: str


def _build_user_edit_table(metadata: MetaData) -> Table:
    return Table(
        CACHE_USER_EDIT_TABLE,
        metadata,
        Column("root", Text, primary_key=True),
        Column("relative", Text, primary_key=True),
        Column("column_name", Text, primary_key=True),
        Column("json_data", Text, nullable=False),
    )


@beartype
def store_user_column_edit(
    user_edit_path: Path,
    root: str,
    relative: str,
    column: FileTreeColumnSpec,
    data: Optional[BaseModel],
) -> None:
    engine = create_cache_engine(user_edit_path)
    metadata = MetaData()
    user_edit_table = _build_user_edit_table(metadata)
    metadata.create_all(engine)

    json_data = json.dumps(
        None if data is None else model_to_json_data(data),
        separators=(",", ":"),
    )

    insert_stmt = sqlite_insert(user_edit_table)
    upsert_stmt = insert_stmt.on_conflict_do_update(
        index_elements=[
            user_edit_table.c.root,
            user_edit_table.c.relative,
            user_edit_table.c.column_name,
        ],
        set_={"json_data": insert_stmt.excluded.json_data},
    )

    with engine.begin() as connection:
        connection.execute(
            upsert_stmt,
            [{
                "root": root,
                "relative": relative,
                "column_name": column.column_name,
                "json_data": json_data,
            }],
        )

    engine.dispose()


def load_user_edits(
    user_edit_path: Path,
    columns: Sequence[FileTreeColumnSpec],
) -> list[_UserEditRow]:
    engine = create_cache_engine(user_edit_path)
    metadata = MetaData()
    user_edit_table = _build_user_edit_table(metadata)
    metadata.create_all(engine)

    allowed_columns = [column.column_name for column in columns]
    if not allowed_columns:
        engine.dispose()
        return []

    query = select(
        user_edit_table.c.root,
        user_edit_table.c.relative,
        user_edit_table.c.column_name,
        user_edit_table.c.json_data,
    ).where(user_edit_table.c.column_name.in_(allowed_columns))

    with engine.connect() as connection:
        rows = [
            _UserEditRow(
                root=row.root,
                relative=row.relative,
                column_name=row.column_name,
                json_data=row.json_data,
            ) for row in connection.execute(query)
        ]

    engine.dispose()
    return rows


def apply_user_edits(
    nodes: Sequence[FileTreeNode],
    columns: Sequence[FileTreeColumnSpec],
    user_edit_rows: Sequence[_UserEditRow],
) -> None:
    edits_by_key = {
        (row.root, row.relative, row.column_name): row.json_data for row in user_edit_rows
    }

    column_by_name = {column.column_name: column for column in columns}

    def apply_to_node(node: FileTreeNode) -> None:
        if node.root is None:
            for nested_node in node.nested:
                apply_to_node(nested_node)
            return

        for column_name, column in column_by_name.items():
            key = (node.root, node.root_relative, column_name)
            if key not in edits_by_key:
                continue

            json_data = json.loads(edits_by_key[key])
            node.columns[column_name] = (None
                                         if json_data is None else model_from_json_data(
                                             json_data, column.column_type))

        for nested_node in node.nested:
            apply_to_node(nested_node)

    for node in nodes:
        apply_to_node(node)
