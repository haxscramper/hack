from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable

from PyQt6.QtCore import (
    QAbstractItemModel,
    QAbstractProxyModel,
    QModelIndex,
    Qt,
    QSettings,
    QSize,
)
from PyQt6.QtWidgets import QStyledItemDelegate, QStyleOptionViewItem


@dataclass
class ModelProxyRecord:
    index: QModelIndex


@dataclass
class IndexRoleRepr:
    role_name: str
    role_value: str


@dataclass
class ModelLevelRecord:
    depth: int
    proxies: list[ModelProxyRecord] = field(default_factory=list)
    final_repr: str = ""
    roles: list[IndexRoleRepr] = field(default_factory=list)


def _role_name(role_name: bytes) -> str:
    return bytes(role_name).decode("utf-8", errors="replace")


def _format_value(value: object) -> str:
    if isinstance(value, str):
        return value

    return repr(value)


def _index_identity(index: QModelIndex) -> str:
    if not index.isValid():
        return "0x0"

    return f"0x{index.internalId():x}"


def _recurse_tree(
    index: QModelIndex,
    level: int,
    role_names: dict[int, bytes],
    records: list[ModelLevelRecord],
    ignore_exceptions: bool,
    model: QAbstractItemModel,
    to_string: Callable[[QModelIndex], str] | None,
    max_depth: int | None,
) -> None:
    if max_depth is not None and level > max_depth:
        return

    record = ModelLevelRecord(depth=level)

    for role in sorted(role_names):
        role_repr = IndexRoleRepr(role_name=_role_name(role_names[role]), role_value="")

        def read_role() -> None:
            value = index.data(role)
            if value is not None:
                role_repr.role_value = _format_value(value)
                record.roles.append(role_repr)

        if ignore_exceptions:
            try:
                read_role()
            except Exception as error:
                role_repr.role_value = (f"Exception {type(error).__name__} {error}")
                record.roles.append(role_repr)
        else:
            read_role()

    current_index = index
    current_proxy_model = (current_index.model() if isinstance(
        current_index.model(), QAbstractProxyModel) else None)

    record.proxies.append(ModelProxyRecord(index=current_index))

    while current_proxy_model is not None:
        mapped_index = current_proxy_model.mapToSource(current_index)
        if not mapped_index.isValid():
            break

        current_index = mapped_index
        record.proxies.append(ModelProxyRecord(index=current_index))

        source_model = current_proxy_model.sourceModel()
        if isinstance(source_model, QAbstractProxyModel):
            current_proxy_model = source_model
        else:
            current_proxy_model = None

    if to_string is not None:
        record.final_repr = str(to_string(index))

    records.append(record)

    row_count = model.rowCount(index)
    for row in range(row_count):
        child_index = model.index(row, 0, index)
        _recurse_tree(
            index=child_index,
            level=level + 1,
            role_names=role_names,
            records=records,
            ignore_exceptions=ignore_exceptions,
            model=model,
            to_string=to_string,
            max_depth=max_depth,
        )


def _format_records(records: list[ModelLevelRecord]) -> str:
    model_names: dict[int, str] = {}

    for record in records:
        for proxy in record.proxies:
            model = proxy.index.model()
            model_id = id(model)

            if model_id in model_names:
                continue

            if model is None:
                model_names[model_id] = "0x0"
                continue

            object_name = model.objectName()
            if object_name:
                model_names[model_id] = object_name
            else:
                model_names[model_id] = f"M{len(model_names)}"

    lines: list[str] = []

    for record in records:
        prefix = " " * (record.depth * 2)

        proxy_parts: list[str] = []
        for proxy in record.proxies:
            index = proxy.index
            model = index.model()
            model_name = model_names[id(model)]

            proxy_parts.append(f"[{index.row()}:{index.column()}, "
                               f"{_index_identity(index)}, {model_name}]")

        line = prefix + " " + "->".join(proxy_parts)

        final_repr_lines = record.final_repr.split("\n")
        if len(final_repr_lines) == 1:
            if final_repr_lines[0]:
                line += f" {final_repr_lines[0]!r}"
            lines.append(line)
        else:
            lines.append(line)
            for final_repr_line in final_repr_lines:
                lines.append(f"{' ' * (record.depth * 2 + 2)}"
                             f">> {final_repr_line}")

        for role in record.roles:
            role_prefix = " " * (record.depth * 2 + 3)
            if "\n" not in role.role_value:
                lines.append(f"{role_prefix}{role.role_name} = {role.role_value}")
                continue

            lines.append(f"{role_prefix}{role.role_name} =")
            for role_value_line in role.role_value.split("\n"):
                lines.append(f"{role_prefix}| {role_value_line}")

    return "\n".join(lines)


def print_model_tree(
    model: QAbstractItemModel | None,
    parent: QModelIndex | None = None,
    to_string: Callable[[QModelIndex], str] | None = None,
    ignore_exceptions: bool = False,
    max_depth: int | None = None,
) -> str:
    """Return a textual representation of a Qt item model tree.

    Proxy-model indexes are followed through their source-model chain.
    """

    if model is None:
        return ""

    if parent is None:
        parent = QModelIndex()

    role_names = {
        int(role): name
        for role, name in model.roleNames().items()
        if (int(role) == int(Qt.ItemDataRole.DisplayRole) or int(role) == int(
            Qt.ItemDataRole.WhatsThisRole) or int(role) >= int(Qt.ItemDataRole.UserRole))
    }

    records: list[ModelLevelRecord] = []

    _recurse_tree(
        index=parent,
        level=0,
        role_names=role_names,
        records=records,
        ignore_exceptions=ignore_exceptions,
        model=model,
        to_string=to_string,
        max_depth=max_depth,
    )

    return _format_records(records)


def get_settings() -> QSettings:
    return QSettings()
