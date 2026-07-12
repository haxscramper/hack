import enum

from PyQt6.QtCore import Qt


class CustomModelRole(enum):
    HashRole = Qt.ItemDataRole.UserRole + 1
    PathRole = Qt.ItemDataRole.UserRole + 2
    ExtraRole = Qt.ItemDataRole.UserRole + 3
