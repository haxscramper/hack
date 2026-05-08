from __future__ import annotations

from typing import Optional

from PySide6.QtCore import QMimeData, QByteArray, QDataStream, QIODevice

from ascii_diagram.model.enums import ShapeType

PALETTE_MIME = "application/x-ascii-diagram-palette"


def encode_palette_mime(shape_type: ShapeType) -> QMimeData:
    data = QMimeData()
    encoded = QByteArray()
    stream = QDataStream(encoded, QIODevice.WriteOnly)
    stream.writeInt32(shape_type.value)
    data.setData(PALETTE_MIME, encoded)
    return data


def decode_palette_mime(data: QMimeData) -> Optional[ShapeType]:
    if not data.hasFormat(PALETTE_MIME):
        return None
    raw = data.data(PALETTE_MIME)
    stream = QDataStream(raw, QIODevice.ReadOnly)
    val = stream.readInt32()
    try:
        return ShapeType(val)
    except ValueError:
        return None