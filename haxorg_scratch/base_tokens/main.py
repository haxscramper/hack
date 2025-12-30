#!/usr/bin/env python
import sys
from pydantic import BaseModel
from typing import List, Optional, Any
import os

from PySide6.QtCore import QUrl, QObject, Property  # type: ignore[import-not-found]
from PySide6.QtWidgets import QApplication  # type: ignore[import-not-found]
from PySide6.QtQml import QQmlApplicationEngine, QQmlContext  # type: ignore[import-not-found]
# trunk-ignore(ruff/F401)
from PySide6.QtQml import QQmlDebuggingEnabler

# debug = QQmlDebuggingEnabler()


class Token(BaseModel):
    col: int
    kind: str
    line: int
    text: str


class TokenList(BaseModel):
    tokens: List[Token]


class TokenProvider(QObject):

    def __init__(self) -> None:
        super().__init__()
        self._tokenRows: List[Any] = []

        with open("/tmp/token.json", "r") as f:
            data = TokenList.model_validate_json(f.read())

        current_row = []
        for token in data.tokens:
            current_row.append(token.model_dump())
            if token.kind == "Newline":
                self._tokenRows.append(current_row)
                current_row = []

        if current_row:
            self._tokenRows.append(current_row)

    @Property('QVariantList', constant=True)
    def tokenRows(self) -> List[Any]:
        return self._tokenRows


app = QApplication(sys.argv)

engine = QQmlApplicationEngine()
tokenProvider = TokenProvider()
ctx: Optional[QQmlContext] = engine.rootContext()
if ctx:
    ctx.setContextProperty('tokenProvider', tokenProvider)

    engine.load(QUrl(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                  'main.qml')))
    if not engine.rootObjects():
        sys.exit(-1)
    sys.exit(app.exec())
