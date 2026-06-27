import logging
from typing import Any

_installed = False
_original_handle = logging.Logger.handle


def disable_dagster_logs_forever() -> None:
    global _installed
    if _installed:
        return
    _installed = True

    def _handle(self: logging.Logger, record: logging.LogRecord) -> Any:
        if record.name.startswith("dagster"):
            return None
        return _original_handle(self, record)

    logging.Logger.handle = _handle


disable_dagster_logs_forever()


def main() -> None:
    print("Hello from back-indexer!")
