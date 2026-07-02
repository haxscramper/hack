import json
import logging
from datetime import datetime
from pathlib import Path

from beartype import beartype
from beartype.typing import Any


@beartype
def configure_logging() -> None:
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
    )


class JsonlFormatter(logging.Formatter):

    def format(self, record: logging.LogRecord) -> str:
        message = record.getMessage()
        payload: dict[str, Any] = {
            "timestamp": datetime.fromtimestamp(record.created).isoformat(),
            "level": record.levelname,
            "logger": record.name,
            "filename": record.filename,
            "lineno": record.lineno,
            "message": message,
        }

        if record.exc_info:
            payload["exception"] = self.formatException(record.exc_info)

        return json.dumps(payload, ensure_ascii=False)


def keep_last_files(directory: Path, pattern: str, keep: int) -> None:
    files = sorted(
        directory.glob(pattern),
        key=lambda path: path.stat().st_mtime,
        reverse=True,
    )

    for old_file in files[keep:]:
        old_file.unlink()
