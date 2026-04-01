from dataclasses import dataclass
from pathlib import Path


@dataclass(slots=True)
class AppPaths:
    root_dir: Path
    sqlite_path: Path
    chroma_path: Path
