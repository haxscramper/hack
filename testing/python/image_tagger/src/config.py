from pathlib import Path
from pydantic import BaseModel, Field

CONFIG_FILE = Path("config.json")


class AppConfig(BaseModel):
    IMAGE_EXTENSIONS: set[str] = {".jpg", ".jpeg", ".png", ".bmp", ".webp", ".gif"}
    SQLITE_FILENAME: str = "image_tagger.sqlite"
    CHROMA_DIRNAME: str = ".image_tagger_chroma"
    THUMBNAIL_SIZE: int = 160
    PREVIEW_GRID_COLUMNS: int = 3
    excluded_directories: set[str] = Field(default_factory=set)

    @classmethod
    def load(cls) -> "AppConfig":
        if CONFIG_FILE.exists():
            return cls.model_validate_json(CONFIG_FILE.read_text())
        return cls()

    def save(self):
        CONFIG_FILE.write_text(self.model_dump_json(indent=4))


config = AppConfig.load()

# Provide global aliases for compatibility
IMAGE_EXTENSIONS = config.IMAGE_EXTENSIONS
SQLITE_FILENAME = config.SQLITE_FILENAME
CHROMA_DIRNAME = config.CHROMA_DIRNAME
THUMBNAIL_SIZE = config.THUMBNAIL_SIZE
PREVIEW_GRID_COLUMNS = config.PREVIEW_GRID_COLUMNS
