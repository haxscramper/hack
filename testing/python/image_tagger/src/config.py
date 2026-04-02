import logging
from pathlib import Path
from pydantic import BaseModel, Field


class AppConfig(BaseModel):
    IMAGE_EXTENSIONS: set[str] = {".jpg", ".jpeg", ".png", ".bmp", ".webp", ".gif"}
    SQLITE_FILENAME: str = "image_tagger.sqlite"
    CHROMA_DIRNAME: str = ".image_tagger_chroma"
    THUMBNAIL_SIZE: int = 160
    PREVIEW_GRID_COLUMNS: int = 3
    excluded_directories: set[str] = Field(default_factory=set)


config = AppConfig()
_config_file: Path | None = None


def init_config(root_dir: Path):
    global config, _config_file
    _config_file = root_dir / "config.json"
    if _config_file.exists():
        loaded_data = _config_file.read_text()
        logging.info(f"Loading config from {_config_file}")
        config = AppConfig.model_validate_json(loaded_data)
    else:
        config = AppConfig()
        _config_file.write_text(config.model_dump_json(indent=4))


def save_config():
    if _config_file:
        _config_file.write_text(config.model_dump_json(indent=4))
