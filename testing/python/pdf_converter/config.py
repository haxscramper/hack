from pydantic import BaseModel, Field, PrivateAttr
from typing import List, Optional
from pathlib import Path
import json


class AppConfig(BaseModel):
    input_dirs: List[Path] = Field(
        default_factory=list,
        description="List of input directories or individual PDF files.",
    )
    output_dir: Path = Field(
        description="The single output directory where sub-directories for each PDF will be created."
    )
    ocr_only_files: List[str] = Field(
        default_factory=list,
        description="List of PDF file paths to process with OCR only.",
    )

    _config_path: Optional[Path] = PrivateAttr(default=None)

    @classmethod
    def load_from_json(cls, config_path: Path | str) -> "AppConfig":
        with open(config_path, "r") as f:
            data = json.load(f)
        obj = cls(**data)
        obj._config_path = Path(config_path)
        return obj

    def save_to_json(self) -> None:
        if self._config_path:
            with open(self._config_path, "w") as f:
                f.write(self.model_dump_json(indent=4))
