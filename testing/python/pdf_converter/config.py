from pydantic import BaseModel, Field
from typing import List
from pathlib import Path
import json

class AppConfig(BaseModel):
    input_dirs: List[Path] = Field(default_factory=list, description="List of input directories or individual PDF files.")
    output_dir: Path = Field(description="The single output directory where sub-directories for each PDF will be created.")

    @classmethod
    def load_from_json(cls, config_path: Path | str) -> 'AppConfig':
        with open(config_path, 'r') as f:
            data = json.load(f)
        return cls(**data)
