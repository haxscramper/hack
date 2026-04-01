from __future__ import annotations

from pathlib import Path


class OllamaTagger:
    """
    Placeholder service.
    Replace internals with actual ollama calls.
    """

    def __init__(self, model_name: str = "llava"):
        self.model_name = model_name

    def regular_tags(self, image_path: Path) -> list[tuple[str, str]]:
        # Example output format: [("object", "tree"), ("style", "outdoor")]
        return []

    def describe(self, image_path: Path) -> str:
        return ""
