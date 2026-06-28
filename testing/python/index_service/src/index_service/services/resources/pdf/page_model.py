from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any


class BoundingBox(BaseModel):
    x: float
    y: float
    width: float
    height: float


class DocTag(BaseModel):
    id: str = Field(
        description="Unique ID for GUI selection, e.g., 'page1-tag4'")
    tag_name: str = Field(description="e.g., 'h1', 'p', 'table'")
    bbox: Optional[BoundingBox] = None
    text: Optional[str] = ""
    attributes: Dict[str, Any] = Field(default_factory=dict)

    # Tree Structure
    nested: List['DocTag'] = Field(default_factory=list)

    # User Annotations (GUI overrides)
    user_removed: bool = False
    user_edited_text: Optional[str] = None
    user_tag_override: Optional[str] = None
