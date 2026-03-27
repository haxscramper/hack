from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any

class BoundingBox(BaseModel):
    x: float
    y: float
    width: float
    height: float

class DocTag(BaseModel):
    id: str = Field(description="Unique ID for GUI selection, e.g., 'page1-tag4'")
    tag_name: str = Field(description="e.g., 'h1', 'p', 'table'")
    bbox: Optional[BoundingBox] = None
    text: Optional[str] = ""
    attributes: Dict[str, Any] = Field(default_factory=dict)
    
    # Tree Structure
    children: List['DocTag'] = Field(default_factory=list)
    
    # User Annotations (GUI overrides)
    user_removed: bool = False
    user_edited_text: Optional[str] = None
    user_tag_override: Optional[str] = None

class PageData(BaseModel):
    page_number: int
    raw_docling_response: str
    spatial_tags: List[DocTag] = Field(default_factory=list)
    image_cache_path: str
