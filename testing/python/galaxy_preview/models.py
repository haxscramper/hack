from pydantic import BaseModel, Field, RootModel
from typing import List, Optional, Tuple, Union, Annotated, Literal
from enum import Enum
import uuid

class EntryType(str, Enum):
    STAR = "star"
    PLANET = "planet"
    IMAGE = "image"
    SHAPE = "shape"

class BaseGalacticEntry(BaseModel):
    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    name: str = "New Entry"
    short_description: str = ""  # Rich text (HTML)
    detailed_description: str = ""  # Rich text (HTML)
    color: str = "#ffffff"
    visible: bool = True
    show_name: bool = True
    show_short_desc: bool = False
    entry_type: EntryType

class Star(BaseGalacticEntry):
    entry_type: Literal[EntryType.STAR] = EntryType.STAR
    x: float = 0.0
    y: float = 0.0
    z: float = 0.0
    radius: float = 0.1

class Planet(BaseGalacticEntry):
    entry_type: Literal[EntryType.PLANET] = EntryType.PLANET
    parent_star_id: str
    rel_x: float = 0.0
    rel_y: float = 0.0
    rel_z: float = 0.0
    radius: float = 0.05

class ImageOverlay(BaseGalacticEntry):
    entry_type: Literal[EntryType.IMAGE] = EntryType.IMAGE
    file_path: str = ""
    x: float = 0.0
    y: float = 0.0
    z: float = 0.0
    width: float = 10.0
    height: float = 10.0
    rotation: float = 0.0

class Shape(BaseGalacticEntry):
    entry_type: Literal[EntryType.SHAPE] = EntryType.SHAPE
    points: List[Tuple[float, float, float]] = [] # (x, y, z) in parsecs
    closed: bool = False

class CameraState(BaseModel):
    scale_factor: Optional[float] = None
    center: Tuple[float, float, float] = (0.0, 0.0, 0.0)
    fov: float = 45.0
    elevation: float = 30.0
    azimuth: float = 30.0
    roll: float = 0.0
    distance: Optional[float] = 1000.0

GalacticEntry = Annotated[
    Union[Star, Planet, ImageOverlay, Shape],
    Field(discriminator='entry_type')
]

class GalacticMap(BaseModel):
    entries: List[GalacticEntry] = []
    camera_state: Optional[CameraState] = None

    # Helper methods to get specific types
    def get_stars(self) -> List[Star]:
        return [e for e in self.entries if isinstance(e, Star)]

    def get_planets(self) -> List[Planet]:
        return [e for e in self.entries if isinstance(e, Planet)]
