from pydantic import BaseModel, Field
from typing import List, Optional, Tuple, Union, Annotated, Literal
from enum import Enum
import uuid
import math


class EntryType(str, Enum):
    STAR = "star"
    PLANET = "planet"
    IMAGE = "image"
    SHAPE = "shape"


class RA(BaseModel):
    h: int = Field(default=0, ge=0, le=23)
    m: int = Field(default=0, ge=0, le=59)
    s: float = Field(default=0.0, ge=0.0, lt=60.0)


class Declination(BaseModel):
    d: int = Field(default=0, ge=-90, le=90)
    m: int = Field(default=0, ge=0, le=59)
    s: float = Field(default=0.0, ge=0.0, lt=60.0)


class Location(BaseModel):
    ra: RA = Field(default_factory=RA)
    dec: Declination = Field(default_factory=Declination)
    distance: float = Field(default=0.0, ge=0.0)


class BaseGalacticEntry(BaseModel):
    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    name: str = "New Entry"
    short_description: str = ""
    detailed_description: str = ""
    color: str = "#ffffff"
    visible: bool = True
    show_name: bool = True
    show_short_desc: bool = False
    entry_type: EntryType


class Star(BaseGalacticEntry):
    entry_type: Literal[EntryType.STAR] = EntryType.STAR
    location: Location = Field(default_factory=Location)
    radius: float = Field(default=0.1, gt=0.0)

    @property
    def x(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[0]

    @property
    def y(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[1]

    @property
    def z(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[2]

    def _ra_to_degrees(self) -> float:
        total_hours = self.location.ra.h + self.location.ra.m / 60.0 + self.location.ra.s / 3600.0
        return total_hours * 15.0

    def _dec_to_degrees(self) -> float:
        sign = -1 if self.location.dec.d < 0 else 1
        total_deg = abs(self.location.dec.d) + self.location.dec.m / 60.0 + self.location.dec.s / 3600.0
        return sign * total_deg

    def _to_cartesian(
        self, ra_deg: float, dec_deg: float
    ) -> Tuple[float, float, float]:
        ra_rad = math.radians(ra_deg)
        dec_rad = math.radians(dec_deg)
        x = self.location.distance * math.cos(dec_rad) * math.cos(ra_rad)
        y = self.location.distance * math.cos(dec_rad) * math.sin(ra_rad)
        z = self.location.distance * math.sin(dec_rad)
        return (x, y, z)


class Planet(BaseGalacticEntry):
    entry_type: Literal[EntryType.PLANET] = EntryType.PLANET
    parent_star_id: str
    location: Location = Field(default_factory=Location)
    radius: float = Field(default=0.05, gt=0.0)

    @property
    def rel_x(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[0]

    @property
    def rel_y(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[1]

    @property
    def rel_z(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[2]

    def _ra_to_degrees(self) -> float:
        total_hours = self.location.ra.h + self.location.ra.m / 60.0 + self.location.ra.s / 3600.0
        return total_hours * 15.0

    def _dec_to_degrees(self) -> float:
        sign = -1 if self.location.dec.d < 0 else 1
        total_deg = (
            abs(self.location.dec.d) + self.location.dec.m / 60.0 + self.location.dec.s / 3600.0
        )
        return sign * total_deg

    def _to_cartesian(
        self, ra_deg: float, dec_deg: float
    ) -> Tuple[float, float, float]:
        ra_rad = math.radians(ra_deg)
        dec_rad = math.radians(dec_deg)
        x = self.location.distance * math.cos(dec_rad) * math.cos(ra_rad)
        y = self.location.distance * math.cos(dec_rad) * math.sin(ra_rad)
        z = self.location.distance * math.sin(dec_rad)
        return (x, y, z)


class ImageOverlay(BaseGalacticEntry):
    entry_type: Literal[EntryType.IMAGE] = EntryType.IMAGE
    file_path: str = ""
    location: Location = Field(default_factory=Location)
    width: float = Field(default=10.0, gt=0.0)
    height: float = Field(default=10.0, gt=0.0)
    rotation: float = 0.0

    @property
    def x(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[0]

    @property
    def y(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[1]

    @property
    def z(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[2]

    def _ra_to_degrees(self) -> float:
        total_hours = self.location.ra.h + self.location.ra.m / 60.0 + self.location.ra.s / 3600.0
        return total_hours * 15.0

    def _dec_to_degrees(self) -> float:
        sign = -1 if self.location.dec.d < 0 else 1
        total_deg = abs(self.location.dec.d) + self.location.dec.m / 60.0 + self.location.dec.s / 3600.0
        return sign * total_deg

    def _to_cartesian(
        self, ra_deg: float, dec_deg: float
    ) -> Tuple[float, float, float]:
        ra_rad = math.radians(ra_deg)
        dec_rad = math.radians(dec_deg)
        x = self.location.distance * math.cos(dec_rad) * math.cos(ra_rad)
        y = self.location.distance * math.cos(dec_rad) * math.sin(ra_rad)
        z = self.location.distance * math.sin(dec_rad)
        return (x, y, z)


class Shape(BaseGalacticEntry):
    entry_type: Literal[EntryType.SHAPE] = EntryType.SHAPE
    points: List[Tuple[float, float, float]] = []
    closed: bool = False


class CameraState(BaseModel):
    zoom: float = 1.0
    rotation: float = 0.0
    center_x: float = 0.0
    center_y: float = 0.0


GalacticEntry = Annotated[
    Union[Star, Planet, ImageOverlay, Shape], Field(discriminator="entry_type")
]


class GalacticMap(BaseModel):
    entries: List[GalacticEntry] = []
    camera_state: Optional[CameraState] = None

    def get_stars(self) -> List[Star]:
        return [e for e in self.entries if isinstance(e, Star)]

    def get_planets(self) -> List[Planet]:
        return [e for e in self.entries if isinstance(e, Planet)]
