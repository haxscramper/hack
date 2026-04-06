from pydantic import BaseModel, Field, computed_field
from typing import List, Optional, Tuple, Union, Annotated, Literal
from enum import Enum
import uuid
import math


class EntryType(str, Enum):
    STAR = "star"
    PLANET = "planet"
    IMAGE = "image"
    SHAPE = "shape"


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
    ra_h: int = Field(default=0, ge=0, le=23)
    ra_m: int = Field(default=0, ge=0, le=59)
    ra_s: float = Field(default=0.0, ge=0.0, lt=60.0)
    dec_d: int = Field(default=0, ge=-90, le=90)
    dec_m: int = Field(default=0, ge=0, le=59)
    dec_s: float = Field(default=0.0, ge=0.0, lt=60.0)
    distance: float = Field(default=0.0, ge=0.0)
    radius: float = Field(default=0.1, gt=0.0)

    @computed_field
    @property
    def x(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[0]

    @computed_field
    @property
    def y(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[1]

    @computed_field
    @property
    def z(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[2]

    def _ra_to_degrees(self) -> float:
        total_hours = self.ra_h + self.ra_m / 60.0 + self.ra_s / 3600.0
        return total_hours * 15.0

    def _dec_to_degrees(self) -> float:
        sign = -1 if self.dec_d < 0 else 1
        total_deg = abs(self.dec_d) + self.dec_m / 60.0 + self.dec_s / 3600.0
        return sign * total_deg

    def _to_cartesian(
        self, ra_deg: float, dec_deg: float
    ) -> Tuple[float, float, float]:
        ra_rad = math.radians(ra_deg)
        dec_rad = math.radians(dec_deg)
        x = self.distance * math.cos(dec_rad) * math.cos(ra_rad)
        y = self.distance * math.cos(dec_rad) * math.sin(ra_rad)
        z = self.distance * math.sin(dec_rad)
        return (x, y, z)

    def set_from_degrees(self, ra_deg: float, dec_deg: float, dist: float):
        total_hours = ra_deg / 15.0
        self.ra_h = int(total_hours) % 24
        remaining = (total_hours - self.ra_h) * 60
        self.ra_m = int(remaining)
        self.ra_s = round((remaining - self.ra_m) * 60, 2)

        sign = -1 if dec_deg < 0 else 1
        abs_dec = abs(dec_deg)
        self.dec_d = int(abs_dec) * sign
        remaining = (abs_dec - int(abs_dec)) * 60
        self.dec_m = int(remaining)
        self.dec_s = round((remaining - self.dec_m) * 60, 2)

        self.distance = dist


class Planet(BaseGalacticEntry):
    entry_type: Literal[EntryType.PLANET] = EntryType.PLANET
    parent_star_id: str
    rel_ra_h: int = Field(default=0, ge=0, le=23)
    rel_ra_m: int = Field(default=0, ge=0, le=59)
    rel_ra_s: float = Field(default=0.0, ge=0.0, lt=60.0)
    rel_dec_d: int = Field(default=0, ge=-90, le=90)
    rel_dec_m: int = Field(default=0, ge=0, le=59)
    rel_dec_s: float = Field(default=0.0, ge=0.0, lt=60.0)
    rel_distance: float = Field(default=0.0, ge=0.0)
    radius: float = Field(default=0.05, gt=0.0)

    @computed_field
    @property
    def rel_x(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[0]

    @computed_field
    @property
    def rel_y(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[1]

    @computed_field
    @property
    def rel_z(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[2]

    def _ra_to_degrees(self) -> float:
        total_hours = self.rel_ra_h + self.rel_ra_m / 60.0 + self.rel_ra_s / 3600.0
        return total_hours * 15.0

    def _dec_to_degrees(self) -> float:
        sign = -1 if self.rel_dec_d < 0 else 1
        total_deg = (
            abs(self.rel_dec_d) + self.rel_dec_m / 60.0 + self.rel_dec_s / 3600.0
        )
        return sign * total_deg

    def _to_cartesian(
        self, ra_deg: float, dec_deg: float
    ) -> Tuple[float, float, float]:
        ra_rad = math.radians(ra_deg)
        dec_rad = math.radians(dec_deg)
        x = self.rel_distance * math.cos(dec_rad) * math.cos(ra_rad)
        y = self.rel_distance * math.cos(dec_rad) * math.sin(ra_rad)
        z = self.rel_distance * math.sin(dec_rad)
        return (x, y, z)

    def set_from_degrees(self, ra_deg: float, dec_deg: float, dist: float):
        total_hours = ra_deg / 15.0
        self.rel_ra_h = int(total_hours) % 24
        remaining = (total_hours - self.rel_ra_h) * 60
        self.rel_ra_m = int(remaining)
        self.rel_ra_s = round((remaining - self.rel_ra_m) * 60, 2)

        sign = -1 if dec_deg < 0 else 1
        abs_dec = abs(dec_deg)
        self.rel_dec_d = int(abs_dec) * sign
        remaining = (abs_dec - int(abs_dec)) * 60
        self.rel_dec_m = int(remaining)
        self.rel_dec_s = round((remaining - self.rel_dec_m) * 60, 2)

        self.rel_distance = dist


class ImageOverlay(BaseGalacticEntry):
    entry_type: Literal[EntryType.IMAGE] = EntryType.IMAGE
    file_path: str = ""
    ra_h: int = Field(default=0, ge=0, le=23)
    ra_m: int = Field(default=0, ge=0, le=59)
    ra_s: float = Field(default=0.0, ge=0.0, lt=60.0)
    dec_d: int = Field(default=0, ge=-90, le=90)
    dec_m: int = Field(default=0, ge=0, le=59)
    dec_s: float = Field(default=0.0, ge=0.0, lt=60.0)
    distance: float = Field(default=0.0, ge=0.0)
    width: float = Field(default=10.0, gt=0.0)
    height: float = Field(default=10.0, gt=0.0)
    rotation: float = 0.0

    @computed_field
    @property
    def x(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[0]

    @computed_field
    @property
    def y(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[1]

    @computed_field
    @property
    def z(self) -> float:
        ra_deg = self._ra_to_degrees()
        dec_deg = self._dec_to_degrees()
        return self._to_cartesian(ra_deg, dec_deg)[2]

    def _ra_to_degrees(self) -> float:
        total_hours = self.ra_h + self.ra_m / 60.0 + self.ra_s / 3600.0
        return total_hours * 15.0

    def _dec_to_degrees(self) -> float:
        sign = -1 if self.dec_d < 0 else 1
        total_deg = abs(self.dec_d) + self.dec_m / 60.0 + self.dec_s / 3600.0
        return sign * total_deg

    def _to_cartesian(
        self, ra_deg: float, dec_deg: float
    ) -> Tuple[float, float, float]:
        ra_rad = math.radians(ra_deg)
        dec_rad = math.radians(dec_deg)
        x = self.distance * math.cos(dec_rad) * math.cos(ra_rad)
        y = self.distance * math.cos(dec_rad) * math.sin(ra_rad)
        z = self.distance * math.sin(dec_rad)
        return (x, y, z)

    def set_from_degrees(self, ra_deg: float, dec_deg: float, dist: float):
        total_hours = ra_deg / 15.0
        self.ra_h = int(total_hours) % 24
        remaining = (total_hours - self.ra_h) * 60
        self.ra_m = int(remaining)
        self.ra_s = round((remaining - self.ra_m) * 60, 2)

        sign = -1 if dec_deg < 0 else 1
        abs_dec = abs(dec_deg)
        self.dec_d = int(abs_dec) * sign
        remaining = (abs_dec - int(abs_dec)) * 60
        self.dec_m = int(remaining)
        self.dec_s = round((remaining - self.dec_m) * 60, 2)

        self.distance = dist


class Shape(BaseGalacticEntry):
    entry_type: Literal[EntryType.SHAPE] = EntryType.SHAPE
    points: List[Tuple[float, float, float]] = []
    closed: bool = False



GalacticEntry = Annotated[
    Union[Star, Planet, ImageOverlay, Shape], Field(discriminator="entry_type")
]


class GalacticMap(BaseModel):
    entries: List[GalacticEntry] = []
    use_markers: bool = False

    def get_stars(self) -> List[Star]:
        return [e for e in self.entries if isinstance(e, Star)]

    def get_planets(self) -> List[Planet]:
        return [e for e in self.entries if isinstance(e, Planet)]
