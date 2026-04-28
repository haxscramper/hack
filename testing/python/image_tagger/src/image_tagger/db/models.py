from beartype import beartype
from sqlalchemy import (
    Column,
    Integer,
    String,
    Float,
    ForeignKey,
    UniqueConstraint,
    Text,
)
from sqlalchemy.orm import declarative_base, relationship

Base = declarative_base()


class ImageEntry(Base):
    __tablename__ = "image_entries"

    id = Column(Integer, primary_key=True, autoincrement=True)
    relative_path = Column(String, unique=True, nullable=True, index=True)
    original_name = Column(String, nullable=False)
    md5_digest = Column(String, nullable=False, index=True)

    probabilistic_tags = relationship("ImageProbabilisticTag", cascade="all, delete-orphan")
    regular_tags = relationship("ImageRegularTag", cascade="all, delete-orphan")
    descriptions = relationship("ImageDescription", cascade="all, delete-orphan")


class ProbabilisticTag(Base):
    __tablename__ = "probabilistic_tags"

    id = Column(Integer, primary_key=True, autoincrement=True)
    external_tag_id = Column(Integer, unique=True, nullable=True)
    category = Column(String, nullable=False)
    name = Column(String, nullable=False, unique=True)


class ImageProbabilisticTag(Base):
    __tablename__ = "image_probabilistic_tags"

    id = Column(Integer, primary_key=True, autoincrement=True)
    image_id = Column(Integer, ForeignKey("image_entries.id"), nullable=False)
    tag_id = Column(Integer, ForeignKey("probabilistic_tags.id"), nullable=False)
    probability = Column(Float, nullable=False)

    __table_args__ = (UniqueConstraint("image_id", "tag_id"), )


class RegularTag(Base):
    __tablename__ = "regular_tags"

    id = Column(Integer, primary_key=True, autoincrement=True)
    category = Column(String, nullable=False)
    name = Column(String, nullable=False)

    __table_args__ = (UniqueConstraint("category", "name"), )


class ImageRegularTag(Base):
    __tablename__ = "image_regular_tags"

    id = Column(Integer, primary_key=True, autoincrement=True)
    image_id = Column(Integer, ForeignKey("image_entries.id"), nullable=False)
    tag_id = Column(Integer, ForeignKey("regular_tags.id"), nullable=False)

    __table_args__ = (UniqueConstraint("image_id", "tag_id"), )


class ImageDescription(Base):
    __tablename__ = "image_descriptions"

    id = Column(Integer, primary_key=True, autoincrement=True)
    image_id = Column(Integer, ForeignKey("image_entries.id"),
                      nullable=False,
                      unique=True)
    description = Column(Text, nullable=False)
    model_name = Column(String, nullable=True)
