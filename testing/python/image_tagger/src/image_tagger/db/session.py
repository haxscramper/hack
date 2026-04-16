from beartype import beartype
from pathlib import Path
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from .models import Base


def create_sqlite_engine(db_path: Path):
    return create_engine(f"sqlite:///{db_path}", future=True)


def init_db(db_path: Path):
    engine = create_sqlite_engine(db_path)
    Base.metadata.create_all(engine)
    return engine


def make_session_factory(engine):
    return sessionmaker(bind=engine, expire_on_commit=False, future=True)
