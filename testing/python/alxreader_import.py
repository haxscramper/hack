#!/usr/bin/env python

import pandas as pd
from beartype import beartype
from beartype.typing import List, Optional
from collections import defaultdict
from sqlalchemy import Column, Integer, String, ForeignKey, DateTime, Boolean
from sqlalchemy import create_engine, MetaData, Table as SATable, Engine, inspect
from pathlib import Path

from sqlalchemy.orm import declarative_base, sessionmaker, Session
from sqlalchemy import Column, select
from datetime import datetime, timedelta
from pydantic import validator, field_validator, BaseModel, Field
from sqlalchemy.types import TypeDecorator
import click
import re
import logging
import json
from pprint import pformat, pprint


def log(category="rich") -> logging.Logger:
    log = logging.getLogger(category)
    return log


@beartype
def open_sqlite(file: Path, base=None) -> Engine:
    engine = create_engine("sqlite:///" + str(file))
    if base:
        base.metadata.create_all(engine)

    return engine


def IdColumn():
    return Column(Integer, primary_key=True, autoincrement=True)


def ForeignId(name: str, nullable: bool = False):
    return Column(Integer, ForeignKey(name), nullable=nullable)


def IntColumn(nullable: bool = False):
    return Column(Integer, nullable=nullable)


def BoolColumn(nullable: bool = False):
    return Column(Boolean, nullable=nullable)


def StrColumn(nullable: bool = False):
    return Column(String, nullable=nullable)


CAT = Path(__file__).name


class MillisecondsUnixTimestamp(TypeDecorator):
    """Converts between Unix timestamp in milliseconds and Python datetime objects."""
    impl = Integer

    def process_bind_param(self, value, dialect):
        """Convert Python datetime to Unix timestamp in milliseconds."""
        if value is not None:
            return int(value.timestamp() * 1000)
        return value

    def process_result_value(self, value, dialect):
        """Convert Unix timestamp in milliseconds to Python datetime."""
        if value is not None:
            return datetime.fromtimestamp(value / 1000.0)
        return value


class AlXreaderImportOptions(BaseModel):
    infile: Path = Field(description="Alxreader backup DB")
    outfile: Path = Field(description="File with bookmark collection")
    import_offset: Optional[timedelta] = Field(
        default=None,
        description=
        "Apply this time offset on all timestamps imported from the sqlite database"
    )

    @field_validator('import_offset', mode="before")
    def parse_timedelta(cls, v: Optional[str]):
        log(CAT).info("Importing the time delta")
        if v == None:
            return None

        if isinstance(v, timedelta):
            return v
        if isinstance(v, str):
            # Example of a simple parser for strings like "1d 2h 3m 4s"
            # You can adjust the regex and conversion logic as needed
            match = re.match(
                r'((?P<days>\d+)d)?\s*((?P<hours>\d+)h)?\s*((?P<minutes>\d+)m)?\s*((?P<seconds>\d+)s)?',
                v)
            if match:
                parts = {
                    key: int(value)
                    for key, value in match.groupdict(default=0).items()
                }
                return timedelta(**parts)
            # Raise an error if the string format is unrecognized
            raise ValueError("Invalid timedelta string format")
        # Optionally handle other input formats here
        raise TypeError("Invalid type for timedelta")


class BookmarkRecord(BaseModel, extra="forbid"):
    title: str
    text: str
    author: str
    dateadd: datetime
    dateedit: datetime
    start: int
    stop: int


class BookRecord(BaseModel, extra="forbid"):
    title: str
    author: str
    booksize: int
    bookpos: int
    datelast: datetime
    datefirst: datetime
    read_time: timedelta

    @field_validator("read_time", mode="before")
    def parse_timedelta(cls, v: Optional[int]):
        return timedelta(seconds=int(v / 1000))


class ReadingRecord(BaseModel, extra="forbid"):
    bookmarks: List[BookmarkRecord]
    books: List[BookRecord]


Base = declarative_base()


class Bookmarks(Base):
    __tablename__ = "bookmarks"
    id = IdColumn()
    idbook = IntColumn()
    dateadd = Column(MillisecondsUnixTimestamp)
    dateedit = Column(MillisecondsUnixTimestamp)
    filename = StrColumn()
    cardpath = StrColumn()
    crc = StrColumn()
    start = IntColumn()
    stop = IntColumn()
    name = StrColumn()
    text = StrColumn()
    lowtext = StrColumn()


class Recent(Base):
    __tablename__ = "recent"
    id = IdColumn()
    filename = StrColumn()
    cardpath = StrColumn()
    booksize = IntColumn()
    bookpos = IntColumn()
    filesize = IntColumn()
    datefirst = Column(MillisecondsUnixTimestamp)
    datelast = Column(MillisecondsUnixTimestamp)
    title = StrColumn()
    author = StrColumn()
    series = StrColumn()
    otherdata = StrColumn()
    param0 = IntColumn()


@beartype
def get_bookmarks(session: Session) -> List[BookmarkRecord]:
    result: List[BookmarkRecord] = []

    expr = select(
        Bookmarks.filename,
        Recent.title,
        Recent.author,
        Bookmarks.dateadd,
        Bookmarks.dateedit,
        Bookmarks.text,
        Bookmarks.start,
        Bookmarks.stop,
    ).join(
        Recent,
        Recent.filename == Bookmarks.filename,
    )

    for item in session.execute(expr).mappings():
        data = {k: item[k] for k in BookmarkRecord.model_fields}
        result.append(BookmarkRecord.model_validate(data, strict=True))

    def compare_bookmarks(mark: BookmarkRecord):
        return mark.dateadd

    return sorted(result, key=compare_bookmarks)


@beartype
def get_books(session: Session) -> List[BookRecord]:
    result: List[BookRecord] = []

    expr = select(
        Recent.title,
        Recent.author,
        Recent.datefirst,
        Recent.datelast,
        Recent.param0.label("read_time"),
        Recent.booksize,
        Recent.bookpos,
    )

    for item in session.execute(expr).mappings():
        data = {k: item[k] for k in BookRecord.model_fields}
        result.append(BookRecord.model_validate(data, strict=True))

    def book_key(mark: BookRecord):
        return mark.datefirst

    return sorted(result, key=book_key)


@beartype
def format_timedelta(td: timedelta) -> str:
    total_seconds = int(td.total_seconds())
    minutes, seconds = divmod(total_seconds, 60)
    hours, minutes = divmod(minutes, 60)
    days, hours = divmod(hours, 24)
    duration = "P"
    if days:
        duration += f"{days}D"
    if hours or minutes or seconds:
        duration += "T"
    if hours:
        duration += f"{hours}H"
    if minutes:
        duration += f"{minutes}M"
    if seconds:
        duration += f"{seconds}S"
    return duration


@click.command()
@click.argument("infile")
@click.argument("outfile")
@click.option("--import_offset")
def main(
    infile: str,
    outfile: str,
    import_offset: Optional[str] = None,
) -> None:
    opts = AlXreaderImportOptions(
        infile=Path(infile),
        outfile=Path(outfile),
        import_offset=import_offset,
    )
    engine = open_sqlite(opts.infile)
    session = sessionmaker()(bind=engine)
    if opts.import_offset:
        log(CAT).info(f"Import time offset {opts.import_offset}")
        bookmarks = [
            it.model_copy(update=dict(
                dateadd=it.dateadd + opts.import_offset,
                dateedit=it.dateedit + opts.import_offset,
            )) for it in get_bookmarks(session)
        ]

    else:
        bookmarks = get_bookmarks(session)

    Path("/tmp/dbg.txt").write_text(pformat(bookmarks, indent=2, width=200))

    reading = ReadingRecord(
        bookmarks=bookmarks,
        books=get_books(session),
    )

    def custom_encoder(obj):
        match obj:
            case datetime():
                return obj.isoformat()

            case timedelta():
                return {
                    "iso": format_timedelta(obj),
                    "seconds": obj.total_seconds(),
                }

            case _:
                raise TypeError(
                    f"Object of type {obj.__class__.__name__} is not JSON serializable"
                )

    opts.outfile.write_text(
        json.dumps(
            reading.model_dump(),
            indent=2,
            default=custom_encoder,
            ensure_ascii=False,
        ))

    log(CAT).info("Import done")


if __name__ == "__main__":
    main()
