#!/usr/bin/env python

import json
import logging
import re
import sys
from collections import defaultdict
from datetime import datetime, timedelta
from pathlib import Path
from pprint import pformat, pprint
from typing import Dict, List, Optional

import click
import pandas as pd
import yaml
from beartype import beartype
from beartype.typing import Any, Dict, List, Optional, Type
from notion_client import Client
from pydantic import BaseModel, Field, field_validator, validator
from sqlalchemy import (Boolean, Column, DateTime, Engine, ForeignKey, Integer,
                        MetaData, String)
from sqlalchemy import Table as SATable
from sqlalchemy import create_engine, inspect, select
from sqlalchemy.orm import Session, declarative_base, sessionmaker
from sqlalchemy.types import TypeDecorator

logging.basicConfig(stream=sys.stdout)

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


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
        logger.info("Importing the time delta")
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
        return timedelta(seconds=int(v / 1000))  # type: ignore


class ReadingRecord(BaseModel, extra="forbid"):
    bookmarks: List[BookmarkRecord]
    books: List[BookRecord]


from sqlalchemy.ext.declarative import DeclarativeMeta

Base: Type[Any] = declarative_base()


class Bookmarks(Base):
    __tablename__ = "bookmarks"
    id = IdColumn()
    idbook = IntColumn()
    num = Column(Integer, default=0)
    dateadd = Column(MillisecondsUnixTimestamp)
    dateedit = Column(MillisecondsUnixTimestamp)
    filename = StrColumn()
    cardpath = StrColumn()
    crc = StrColumn()
    start = IntColumn()
    stop = IntColumn()
    color = Column(Integer, default=0)
    typebmk = Column(Integer, default=0)
    name = StrColumn()
    text = StrColumn()
    lowtext = StrColumn()
    param0 = Column(Integer, nullable=False, default=-1)
    param1 = StrColumn()
    shiftpos = Column(Integer, nullable=False, default=-1)
    shiftstart = Column(Integer, nullable=False, default=-1)
    shiftstop = Column(Integer, nullable=False, default=-1)
    textpos = Column(Integer, nullable=False, default=-1)
    textstart = Column(Integer, nullable=False, default=-1)
    textstop = Column(Integer, nullable=False, default=-1)


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
    lowtitle = StrColumn()
    lowauthor = StrColumn()
    lowseries = StrColumn()
    lowotherdata = StrColumn()
    cpopen = IntColumn()
    cpdef = IntColumn()
    param = IntColumn()
    crc = StrColumn()
    param0 = Column(Integer, nullable=False, default=-1)
    param1 = Column(Integer, nullable=False, default=-1)
    param2 = Column(Integer, nullable=False, default=-1)
    param3 = Column(Integer, nullable=False, default=-1)
    param4 = Column(Integer, nullable=False, default=-1)
    param5 = StrColumn()
    param6 = StrColumn()


class Stack(Base):
    __tablename__ = "stack"
    id = IdColumn()
    idbook = IntColumn()
    stackpos = IntColumn()
    stacksize = IntColumn()
    allpos = StrColumn()
    alltext = StrColumn()
    crc = StrColumn()
    param0 = Column(Integer, nullable=False, default=-1)
    param1 = StrColumn()


class Tmstat(Base):
    __tablename__ = "tmstat"
    id = IdColumn()
    idbook = IntColumn()
    s0 = StrColumn()
    s1 = StrColumn()
    s2 = StrColumn()
    t0 = Column(Integer, default=0)
    t1 = Column(Integer, default=0)
    t2 = Column(Integer, default=0)
    t3 = Column(Integer, default=0)
    t4 = Column(Integer, default=0)
    t5 = Column(Integer, default=0)
    t6 = Column(Integer, default=0)
    t7 = Column(Integer, default=0)
    t8 = Column(Integer, default=0)
    t9 = Column(Integer, default=0)


class Filebmk(Base):
    __tablename__ = "filebmk"
    id = IdColumn()
    type = IntColumn()
    path = StrColumn()
    card = StrColumn()
    datefirst = Column(MillisecondsUnixTimestamp)
    title = StrColumn()
    param0 = Column(Integer, nullable=False, default=-1)
    param1 = Column(Integer, nullable=False, default=-1)
    param2 = Column(Integer, nullable=False, default=-1)
    param3 = Column(Integer, nullable=False, default=-1)
    param4 = Column(Integer, nullable=False, default=-1)
    param5 = StrColumn()
    param6 = StrColumn()


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


@beartype
class NotionSyncManager:

    def __init__(self, token: str, database_id: str, dry_run: bool):
        self.notion = Client(auth=token)
        self.database_id = database_id
        self.dry_run = dry_run

    def get_existing_books(self) -> Dict[str, Dict[str, Any]]:
        books = {}
        start_cursor = None

        while True:
            query_params = {"database_id": self.database_id}
            if start_cursor:
                query_params["start_cursor"] = start_cursor

            response = self.notion.databases.query(**query_params)
            results = response.get("results", [])

            for page in results:
                properties = page.get("properties", {})
                title = properties.get("Name", {}).get("title", [])
                if title:
                    book_title = title[0].get("plain_text", "")
                    if book_title in books:
                        logger.warning(
                            f"Notion database has duplicate data, '{book_title}' is already present"
                        )

                    if book_title:
                        books[book_title] = {
                            "id": page["id"],
                            "properties": properties
                        }

            # Check if there are more pages to fetch
            next_cursor = response.get("next_cursor")
            if not next_cursor:
                break

            start_cursor = next_cursor

        logger.info(f"Retrieved {len(books)} books from Notion database")
        return books

    def update_notion_database(self, books: List[BookRecord]) -> None:
        existing_books = self.get_existing_books()

        if self.dry_run:
            for idx, existing_book in enumerate(existing_books):
                logger.info(f"Existing book [{idx}] '{existing_book}'")

        for book in books:
            reading_progress = book.bookpos / book.booksize if book.booksize > 0 else 0

            # Skip books with less than 2% progress if they're not already in the database
            if reading_progress < 0.02 and book.title not in existing_books:
                logger.info(
                    f"Skipping new book with less than 2% progress: '{book.title}'"
                )
                continue

            if book.title in existing_books:
                # Check if reading progress has changed
                current_page = existing_books[book.title]  # type: ignore
                current_progress = current_page["properties"].get(
                    "Reading progress", {}).get("number", 0) or 0

                # I can go back on reading some books again
                if int(reading_progress * 100) <= int(current_progress * 100):
                    logger.info(
                        f"No progress change for book: '{book.title}', current progress is {current_progress:.2%}, DB store is {reading_progress:.2%}"
                    )
                    continue

                # Update existing book
                logger.info(
                    f"Updating book: '{book.title}' with new progress: {reading_progress:.2%} < {current_progress:.2%}"
                )

                if not self.dry_run:
                    self.notion.pages.update(
                        page_id=current_page["id"],
                        properties={
                            "Reading progress": {
                                "number": reading_progress
                            },
                            "Finished/paused reading": {
                                "date": {
                                    "start": book.datelast.isoformat()
                                }
                            },
                        },
                    )
            elif book.title:
                # Create new book
                logger.info(
                    f"Adding new book: {book.title} with progress: {reading_progress:.2%}"
                )
                if not self.dry_run:
                    self.notion.pages.create(
                        parent={"database_id": self.database_id},
                        properties={
                            "Name": {
                                "title": [{
                                    "text": {
                                        "content": book.title
                                    }
                                }]
                            },
                            "Author": {
                                "rich_text": [{
                                    "text": {
                                        "content": book.author
                                    }
                                }]
                            },
                            "Reading progress": {
                                "number": reading_progress
                            },
                            "Started reading": {
                                "date": {
                                    "start": book.datefirst.isoformat()
                                }
                            },
                            "Finished/paused reading": {
                                "date": {
                                    "start": book.datelast.isoformat()
                                }
                            },
                        },
                    )


@beartype
def sync_books_to_notion(
    books: List[BookRecord],
    notion_token: str,
    database_id: str,
    dry_run: bool,
) -> None:
    sync_manager = NotionSyncManager(
        token=notion_token,
        database_id=database_id,
        dry_run=dry_run,
    )
    sync_manager.update_notion_database(books)


@click.command()
@click.argument("infile")
@click.argument("outfile")
@click.option("--import_offset")
@click.option("--notion_token")
@click.option("--notion_database")
@click.option("--notion_access_dry_run", type=click.BOOL)
def main(
    infile: str,
    outfile: str,
    import_offset: Optional[str] = None,
    notion_token: Optional[str] = None,
    notion_database: Optional[str] = None,
    notion_access_dry_run: bool = True,
) -> None:

    opts = AlXreaderImportOptions(
        infile=Path(infile),
        outfile=Path(outfile),
        import_offset=import_offset,  # type: ignore
    )
    engine = open_sqlite(opts.infile)
    session = sessionmaker()(bind=engine)
    if opts.import_offset:
        logger.info(f"Import time offset {opts.import_offset}")
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

    logger.info("Import done")

    if notion_database and notion_token:
        sync_books_to_notion(
            reading.books,
            notion_token=notion_token,
            database_id=notion_database,
            dry_run=bool(notion_access_dry_run),
        )


if __name__ == "__main__":
    main()
