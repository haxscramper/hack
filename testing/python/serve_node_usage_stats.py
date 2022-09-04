#!/usr/bin/env python
import argparse
import argparse
import os
from flask import Flask, render_template, request
import sqlalchemy as sqa
from sqlalchemy import Column, Integer, Text, ForeignKey, Boolean
from sqlalchemy.orm import declarative_base, scoped_session
from typing import List
from dataclasses import dataclass, field


SQLBase = declarative_base()


class SQLFile(SQLBase):
    __tablename__ = "path_map"
    id = Column(Integer, primary_key=True)
    abs_path = Column(Text)


class SQLNodeKind(SQLBase):
    __tablename__ = "node_kind"
    id = Column(Integer, primary_key=True)
    kind_name = Column(Text)


class SQLTypeKind(SQLBase):
    __tablename__ = "type_kind"
    id = Column(Integer, primary_key=True)
    type_name = Column(Text)


class SQLFlow(SQLBase):
    __tablename__ = "dedup"
    rowid = Column(Integer, primary_key=True)
    file_id = Column(Integer, ForeignKey("path_map.id"))
    line = Column(Integer)
    col = Column(Integer)
    kind = Column(Integer)
    to_kind = Column(Integer)
    from_kind = Column(Integer)
    num = Column(Integer)


app = Flask(__name__, template_folder=os.getcwd())


parser = argparse.ArgumentParser(description="Serve database")
parser.add_argument("database", type=str, help="Input database file")

args = parser.parse_args()

engine = sqa.create_engine(f"sqlite:///{args.database}")
# https://stackoverflow.com/a/60290438/6086513
session = scoped_session(sqa.orm.sessionmaker(bind=engine))()
meta = SQLBase.metadata


@app.template_filter()
def path_basename(s: str) -> str:
    return os.path.basename(s)


@dataclass
class Char():
    has_tooltip: bool
    tooltip: str
    char: str


@dataclass
class Line():
    number: int
    chars: List[Char] = field(default_factory=list)


rev_files = {}
for file in session.query(SQLFile).all():
    rev_files[file.abs_path] = file.id

node_kinds = {}
for kind in session.query(SQLNodeKind).all():
    node_kinds[kind.id] = kind.kind_name

type_kinds = {}
for kind in session.query(SQLTypeKind).all():
    type_kinds[kind.id] = kind.type_name

ann = {}


for flow in session.query(SQLFlow).all():
    if flow.file_id not in ann:
        ann[flow.file_id] = {}

    if flow.line not in ann[flow.file_id]:
        ann[flow.file_id][flow.line] = {}

    if flow.col not in ann[flow.file_id][flow.line]:
        ann[flow.file_id][flow.line][flow.col] = ""

    ann[flow.file_id][flow.line][flow.col] += (
        f"{node_kinds[flow.from_kind]} -> {node_kinds[flow.to_kind]} ({flow.num} times)\n"
    )


@ app.template_filter()
def get_lines(path: str) -> List[Line]:
    result = []
    with open(path, 'r') as file:
        file_id = rev_files[path]
        for idx, line in enumerate(file.read().split("\n")):
            line_num = idx + 1
            out_line = Line(number=line_num)
            for char_idx, char in enumerate(line):
                if file_id in ann \
                        and line_num in ann[file_id] \
                        and char_idx in ann[file_id][line_num]:

                    out_line.chars.append(Char(
                        tooltip=str(ann[file_id][line_num][char_idx]),
                        char=char,
                        has_tooltip=True))

                else:
                    out_line.chars.append(Char(
                        tooltip="", char=char, has_tooltip=False))

            result.append(out_line)

    return result


files = session.query(SQLFile).all()


@ app.route("/<path>", methods=['GET'])
def hello(path):
    args = request.args
    print(args)
    return render_template(
        "node_usage.html.j2",
        files=files,
        abs_path=request.args["path"]
    )


app.run(debug=True)
