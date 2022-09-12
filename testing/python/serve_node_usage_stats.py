#!/usr/bin/env python
import argparse
import argparse
import os
from flask import Flask, render_template, request
from flask_sqlalchemy import SQLAlchemy
from typing import List
from pprint import pprint
from dataclasses import dataclass, field

parser = argparse.ArgumentParser(description="Serve database")
parser.add_argument("database", type=str, help="Input database file")

args = parser.parse_args()
app = Flask(__name__, template_folder=os.getcwd())
app.config["SQLALCHEMY_DATABASE_URI"] = f"sqlite:///{args.database}"
db = SQLAlchemy(app)


class SQLFile(db.Model):
    __tablename__ = "path_map"
    id = db.Column(db.Integer, primary_key=True)
    abs_path = db.Column(db.Text)


class SQLNodeKind(db.Model):
    __tablename__ = "node_kind"
    id = db.Column(db.Integer, primary_key=True)
    kind_name = db.Column(db.Text)


class SQLStoredFile(db.Model):
    __tablename__ = "stored_file"
    id = db.Column(db.Integer, primary_key=True)
    content = db.Column(db.Text)


class SQLTypeKind(db.Model):
    __tablename__ = "type_kind"
    id = db.Column(db.Integer, primary_key=True)
    type_name = db.Column(db.Text)


class SQLFlow(db.Model):
    __tablename__ = "dedup"
    rowid = db.Column(db.Integer, primary_key=True)
    file_id = db.Column(db.Integer, db.ForeignKey("path_map.id"))
    line = db.Column(db.Integer)
    col = db.Column(db.Integer)
    kind = db.Column(db.Integer)
    to_kind = db.Column(db.Integer)
    from_kind = db.Column(db.Integer)
    num = db.Column(db.Integer)


db.create_all()
db.session.commit()


@app.template_filter()
def path_basename(s: str) -> str:
    return os.path.basename(s)


@dataclass
class Char:
    has_tooltip: bool
    tooltip: str
    char: str


@dataclass
class Line:
    number: int
    chars: List[Char] = field(default_factory=list)


rev_files = {}
for file in db.session.query(SQLFile).all():
    rev_files[file.abs_path] = file.id

node_kinds = {}
for kind in db.session.query(SQLNodeKind).all():
    node_kinds[kind.id] = kind.kind_name

type_kinds = {}
for kind in db.session.query(SQLTypeKind).all():
    type_kinds[kind.id] = kind.type_name

ann = {}


for flow in db.session.query(SQLFlow).all():
    if flow.file_id not in ann:
        ann[flow.file_id] = {}

    if flow.line not in ann[flow.file_id]:
        ann[flow.file_id][flow.line] = {}

    if flow.col not in ann[flow.file_id][flow.line]:
        ann[flow.file_id][flow.line][flow.col] = ""

    ann[flow.file_id][flow.line][
        flow.col
    ] += f"{node_kinds[flow.from_kind]} -> {node_kinds[flow.to_kind]} ({flow.num} times)\n"


def get_content(file_path: str) -> str:
    file_id = rev_files[file_path]
    result = db.session.query(SQLStoredFile).where(SQLStoredFile.id == file_id).all()
    if len(result) == 0:
        content = ""
        with open(file_path, "r") as file:
            content = file.read()

        print(f"added {file_id}, {len(content)}")
        db.session.add(SQLStoredFile(id=file_id, content=content))
        db.session.commit()
        return content

    else:
        return result[0].content


for file in db.session.query(SQLFile).all():
    get_content(file.abs_path)


@app.template_filter()
def get_lines(path: str) -> List[Line]:
    result = []
    file_id = rev_files[path]
    for idx, line in enumerate(get_content(path).split("\n")):
        line_num = idx + 1
        out_line = Line(number=line_num)
        for char_idx, char in enumerate(line):
            if (
                file_id in ann
                and line_num in ann[file_id]
                and char_idx in ann[file_id][line_num]
            ):

                out_line.chars.append(
                    Char(
                        tooltip=str(ann[file_id][line_num][char_idx]),
                        char=char,
                        has_tooltip=True,
                    )
                )

            else:
                out_line.chars.append(Char(tooltip="", char=char, has_tooltip=False))

        result.append(out_line)

    return result


files = db.session.query(SQLFile).all()


@app.route("/<path>", methods=["GET"])
def hello(path):
    args = request.args
    print(args)
    return render_template(
        "node_usage.html.j2", files=files, abs_path=request.args["path"]
    )


app.run(debug=True)
