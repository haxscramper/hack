#!/usr/bin/env python

import json
import re
import shlex
import sys
from pathlib import Path
from typing import Optional

import matplotlib.colors as mcolors
from matplotlib import colormaps

import plotly.graph_objects as go
from pydantic import BaseModel, field_validator, model_validator


def build_color_map(categories: list[str]) -> dict[str, str]:
    cats = sorted(set(categories))
    cmap = colormaps.get_cmap("tab20").resampled(len(cats))
    return {cat: mcolors.to_hex(cmap(i)) for i, cat in enumerate(cats)}


class ProcessInfo(BaseModel):
    cmdline: list[str]
    cwd: str
    ppid: Optional[int] = None

    @field_validator("cmdline", mode="before")
    @classmethod
    def split_cmdline(cls, v):
        if isinstance(v, list):
            return v
        return shlex.split(v)

    @field_validator("ppid", mode="before")
    @classmethod
    def parse_ppid(cls, v):
        if v is None or v == "":
            return None
        return int(v)


class Event(BaseModel):
    pid: int
    message: str  # "start" | "end"
    ts: float
    info: ProcessInfo

    @model_validator(mode="before")
    @classmethod
    def extract(cls, data: dict):
        args = data["args"]
        message = args["message"]

        fields = args["state"]["value"]["data"]["value"]["fields"]
        flat = {f["name"]: f["value"]["shortRepr"] for f in fields}

        return {
            "pid": data["pid"],
            "message": message,
            "ts": data["ts"],
            "info": {
                "cmdline": flat["cmdline"],
                "cwd": flat["cwd"],
                "ppid": flat.get("ppid"),
            },
        }


class Process(BaseModel):
    pid: int
    ppid: Optional[int]
    cmdline: list[str]
    cwd: str
    start: float
    end: Optional[float] = None

    @property
    def duration(self) -> float:
        assert self.end is not None
        return self.end - self.start


def categorize(cmdline: list[str], cwd: str) -> str:
    joined = " ".join(cmdline)
    exe = Path(cmdline[0]).name if cmdline else ""

    is_trycompile = "TryCompile-" in cwd or "CMakeScratch" in cwd

    rules: list[tuple[re.Pattern, str]] = [
        (re.compile(r"^/bin/sh$|^sh$|^bash$"), "shell"),
        (re.compile(r"clang\+\+|g\+\+|clang$|gcc$|c\+\+$|cc$"),
         "__compiler__"),
        (re.compile(r"\bcmake$"), "cmake"),
        (re.compile(r"\bninja$"), "ninja"),
        (re.compile(r"\bmake$"), "make"),
        (re.compile(r"\bar$|\branlib$"), "archive"),
        (re.compile(r"\bld$|lld|\bld\.lld$"), "link"),
        (re.compile(r"\bmoc$|\buic$|\brcc$"), "qt-codegen"),
    ]

    category = None
    for pattern, name in rules:
        if pattern.search(exe):
            category = name
            break

    if category == "__compiler__":
        # distinguish compile vs link invocation of the compiler driver
        if "-c" in cmdline:
            category = "compile"
        else:
            category = "link"

    if category is None:
        category = exe or "unknown"

    if is_trycompile:
        category = f"try-compile/{category}"

    return category


def simplify_repr(cmdline: list[str]) -> str:
    path_flags_with_arg = {"-isystem", "-I", "-L", "-iquote", "-idirafter"}
    drop_prefixes = ("-Wno-", "-W", "-rpath", "-Wl,")
    source_suffixes = {
        ".c",
        ".cc",
        ".cpp",
        ".cxx",
        ".c++",
        ".cp",
        ".C",
        ".ixx",
        ".cppm",
        ".cxxm",
        ".mm",
        ".m",
    }

    def is_source_file(tok: str) -> bool:
        if tok.startswith("-"):
            return False
        return Path(tok).suffix in source_suffixes

    def shorten_path(tok: str) -> str:
        if tok.startswith("/") and len(tok) > 40:
            return ".../" + Path(tok).name
        return tok

    if cmdline:
        tool = Path(cmdline[0]).name
        source = next(
            (tok for tok in reversed(cmdline) if is_source_file(tok)), None)
        if source is not None:
            text = f"{tool} {source}"
            if len(text) > 200:
                text = text[:200] + " …"
            return text

    out: list[str] = []
    skip_next = False
    for tok in cmdline:
        if skip_next:
            skip_next = False
            continue

        if tok in path_flags_with_arg:
            out.append(f"{tok} <…>")
            skip_next = True
            continue

        m = re.match(r"^(-isystem|-I|-L|-iquote|-idirafter)(/.+)$", tok)
        if m:
            out.append(f"{m.group(1)} <…>")
            continue

        if tok.startswith(drop_prefixes):
            if out and out[-1] == "[flags…]":
                continue
            out.append("[flags…]")
            continue

        out.append(shorten_path(tok))

    if out:
        out[0] = Path(out[0]).name

    text = " ".join(out)
    if len(text) > 200:
        text = text[:200] + " …"
    return text


def load_events(path: Path) -> list[Event]:
    events: list[Event] = []
    for line in path.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        events.append(Event.model_validate(json.loads(line)))
    return events


def build_processes(events: list[Event]) -> list[Process]:
    procs: dict[int, Process] = {}
    max_ts = max(e.ts for e in events)

    for e in events:
        if e.message == "start":
            procs[e.pid] = Process(
                pid=e.pid,
                ppid=e.info.ppid,
                cmdline=e.info.cmdline,
                cwd=e.info.cwd,
                start=e.ts,
            )
        elif e.message == "end":
            if e.pid in procs:
                procs[e.pid].end = e.ts

    # clamp open-ended processes to the max observed timestamp
    clamped = 0
    for p in procs.values():
        if p.end is None:
            p.end = max_ts
            clamped += 1

    if clamped:
        print(f"clamped {clamped} open-ended processes to max ts",
              file=sys.stderr)

    return list(procs.values())


ROOT_ID = "root"


def format_cmd_hover(cmdline: list[str]) -> str:
    simplified = simplify_repr(cmdline)
    tokens = simplified.split(" ")
    if not tokens:
        return ""

    lines: list[str] = [tokens[0]]  # executable on its own line
    for tok in tokens[1:]:
        # start a new line on each flag, continuation args stay on same line
        if tok.startswith("-"):
            lines.append("  " + tok)
        else:
            lines[-1] += " " + tok
    return "<br>".join(lines)


def build_treemap(procs: list[Process]) -> go.Figure:
    known_pids = {p.pid for p in procs}

    # build stable category colors from matplotlib colormap
    proc_categories = [categorize(p.cmdline, p.cwd) for p in procs]
    color_map = build_color_map(proc_categories)

    ids: list[str] = []
    labels: list[str] = []
    parents: list[str] = []
    values: list[float] = []
    hover: list[str] = []
    colors: list[str] = []

    ids.append(ROOT_ID)
    labels.append("root")
    parents.append("")
    values.append(0.0)
    hover.append("root")
    colors.append("#dddddd")  # root color

    for p, category in zip(procs, proc_categories):
        node_id = str(p.pid)

        if p.ppid is not None and p.ppid in known_pids:
            parent_id = str(p.ppid)
        else:
            parent_id = ROOT_ID

        duration_s = p.duration / 1e6

        ids.append(node_id)
        labels.append(f"{category}<br>{simplify_repr(p.cmdline)}")
        parents.append(parent_id)
        values.append(duration_s)
        colors.append(color_map[category])
        hover.append(
            f"<b>{category}</b><br>"
            f"pid: {p.pid}  ppid: {p.ppid}<br>"
            f"duration: {duration_s:.3f}s<br>"
            f"cwd: {p.cwd}<br>"
            f"cmd:<br>{format_cmd_hover(p.cmdline)}", )

    fig = go.Figure(
        go.Treemap(
            ids=ids,
            labels=labels,
            parents=parents,
            values=values,
            branchvalues="remainder",
            marker=dict(colors=colors),
            customdata=hover,
            hovertemplate="%{customdata}<extra></extra>",
            maxdepth=3,
        ))
    fig.update_layout(margin=dict(t=30, l=10, r=10, b=10))
    return fig


# Main
def main() -> None:
    if len(sys.argv) < 2:
        print(f"usage: {sys.argv[0]} <events.jsonl> [out.html]",
              file=sys.stderr)
        sys.exit(1)

    in_path = Path(sys.argv[1])
    out_path = Path(sys.argv[2]) if len(
        sys.argv) > 2 else Path("proc_treemap.html")

    events = load_events(in_path)
    procs = build_processes(events)
    fig = build_treemap(procs)
    fig.write_html(str(out_path))
    print(f"wrote {out_path}")


if __name__ == "__main__":
    main()
