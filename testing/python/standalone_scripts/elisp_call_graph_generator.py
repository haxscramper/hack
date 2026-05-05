#!/usr/bin/env -S uv run --script
# /// script
# dependencies = [
#   "sexpdata",
#   "graphviz",
#   "beartype"
# ]
# ///

from __future__ import annotations

import argparse
import hashlib
from dataclasses import dataclass, field
from pathlib import Path
from beartype.typing import Iterable
import logging
from beartype import beartype

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
)

import graphviz
import sexpdata

DEF_FORMS = {"defun", "cl-defun", "defsubst"}
EDGE_STYLES = {
    "call": {
        "color": "black",
        "style": "solid",
        "label": "call"
    },
    "quote": {
        "color": "royalblue4",
        "style": "dashed",
        "label": "'"
    },
    "sharpquote": {
        "color": "darkgreen",
        "style": "dotted",
        "label": "#'"
    },
}


@beartype
@dataclass
class FunctionInfo:
    name: str
    file: Path
    refs: set[tuple[str, str]] = field(default_factory=set)  # (kind, target)


@beartype
@dataclass
class DirTree:
    name: str
    children: dict[str, "DirTree"] = field(default_factory=dict)
    funcs: list[str] = field(default_factory=list)


@beartype
def symbol_name(obj) -> str | None:
    if isinstance(obj, sexpdata.Symbol):
        return str(obj)

    return None


@beartype
def quoted_value(obj):
    if isinstance(obj, sexpdata.Quoted):
        return obj.x
    return None


@beartype
def is_sequence(obj) -> bool:
    return isinstance(obj, list) or isinstance(obj, tuple)


def iter_forms_in_file(text: str) -> Iterable:
    wrapped = f"(progn\n{text}\n)"
    parsed = sexpdata.loads(wrapped)[1:]
    for form in parsed:
        yield form


@beartype
def collect_refs(expr, out: set[tuple[str, str]]) -> None:
    qv = quoted_value(expr)
    if qv is not None:
        qsym = symbol_name(qv)
        if qsym is not None:
            out.add(("quote", qsym))
        return

    sym = symbol_name(expr)
    if sym is not None:
        if sym.startswith("#'") and len(sym) > 2:
            out.add(("sharpquote", sym[2:]))
        return

    if not is_sequence(expr) or not expr:
        return

    head_name = symbol_name(expr[0])
    if head_name is not None:
        out.add(("call", head_name))

    for part in expr:
        collect_refs(part, out)


@beartype
def parse_defun(form, file: Path) -> FunctionInfo | None:
    if not (is_sequence(form) and len(form) >= 3):
        return None

    head = symbol_name(form[0])
    if head not in DEF_FORMS:
        return None

    name = symbol_name(form[1])
    if name is None:
        return None

    body_start = 3
    if len(form) > 3 and isinstance(form[3], str):
        body_start = 4

    refs: set[tuple[str, str]] = set()
    for expr in form[body_start:]:
        collect_refs(expr, refs)

    return FunctionInfo(name=name, file=file, refs=refs)


@beartype
def node_id(func_name: str) -> str:
    h = hashlib.sha1(func_name.encode("utf-8")).hexdigest()[:16]
    return f"fn_{h}"


@beartype
def add_to_tree(root: DirTree, rel_dir: Path, func_name: str) -> None:
    current = root
    for part in rel_dir.parts:
        if part in ("", "."):
            continue
        current = current.children.setdefault(part, DirTree(name=part))
    current.funcs.append(func_name)


@beartype
def build_clusters(g, tree: DirTree, id_map: dict[str, str],
                   cluster_path: tuple[str, ...], label: str) -> None:
    cluster_name = "cluster_" + "_".join(
        cluster_path) if cluster_path else "cluster_root"
    with g.subgraph(name=cluster_name) as sg:
        sg.attr(label=label)
        sg.attr(color="gray40")
        for fn in sorted(tree.funcs):
            sg.node(id_map[fn], label=fn)
        for child_name in sorted(tree.children):
            child = tree.children[child_name]
            build_clusters(
                sg,
                child,
                id_map=id_map,
                cluster_path=(*cluster_path, child_name),
                label=child_name,
            )


@beartype
def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate Emacs Lisp function call graph.")
    parser.add_argument(
        "root",
        type=Path,
        help="Root directory to scan recursively for .el files.")
    parser.add_argument("output",
                        type=Path,
                        help="Output file path, e.g. callgraph.svg")
    args = parser.parse_args()

    root = args.root.resolve()
    el_files = sorted(root.rglob("*.el"))

    all_funcs: dict[str, FunctionInfo] = {}
    for file in el_files:
        logging.info(f"{file}")
        text = file.read_text(encoding="utf-8")
        for form in iter_forms_in_file(text):
            info = parse_defun(form, file)
            if info is not None:
                all_funcs[info.name] = info

    defined = set(all_funcs.keys())

    out_path = args.output.resolve()
    fmt = out_path.suffix.lstrip(".")
    stem = out_path.with_suffix("")

    g = graphviz.Digraph("elisp_callgraph", format=fmt)
    g.attr(rankdir="LR")
    g.attr("node", shape="box", fontsize="10")
    g.attr("edge", fontsize="9")

    id_map = {name: node_id(name) for name in defined}

    tree = DirTree(name=root.name)
    for fn, info in all_funcs.items():
        rel_dir = info.file.parent.relative_to(root)
        add_to_tree(tree, rel_dir, fn)

    build_clusters(g, tree, id_map=id_map, cluster_path=(), label=root.name)

    for src_name, src_info in sorted(all_funcs.items()):
        src_id = id_map[src_name]
        edges_by_kind: dict[tuple[str, str], None] = {}
        for kind, target in src_info.refs:
            if target in defined:
                edges_by_kind[(kind, target)] = None

        for kind, target in sorted(edges_by_kind):
            style = EDGE_STYLES[kind]
            g.edge(src_id, id_map[target], **style)

    g.render(filename=str(stem), cleanup=True)


if __name__ == "__main__":
    main()
