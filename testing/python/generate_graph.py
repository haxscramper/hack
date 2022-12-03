#!/usr/bin/env python

import yaml
import textwrap
from typing import *


def indent(text, amount, ch=" "):
    padding = amount * ch
    return "".join(padding + line for line in text.splitlines(True))


def format_text(text: str, width: int = 40) -> str:
    return (
        "\\l".join(
            textwrap.wrap(text, width, break_long_words=False, break_on_hyphens=False)
        )
        + "\\l"
    )


def to_org(item, level):
    label = item["text"]
    name = item["name"]
    ing = "*" * level

    todo = "TODO"
    if "todo" in item and item["todo"] == "done":
        todo = "DONE"
    elif "todo" in item and item["todo"] == "wip":
        todo = "WIP"

    deps = []
    if "deps" in item:
        for dep in item["deps"]:
            other = dep["name"]
            reason = ""
            if "reason" in dep:
                reason = dep["reason"]

            deps.append(f"""
**{ing} [[id:{other}]]
  :properties:
  :blocker: t
  :end:

{reason}
""")


    deps = "\n".join(deps)
    return f"""
*{ing} {todo} {label}
  :properties:
  :id: {name}
  :end:

{deps}
"""


def to_graph(item) -> (str, List[str]):
    label = item["text"]
    name = item["name"]
    style = "style=filled,fillcolor="
    if "todo" in item and item["todo"] == "done":
        style += "green"
    elif "todo" in item and item["todo"] == "wip":
        style += "orange"

    else:
        style += "yellow"

    result = f'{name}[label="{label}>> {name} <<",{style}];\n'
    deps = []

    if "deps" in item:
        for dep in item["deps"]:
            other = dep["name"]
            item = f"\n{other} -> {name}"
            if "reason" in dep:
                reason = dep["reason"]
                item += f'[label="{reason}"]'

            deps.append(item)

    return (result, deps)


def rec_write(body: str, item, level: int) -> (str, List[str]):
    result = ""
    links = []
    if "group" in item:
        group = item["group"]
        result += f"subgraph cluster_{group} {{\n"
        if "pass" in item:
            result += indent(item["pass"], 4) + "\n"
        label = format_text(item["label"], 120)
        result += f'    label="{label}";\n'
        if "items" in item:
            for nested in item["items"]:
                (body, link) = rec_write(body, nested, level + 1)
                result += body
                links += link

        result += "\n}\n"

    else:
        (body, link) = to_graph(item)
        result += indent(body, level * 4)
        links += link

    return (result, links)


def rec_write_org(body: str, item, level: int) -> (str, List[str]):
    result = ""
    if "group" in item:
        group = item["group"]
        label = item["label"]
        result += f"""
* {label}
  :properties:
  :id: {group}
  :end:

"""
        if "items" in item:
            for nested in item["items"]:
                result += rec_write_org(body, nested, level + 1)

    else:
        result += to_org(item, level)

    return result

body = ""
doc = yaml.load(open("graph.yaml").read(), Loader=yaml.Loader)
all_links = []

with open("/tmp/res.org", "w") as file:
  for it in doc:
      file.write(rec_write_org(body, it, 0))

for it in doc:
    (chunk, links) = rec_write(body, it, 0)
    body += chunk
    all_links += links

l = indent(";\n".join(all_links), 4)

result = f"""
digraph G {{
    node[shape=rect, fontname=consolas,color=black,penwidth=2];
    edge[fontname=consolas,penwidth=2];
    graph[fontname=consolas];
    nodesep=0.8;
    rankdir=LR;
    splines=polyline;
{indent(body, 4)}
{l};
}}
"""

import os

with open("graph.dot", "w+") as file:
    file.write(result)

os.system("dot -Tpng graph.dot > graph.png")
os.system("dot -Tsvg graph.dot > graph.svg")
