#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from nltk.parse.corenlp import CoreNLPParser
import nltk
from typing import List
from rich.table import Table
from rich.console import Console
import rich


def print_to_table(
    tree: nltk.tree.Tree,
    level: int,
    fill: List[List[str]],
) -> int:

    fill[-1][-tree.height()] = tree.label()
    def rec(
        tree: nltk.tree.Tree,
        level: int,
        fill: List[List[str]],
        offset: int,
    ) -> int:
        for sub in tree:
            text = ""
            row = -1
            if type(sub) == nltk.tree.Tree:
                offset = rec(sub, level + 1, fill, offset)
                text = sub.label()
                row = -sub.height()
            else:
                offset += 1
                text = f"[green]{sub}[/]"

            fill[offset - 1][row] = text

        return offset

    rec(tree, level, fill, 0)


def count_words(tree: nltk.tree.Tree):
    result = 0
    for sub in tree:
        if type(sub) == nltk.tree.Tree:
            result += count_words(sub)

        else:
            result += 1

    return result


parser = CoreNLPParser()
parse = next(
    parser.raw_parse(
        "I put the book in the box on the table. It fell down immediately."
    )
)
parse.pretty_print()

fill = [[""] * parse.height() for word in range(count_words(parse))]
flat = [str(it) for it in parse.flatten()]

for row in range(len(fill)):
    fill[row][-1] = flat[row]

table = Table()
console = Console()

print_to_table(parse, 0, fill)

for col in range(len(fill[0])):
    table.add_column(str(col))

table.add_column("long names")

pos_names = {
    "S": "sentence",
    "PRP": "pronoun",
    "VBD": "past tense",
    "DT": "determiner",
    "NN": "noun",
    "IN": "subordinating",
    ".": "punct",
    "RP": "particle",
    "RB": "averb"
}

for row in fill:
    table.add_row(
        *row[0:-1],
        "[green]\"" + row[-1] + "\"[/]",
        "[yellow]" + pos_names[row[-2]] + "[/]"
    )


console.print(table, markup=True)
