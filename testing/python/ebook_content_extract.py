#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov  8 10:29:47 2022

@author: haxscramper
"""

import ebooklib
from ebooklib import epub
from lxml import etree
import json
from typing import List
from glob import glob
import os


def flatparagraph(dom) -> str:
    result = ""
    if dom.text and 0 < len(dom.text):
        result += dom.text.strip()

    for item in dom:
        result += flatparagraph(item)

    return result


def flatten(dom) -> List[str]:
    result = []
    if dom.tag in ["span", "p", "sup", "b", "i", "h1", "h2", "h3", "h4"]:
        flat = flatparagraph(dom)
        if 0 < len(flat):
            result.append(flat)

    elif dom.tag in ["body", "div", "blockquote", "a", "ul", "li"]:
        for item in dom:
            result += flatten(item)

    elif dom.tag in ["svg", "img", "hr", "br"]:
        pass

    else:
        assert False, dom.tag

    return result


def split_content(items: List[str]):
    current_name = None
    current_items = []
    pairs = {}
    for item in items:
        if 0 == len(item):
            continue

        split = item.split()
        first = split[0]
        if 1 < len(split) and first in ["A", "I"]:
            first = split[1]

        if all(it.isupper() or not it.isalnum() for it in first):
            if current_name:
                pairs[current_name] = current_items

            if ":" in item:
                words = [
                    word.strip() for word in item.split(":") if 0 < len(word)
                ]
                if 1 == len(words):
                    current_items = []
                    current_name = item

                else:
                    current_name = words[0]
                    current_items = words[1:]

            else:
                current_items = []
                current_name = item

        elif current_name:
            current_items.append(item)

    return pairs


fullres = {}

for infile in glob("books/*.epub"):
    name = os.path.basename(infile)
    outdir = f"/tmp/{name}"
    if not os.path.exists(outdir):
        os.mkdir(outdir)

    book = epub.read_epub(infile)
    for doc in book.get_items_of_type(ebooklib.ITEM_DOCUMENT):
        if type(doc) == epub.EpubHtml:
            bookres = {}
            dom = etree.HTML(doc.get_content())
            body = dom[1]
            flat = flatten(body)
            has_def = 1 < len(flat)
            for item in flat:
                if item.startswith("DEFINITION"):
                    has_def = True

            if has_def:
                data = {"title": flat[0], "content": split_content(flat[1:-1])}
                outfile = os.path.join(
                    outdir, flat[0].replace("/", "_") + ".json"
                )
                bookres[flat[0]] = data
                with open(outfile, "w") as file:
                    file.write(json.dumps(data, indent=4))

            else:
                if 0 == len(flat):
                    outfile = os.path.join(
                        outdir, doc.get_name().replace("/", "_") + ".html"
                    )

                    with open(outfile, "w") as file:
                        file.write(
                            etree.tostring(body, pretty_print=True).decode(
                                "utf-8"
                            )
                        )

                else:
                    outfile = os.path.join(
                        outdir, doc.get_name().replace("/", "_") + ".txt"
                    )
                    with open(outfile, "w") as file:
                        file.write("\n".join(flat))

            fullres[name] = bookres

with open("/tmp/full.json", "w") as file:
    file.write(json.dumps(fullres, indent=4))
