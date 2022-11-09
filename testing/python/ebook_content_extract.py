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
import re
import os
import shutil


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


def match_check(item: str) -> bool:
    return len(item) < 60 and (
        re.match(r"([A-Z]{2,})|(\d+\.)|(\d+\s+)", item)
        or " in Alphabetical Order" in item
    )


def maybe_split(item: str) -> List[str]:
    split = item.split(";")
    if 2 < len(split):
        return [it.strip() for it in split]

    else:
        return [item]


def split_content(items: List[str]):
    current_name = None
    current_items = []
    pairs = {}
    for item in items:
        if 0 == len(item):
            continue

        if match_check(item):
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
            current_items += maybe_split(item)

    if current_name and 0 < len(current_items):
        pairs[current_name] = current_items

    return pairs


fullres = {}

for infile in glob("books/*.epub"):
    name = os.path.basename(infile)
    outdir = f"/tmp/{name}"
    if os.path.exists(outdir):
        shutil.rmtree(outdir)

    os.mkdir(outdir)

    book = epub.read_epub(infile)
    fullres[name] = []
    for doc in book.get_items_of_type(ebooklib.ITEM_DOCUMENT):
        if type(doc) == epub.EpubHtml:
            bookres = []
            dom = etree.HTML(doc.get_content())
            body = dom[1]
            flat = flatten(body)
            if len(flat) == 0:
                continue

            skip_index = 1
            while skip_index < len(flat) and not match_check(
                flat[skip_index - 1]
            ):

                skip_index += 1

            if skip_index == len(flat):
                continue

            content = split_content(flat[skip_index:-1])
            data = {"title": flat[skip_index - 1], "content": content}
            res = data["title"].replace("/", "_")
            outfile = os.path.join(outdir, res + ".json")
            if 0 < len(content):
                bookres.append(data)
                with open(outfile, "w") as file:
                    file.write(json.dumps(data, indent=4, ensure_ascii=False))

            if 0 < len(bookres):
                fullres[name].append(bookres)

    if len(fullres[name]) == 0:
        del fullres[name]

with open("/tmp/full.json", "w") as file:
    file.write(json.dumps(fullres, indent=4, ensure_ascii=False))
