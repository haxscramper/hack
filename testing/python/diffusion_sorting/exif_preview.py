#!/usr/bin/env python

from dominate import document
import dominate.tags as tags
from dominate.util import text
import os
import re
from PIL import Image
import json
from pathlib import Path
from datetime import datetime
from dataclasses import dataclass, field
from beartype.typing import List, Union, Set
from beartype import beartype
import functools
import sys

import logging

log = logging.getLogger("preview")
log.setLevel(logging.DEBUG)

log.setLevel(logging.DEBUG)
handler = logging.StreamHandler()
handler.setLevel(logging.DEBUG)
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
handler.setFormatter(formatter)
log.addHandler(handler)
log.info(f"Changing path to {sys.argv[1]}")

workspace_root = Path(sys.argv[1])

output_html = workspace_root.joinpath("images_with_exif.html")


@beartype
def as_multiline(txt: str):
    for idx, part in enumerate(txt.split("\n")):
        if idx != 0:
            tags.br()

        text(part)


@beartype
@dataclass
class LoRATag:
    id2: str
    id3: str
    weight: str


@beartype
@dataclass
class Tag:
    text: str
    amplifier: float = 1.0
    parens: int = 0
    categories: List[str] = field(default_factory=list)

    def __repr__(self) -> str:
        if self.amplifier == 1.0:
            result = self.text

        else:
            result = f"{self.text}:{self.amplifier}"

        return "(" * self.parens + result + ")" * self.parens


ParsedTag = Union[Tag, LoRATag, str]


@beartype
@dataclass
class TagCategory():
    tags: Set[str]
    name: str

    def matches(self, tag: str) -> bool:
        return tag in self.tags

    @beartype
    @staticmethod
    def from_file(path: Path) -> "TagCategory":
        result = TagCategory(name=path.stem.lower(), tags=set())
        for line in path.read_text().splitlines():
            if line.startswith("#"):
                continue

            else:

                def add(text: str):
                    result.tags.add(text.strip().strip("\\").lower())

                add(line)
                if "(" in line:
                    add(line.split("(")[0])

        return result


@beartype
class PromptParser:

    def __init__(self, category_dicts: List[TagCategory]):
        self.break_token = "BREAK"
        self.category_dicts = category_dicts

    def categories(self, tag: str) -> List:
        result = []
        for cat in self.category_dicts:
            if cat.matches(tag):
                result.append(cat.name)

        return result

    def parse(self, prompt: str) -> List[ParsedTag]:
        tokens = self.tokenize(prompt)
        return self.parse_tokens(tokens)

    def tokenize(self, prompt: str) -> List[str]:
        prompt = prompt.replace(r"\s+", " ")
        prompt = re.sub(r"(,\s*)+", ",", prompt)
        result = [
            tok for tok in re.split(r"(,|BREAK|<[^>]+>|[\(\)])", prompt)
            if (tok not in [","]) and len(tok.strip()) != 0
        ]
        # print(f"{prompt} -> {result}")
        return result

    def parse_tokens(self, tokens: List[str]) -> List[ParsedTag]:
        result = []
        current_level = 0

        def add_tag(text: str, amplifier: float = 1.0, level: int = 0):
            result.append(
                Tag(text=text.strip(),
                    amplifier=amplifier,
                    parens=level,
                    categories=self.categories(text.strip())))

        for token in tokens:
            if token == "":
                continue
            elif token == "(":
                current_level += 1

            elif token == ")":
                if current_level > 0:
                    current_level -= 1

            elif token == "BREAK":
                result.append(self.break_token)

            elif re.match(r"^<[^>]+>$", token):
                parts = token[1:-1].split(":")
                if len(parts) == 3:
                    result.append(LoRATag(parts[0], parts[1], parts[2]))

            elif ":" in token:
                tag, amp = token.split(":")
                tag = tag.strip()
                if tag:
                    try:
                        add_tag(tag, float(amp), level=current_level)

                    except ValueError:
                        add_tag(tag, level=current_level)

            else:
                if token:
                    add_tag(token.strip(), 1.0, level=current_level)

        return result


borders = dict(border=1, style='border-collapse: collapse; width: 100%;')

categories: List[TagCategory] = []
for directory in ["reference_tag_categories", "manual_tag_categories"]:
    log.info(f"Parsing tag categories from {directory}")
    for file in workspace_root.joinpath(directory).rglob("*.txt"):
        categories.append(TagCategory.from_file(file))

log.info(f"Found {len(categories)} tag category descriptors")

def main_impl():
    with document(title="Images and EXIF Metadata") as doc:
        log.info("Creating HTML")
        with tags.table(
                border=1,
                style=
                'border-collapse: collapse; width: 100%; table-layout: fixed;'
        ):
            with tags.thead():
                tags.th("Image")
                tags.th("Prompt", style="width: 35%;")
                tags.th(style="width: 35%;")
                tags.th(style="width: 10%;")

            for reference_dir in [
                    workspace_root.joinpath("reference_tensor_art"),
                    workspace_root.joinpath("tensor_saved_high_res"),
            ]:
                log.info(f"Getting files from {reference_dir}")
                for filename in sorted(
                        reference_dir.glob("*.png"),
                        key=lambda it: datetime.fromtimestamp(it.stat().
                                                              st_mtime),
                        reverse=True,
                ):
                    img = Image.open(filename)
                    metadata = img.info
                    full_json = json.loads(
                        metadata["prompt"]) if "prompt" in metadata else {}

                    # if "prompt" not in metadata:
                    if "Xqmn92N5vg" in str(filename):
                        log.warning(f"No prompt metadata for {filename}. meta {metadata}")

                    def rowname(name: str):
                        with tags.td(style="text-align:center;"):
                            with tags.b():
                                as_multiline(name.upper())

                    with tags.tr():
                        tags.td(
                            tags.img(src=str(filename.resolve()), width="300"))

                        def add_prompt(prompt: str) -> List[ParsedTag]:
                            parser = PromptParser(category_dicts=categories)
                            try:
                                parsed = parser.parse(prompt)
                            except Exception as e:
                                log.error("-------")
                                log.error(prompt)
                                log.exception(e, stack_info=True)
                                parsed = None

                            as_multiline(prompt)
                            return parsed

                        with tags.td():
                            with tags.table(**borders):
                                parsed_positive: List[ParsedTag] = []
                                parsed_negative: List[ParsedTag] = []
                                rowname("prompt text")
                                with tags.tr():
                                    with tags.td():
                                        if "1000" in full_json:
                                            parsed_positive = add_prompt(
                                                str(full_json["1000"]["inputs"]
                                                    ["text"]))
                                            del full_json["1000"]

                                        elif "1000000000" in full_json:
                                            parsed_positive = add_prompt(
                                                str(full_json["1000000000"]
                                                    ["inputs"]["text"]))
                                            del full_json["1000000000"]

                                        elif "parameters" in metadata:
                                            as_multiline(
                                                str(metadata["parameters"]))

                                rowname("negative prompt")
                                with tags.tr():
                                    with tags.td():
                                        if "1001" in full_json:
                                            parsed_negative = add_prompt(
                                                str(full_json["1001"]["inputs"]
                                                    ["text"]))
                                            del full_json["1001"]

                                if parsed_positive:
                                    rowname("parsed positive")
                                    with tags.tr():
                                        with tags.td():
                                            as_multiline(str(parsed_positive))

                                    all_categories = [
                                        set(tag.categories)
                                        for tag in parsed_positive
                                        if isinstance(tag, Tag)
                                    ]

                                    this_prompt_categories = list(
                                        sorted(
                                            functools.reduce(
                                                lambda prev, new: prev.union(
                                                    new),
                                                all_categories,
                                                set(),
                                            )))

                                    rowname("prompt categories")
                                    with tags.tr():
                                        with tags.td():
                                            as_multiline(
                                                str(", ".join(
                                                    this_prompt_categories)))

                                    for category in [
                                            "Characters",
                                    ]:
                                        matching = [
                                            tag for tag in parsed_positive
                                            if isinstance(tag, Tag) and (
                                                category in tag.categories)
                                        ]
                                        if matching:
                                            rowname("cat")
                                            with tags.tr():
                                                with tags.td():
                                                    as_multiline(
                                                        f"CATEGORY: {category}:{matching}"
                                                    )

                                rowname("parsed negative")
                                if parsed_negative:
                                    with tags.tr():
                                        with tags.td():
                                            as_multiline(str(parsed_negative))

                        if "generation_data" in metadata:
                            tags.td(
                                json.dumps(
                                    json.loads(metadata["generation_data"])))

                        elif "prompt" in metadata:
                            tags.td(
                                json.dumps(
                                    json.loads(metadata["prompt"])))

                        else:
                            tags.td(text("no generation_data"))

                        with tags.td():
                            generation_data = json.loads(
                                metadata["generation_data"]
                            ) if "generation_data" in metadata else {}

                            if "models" in generation_data:
                                with tags.table(
                                        border=1,
                                        style="border-collapse: collapse;"):
                                    for model in generation_data["models"]:
                                        with tags.tr():
                                            tags.td(model["modelFileName"], style="word-break: break-all;")
                                            tags.td(model["weight"])

                            else:
                                text("no lora")

                    with tags.tr():
                        with tags.td(style="text-align:center;"):
                            text("{}x{} {} on {}".format(
                                *img.size,
                                filename.name,
                                datetime.fromtimestamp(
                                    filename.stat().st_mtime),
                            ))

    with open(output_html, "w") as f:
        f.write(str(doc))

    log.info(f"HTML file created at {output_html}")


if True:
    main_impl()


def test_parse(prompt: str):
    return PromptParser(categories).parse(prompt)


for test in [
        "dog", "cat, dog", "(cat), dog", "((cat)), dog:2",
        "cat:0.5, dog:2, elephant", "((cat, dog)), (elephant:2), giraffe",
        "cat, <id2:id3:1>, dog",
        "((cat, (dog))), cat:3, <id2:id3:0.5>, BREAK, elephant, ((tiger, (lion:2)))",
        "cat, dog, elephant, tiger, lion, giraffe, bear, wolf, fox, rabbit, deer, moose, squirrel, hedgehog, bat, owl, rat, mouse, duck, swan",
        "((cat)), ((dog)), (((elephant))), ((((tiger)))), cat:2, dog:1.5, elephant:0.3, tiger:4, ((giraffe:2), (bear:3)), <idA:idB:2>",
        "cat, dog, ((elephant, (tiger, ((lion)))), giraffe), ((bear:2, wolf:0.5)), fox:1.2, (rabbit, (deer, (moose))), (squirrel, (hedgehog, (bat, (owl)))), rat, (mouse), (duck), (swan)",
        "Hana Midorikawa (Prison School)"
]:
    parsed = test_parse(test)
    # print(parsed)
    # print([tag.categories for tag in parsed if isinstance(tag, Tag)])
