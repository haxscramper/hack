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
from dataclasses import dataclass, field, replace
from beartype.typing import List, Union, Set, Tuple, Optional, Dict
from beartype import beartype
import functools
import sys
from pydantic import BaseModel, Field, validator, AliasChoices
from pprint import pformat, pprint
import itertools
import more_itertools

import logging

log = logging.getLogger("preview")

log.setLevel(logging.DEBUG)

log.setLevel(logging.DEBUG)
handler = logging.StreamHandler()
handler.setLevel(logging.DEBUG)
formatter = logging.Formatter("%(name)s - %(levelname)s - %(message)s")
handler.setFormatter(formatter)
log.addHandler(handler)
log.info(f"Changing path to {sys.argv[1]}")

if __name__ == "__main__":
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
class TagCategory():
    tags: Set[str]
    name: str
    aliases: Dict[str, str] = field(default_factory=dict)

    @beartype
    def clean_text(tag: str) -> str:
        return re.sub(RX_DELETE, "", tag).lower()

    @beartype
    def no_alias(self, tag: str) -> str:
        clean = TagCategory.clean_text(tag)
        if clean in self.aliases:
            return self.aliases[clean]

        else:
            return clean

    @beartype
    def matches(self, tag: str) -> bool:
        return self.no_alias(tag) in self.tags

    @beartype
    @staticmethod
    def from_file(path: Path, name: Optional[str] = None) -> "TagCategory":
        result = TagCategory(name=name or path.stem.lower(), tags=set())
        for line in path.read_text().splitlines():
            if line.startswith("#"):
                continue

            else:
                if "," in line:
                    multiple = [
                        TagCategory.clean_text(it) for it in line.split(",")
                    ]
                    line = multiple[0]
                    for item in multiple[1:]:
                        result.aliases[TagCategory.clean_text(item)] = line

                def add(text: str):
                    result.tags.add(TagCategory.clean_text(text))

                add(line)
                if "(" in line:
                    add(line.split("(")[0])

        log.info(result.aliases)

        return result


@beartype
@dataclass
class Tag:
    text: str
    amplifier: float = 1.0
    parens: int = 0
    category: Optional[TagCategory] = None

    def __repr__(self) -> str:
        if self.amplifier == 1.0:
            result = self.text

        else:
            result = f"{self.text}:{self.amplifier}"

        return "<{}{}{}{}>".format(
            "(" * self.parens,
            result,
            ")" * self.parens,
            ":" + self.category.name if self.category else "",
        )


ParsedTag = Union[Tag, LoRATag, str]

RX_DELETE = re.compile(r"[- _,()/\\:]")


@beartype
class PromptParser:

    def __init__(self, category_dicts: List[TagCategory]):
        self.break_token = "BREAK"
        self.category_dicts = category_dicts

    def categories(self, tag: str) -> Optional[TagCategory]:
        for cat in self.category_dicts:
            if cat.matches(tag):
                return cat

        return None

    def parse(self, prompt: str) -> List[ParsedTag]:
        tokens = self.tokenize(prompt)
        return self.parse_tokens(tokens)

    def tokenize(self, prompt: str) -> List[str]:
        prompt = prompt.replace(r"\s+", " ")
        prompt = re.sub(r"(,\s*)+", ",", prompt)
        result = [
            tok for tok in re.split(r"(\n+|,|BREAK|<[^>]+>|[\(\)])", prompt)
            if (tok not in [","]) and len(tok.strip()) != 0
        ]
        # print(f"{prompt} -> {result}")
        return result

    def parse_tokens(self, tokens: List[str]) -> List[ParsedTag]:
        result = []
        current_level = 0

        def add_tag(text: str, amplifier: float = 1.0, level: int = 0):
            cats = self.categories(text.strip())
            result.append(
                Tag(text=cats.no_alias(text.strip()) if cats else text.strip(),
                    amplifier=amplifier,
                    parens=level,
                    category=cats))

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


class ModelParam(BaseModel):
    name: str = ""
    weight: float = 1.0


@beartype
@dataclass
class ImageTags:
    Character: Optional[str] = None


default_negative_prompt = Path(
    "~/tmp/default_negative_prompt.txt").expanduser().read_text()


@beartype
@dataclass
class ImageParams:
    prompt: str = ""
    negative_prompt: str = ""
    generation_data: str = ""
    loras: List[ModelParam] = field(default_factory=list)
    model: str = ""
    size: Tuple[int, int] = (896, 1088)
    ImagePath: Optional[Path] = None
    tags: ImageTags = field(default_factory=lambda: ImageTags())
    parsed_prompt: Optional[List[ParsedTag]] = None
    parsed_negative_prompt: Optional[List[ParsedTag]] = None
    group_key: str = ""
    in_group_index: int = 0
    image_time: datetime = field(default_factory=lambda: datetime.now())
    sampler: str = "Euler a"
    steps: int = 25
    cfgScale: int = 7
    clipSkip: int = 2
    sdVae: str = "Automatic"
    etaNoiseSeedDelta: int = 31337

    def get_tag_formatted_prompt(self) -> str:
        return ",".join(it.text for it in self.parsed_prompt
                        if isinstance(it, Tag))

    def get_tag_formatted_negative_prompt(self) -> str:
        return ",".join(it.text for it in self.parsed_negative_prompt
                        if isinstance(it, Tag))

    def get_infinite_browser_extra(self) -> dict:
        meta = {
            "Model": self.model,
            "Lora hashes": ",".join(f"{it.name}" for it in self.loras),
            "Steps": self.steps,
            "TA defaulted": json.dumps(self.get_defaulted_tensor_art_prompt(), indent=2),
        }
        extra = dict(
            lora=[dict(name=it.name, value=it.weight) for it in self.loras],
            meta=meta,
            pos_prompt=[
                it.text for it in self.parsed_prompt if isinstance(it, Tag)
            ],
        )

        return extra

    def get_defaulted_tensor_art_prompt(self) -> dict:
        return dict(
            prompt=self.prompt,
            negativePrompt=default_negative_prompt,
            width=896,
            height=1088,
            samplerName=self.sampler,
            steps=25,
            cfgScale=self.cfgScale,
            clipSkip=self.clipSkip,
            sdVae=self.sdVae,
            etaNoiseSeedDelta=self.etaNoiseSeedDelta,
        )

    def get_tensor_art_prompt(self) -> dict:
        return dict(
            prompt=self.prompt,
            negativePrompt=self.negative_prompt,
            width=self.size[0],
            height=self.size[1],
            samplerName=self.sampler,
            steps=self.steps,
            cfgScale=self.cfgScale,
            clipSkip=self.clipSkip,
            sdVae=self.sdVae,
            etaNoiseSeedDelta=self.etaNoiseSeedDelta,
        )

    def get_normalized_a1111(self) -> str:
        return f"""
{self.get_tag_formatted_prompt()}
Negative prompt: {self.get_tag_formatted_negative_prompt()},
Steps: {self.steps}, Model: {self.model}
"""


PATTERN = re.compile("(" + "|".join(
    re.escape(key) for key in [
        "Prompt",
        "Negative prompt",
        "Seed",
        "Size",
        "VAE",
        "Denoising strength",
        "Steps",
        "Sampler",
        "KSampler",
        "Schedule",
        "CFG scale",
        "Clip skip",
        "Model",
        "LoRA",
        "Hires resize",
        "Hires steps",
        "Hires upscaler",
        "ADetailer model",
        "ADetailer negative prompt",
        "ADetailer confidence",
        "Guidance",
    ]) + "):")


class SDGenParams(BaseModel):
    Prompt: str
    NegativePrompt: str = Field(alias="Negative prompt")
    Seed: str
    Size: str
    Steps: str
    VAE: str
    Sampler: str
    CFGScale: str = Field(alias="CFG scale")
    DenoisingStrength: str = Field(alias="Denoising strength")
    KSampler: str
    Schedule: str
    ClipSkip: str = Field(alias="Clip skip")
    Model: str
    LoRA: Optional[str] = None
    HiresResize: Optional[str] = Field(alias="Hires resize", default=None)
    HiresSteps: Optional[str] = Field(alias="Hires steps", default=None)
    HiresUpscaler: Optional[str] = Field(alias="Hires upscaler", default=None)


def parse_parameters(param_string: str) -> Optional[SDGenParams]:
    matches = list(re.finditer(PATTERN, param_string))
    results = {}

    for i in range(len(matches)):
        start = matches[i].start()
        end = matches[i].end()

        if i + 1 < len(matches):
            next_start = matches[i + 1].start()
        else:
            next_start = len(param_string)

        key = param_string[start:end - 1].strip()
        value = param_string[end:next_start].strip()

        results[key] = value

    if "Prompt" not in results:
        return None
    else:
        return SDGenParams.model_validate(results)


class TArtV1ModelInfo(BaseModel):
    label: str
    modelId: str
    modelFileId: str
    weight: float
    modelFileName: str
    baseModel: str
    hash: str


class TArtV1BaseModelInfo(BaseModel):
    label: str
    modelId: str
    modelFileId: str
    modelFileName: str
    baseModel: str
    hash: str


class TArtV1SDXLSettings(BaseModel):
    refiner: Optional[bool] = None


class TArtV1MainData(BaseModel):
    models: List[TArtV1ModelInfo] = Field(default_factory=list)
    prompt: str
    negativePrompt: str
    width: int
    height: int
    imageCount: int
    samplerName: str
    steps: int
    cfgScale: int
    seed: Optional[str] = None
    clipSkip: int
    baseModel: TArtV1BaseModelInfo
    sdxl: Optional[TArtV1SDXLSettings] = None
    workEngine: str = "TAMS_V2"
    sdVae: str = "Automatic"
    etaNoiseSeedDelta: int = 31337


categories: List[TagCategory] = []
categories.append(
    TagCategory.from_file(Path("~/tmp/Characters.txt").expanduser(),
                          name="Character"))

WARN_IDX = 0


def get_image_params(path: Path) -> ImageParams:
    img = Image.open(path)
    metadata = img.info

    res = ImageParams(image_time=datetime.fromtimestamp(path.stat().st_mtime))
    res.ImagePath = path
    if path.with_suffix(".txt").exists():
        text_gen_data = path.with_suffix(".txt").read_text()
        sd = parse_parameters(text_gen_data)

        if sd:
            res.prompt = sd.Prompt.lstrip(",")
            res.negative_prompt = sd.NegativePrompt.lstrip(",")
            res.generation_data = text_gen_data
            res.sampler = sd.Sampler.strip(",")
            res.steps = int(sd.Steps.strip(","))
            res.model = sd.Model.strip(",")
            res.clipSkip = int(sd.ClipSkip.strip(","))
            res.cfgScale = int(sd.CFGScale.strip(","))
            wStr, hStr = sd.Size.strip(",").split("x")
            res.size = (int(wStr), int(hStr))
            if sd.LoRA:
                for lora in [it.strip() for it in sd.LoRA.split(",")]:
                    if lora:
                        if ":" in lora:
                            name, weight = lora.split(":")

                        else:
                            name = lora
                            weight = 1.0

                        res.loras.append(
                            ModelParam(
                                name=name,
                                weight=float(weight),
                            ))

    elif "generation_data" in metadata:
        generation = metadata["generation_data"].strip('\x00')
        try:
            full_json = json.loads(generation)
            try:
                load = TArtV1MainData.model_validate(full_json)
                res.prompt = load.prompt
                res.negative_prompt = load.negativePrompt
                res.generation_data = json.dumps(full_json)
                res.sampler = load.samplerName
                res.steps = load.steps
                res.model = load.baseModel.modelFileName
                res.clipSkip = load.clipSkip
                res.cfgScale = load.cfgScale
                res.size = (load.width, load.height)
                res.sdVae = load.sdVae
                res.etaNoiseSeedDelta = load.etaNoiseSeedDelta
                for lora in load.models:
                    res.loras.append(
                        ModelParam(
                            name=lora.modelFileName,
                            weight=lora.weight,
                        ))

            except Exception as e:
                # log.warning(f"{path}", exc_info=e)
                e.add_note(pformat(full_json, width=180))
                raise e from None

        except Exception as e:
            return res

    elif "prompt" in metadata:
        prompt = json.loads(metadata["prompt"])
        for _, node in prompt.items():
            if node["class_type"] == "BNK_CLIPTextEncodeAdvanced":
                text = node["inputs"]["text"]
                if "EasyNegative" in text or "bad anatomy" in text or "negative" in text:
                    res.negative_prompt = text

                else:
                    res.prompt = text

    else:
        log.warning(f"No generation data for {path}")

    if res.prompt:
        parser = PromptParser(category_dicts=categories)
        res.parsed_prompt = parser.parse(res.prompt)
        if res.negative_prompt:
            res.parsed_negative_prompt = parser.parse(res.negative_prompt)

        for lora in res.loras:
            cats = parser.categories(lora.name)
            if cats and cats.name == "Character":
                res.tags.Character = cats.no_alias(lora.name)

        for tag in res.parsed_prompt:
            if isinstance(tag, Tag):
                if tag.category and tag.category.name == "Character":
                    res.tags.Character = TagCategory.clean_text(
                        tag.category.no_alias(tag.text))

        if not res.tags.Character:
            global WARN_IDX
            # log.warning(f"[{WARN_IDX}] {res.parsed_prompt} + {res.loras} no character")
            WARN_IDX += 1

    return res


@beartype
def get_full_params() -> List[ImageParams]:
    result: List[ImageParams] = []
    for reference_dir in [
            workspace_root.joinpath("reference_tensor_art"),
            workspace_root.joinpath("tensor_saved_high_res"),
    ]:
        log.info(f"Getting files from {reference_dir}")
        for filename in reference_dir.rglob("*.png"):
            result.append(get_image_params(filename))

    resort = []

    def sort_by_character(param: ImageParams):
        return param.tags.Character or ""

    def sort_by_mtime(param: ImageParams) -> datetime:
        return param.image_time

    for key, group in itertools.groupby(
            sorted(result, key=sort_by_character),
            sort_by_character,
    ):
        for idx, item in enumerate(sorted(group, key=sort_by_mtime)):
            resort.append(replace(item, in_group_index=idx, group_key=key))

    return resort


@beartype
def get_artist_prompt_galleries() -> List[Tuple[str, List[ImageParams]]]:
    result: List[Tuple[str, List[ImageParams]]] = []
    for reference_dir in workspace_root.glob("*_artists"):
        if reference_dir.is_dir():
            result.append(
                (reference_dir.name,
                 [get_image_params(f) for f in reference_dir.glob("*.png")]))

    return result


def get_current_source_dir() -> Path:
    return Path(__file__).resolve().parent


def generate_common_prompt_gallery():
    output_json = output_html.with_suffix(".json")
    dump_data = {}
    full_param_list = get_full_params()
    with document(title="Images and EXIF Metadata") as doc:
        doc.head.add(
            tags.link(
                rel="stylesheet",
                href=str(
                    get_current_source_dir().joinpath("exif_preview.css"))))
        doc.head.add(
            tags.script(
                src=str(get_current_source_dir().joinpath("exif_preview.js"))))
        with tags.div(id="sidebar", ):
            for idx, tag in enumerate(
                    more_itertools.unique_everseen(it.group_key
                                                   for it in full_param_list)):
                tags.a(f"#{idx} {tag}", href=f"#row-{tag}-0")

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

                for param_idx, params in enumerate(full_param_list):
                    # if not params.prompt:
                    #     continue

                    def rowname(name: str):
                        with tags.td(style="text-align:center;"):
                            with tags.b():
                                as_multiline(name.upper())

                    with tags.tr(id="row-{}-{}".format(params.group_key,
                                                       params.in_group_index)):
                        tags.td(
                            tags.img(src=str(params.ImagePath.resolve()),
                                     width="300"))

                        def add_prompt(prompt: str):
                            as_multiline(prompt)

                        with tags.td():
                            with tags.table(**borders):
                                rowname("prompt text")
                                with tags.tr():
                                    with tags.td():
                                        add_prompt(params.prompt)
                                        # if params.parsed_prompt:
                                        # add_prompt(str(params.parsed_prompt) + str(params.loras))

                                rowname("negative prompt")
                                with tags.tr():
                                    with tags.td():
                                        add_prompt(params.negative_prompt)

                                rowname("tags")
                                with tags.tr():
                                    with tags.td():
                                        as_multiline(str(params.tags))

                                rowname("model")
                                with tags.tr():
                                    with tags.td():
                                        as_multiline(params.model)

                        with tags.td(_class="prompt-raw"):
                            paste = json.dumps(
                                params.get_tensor_art_prompt(),
                                indent=2,
                            )

                            defaulted_prompt = json.dumps(
                                params.get_defaulted_tensor_art_prompt(),
                                indent=2,
                            )

                            idname = f"prompt_copy_{param_idx}"
                            if "prompts" not in dump_data:
                                dump_data["prompts"] = {}

                            dump_data["prompts"][idname] = dict(
                                full=paste,
                                defaulted_prompt=defaulted_prompt,
                            )

                            with tags.button(
                                    onclick=
                                    f"copyToClipboard(data.prompts.{idname}.full)"
                            ):
                                text("Copy prompt")

                            with tags.button(
                                    onclick=
                                    f"copyToClipboard(data.prompts.{idname}.defaulted_prompt)"
                            ):
                                text("Defaulted prompt")

                            as_multiline(params.generation_data)

                        with tags.td():
                            with tags.table(
                                    border=1,
                                    style="border-collapse: collapse;"):
                                for model in params.loras:
                                    with tags.tr():
                                        tags.td(model.name,
                                                style="word-break: break-all;")
                                        tags.td(model.weight)

                    with tags.tr():
                        with tags.td(style="text-align:center;"):
                            text("{}x{} {} on {}".format(
                                *params.size,
                                params.ImagePath.name,
                                params.image_time,
                            ))

    json_literal = tags.script(type="application/json", id="data")
    json_str = json.dumps(dump_data, indent=2)
    json_literal.add_raw_string(json_str)
    doc.head.add(json_literal)

    with open(output_html, "w") as f:
        f.write(str(doc))

    log.info(f"HTML file created at {output_html}")


@beartype
def extract_artist(prompt: str) -> str:
    match = re.search(r"by ([^,\n]+)", prompt)
    if match:
        return match.group(1).strip()
    return "Unknown Artist"


def generate_artist_galleries():
    artists: List[str] = []
    for dir, images in get_artist_prompt_galleries():
        gallery_dir = workspace_root.joinpath(dir)
        log.info(f"{gallery_dir}")
        columns = 8

        with document(title="Images and EXIF Metadata") as doc:
            tbl = tags.table()
            row = None
            for index, image in enumerate(images):
                if index % columns == 0:
                    row = tbl.add(tags.tr())
                artist_name = extract_artist(image.prompt)
                with row.add(tags.td()) as cell:
                    img_path = str(image.ImagePath) if image.ImagePath else "#"
                    image_element = tags.img(src=img_path)
                    cell.add(image_element)
                    cell.add(tags.p(artist_name))
                    artists.append(artist_name)

        doc.head.add(
            tags.style("""
            table {
                width: 100%;
                table-layout: fixed;
                border-collapse: collapse;
            }
            td {
                padding: 10px;
                text-align: center;
                vertical-align: top;
            }
            img {
                max-width: 100%;
                height: auto;
            }
            p {
                margin: 0;
                font-size: 14px;
                text-align: center;
            }
        """))

        gallery_dir.joinpath("index.html").write_text(str(doc))
        gallery_dir.joinpath("arists.txt").write_text("\n".join(
            sorted(artists)))


def main_impl():
    generate_artist_galleries()
    generate_common_prompt_gallery()


if __name__ == "__main__":
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
