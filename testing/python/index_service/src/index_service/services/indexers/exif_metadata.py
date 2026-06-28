import json
import math
from pathlib import Path
from pprint import pformat

from index_service.services.job_types import BaseIndexer, RunContext, cache_indexer_run
from index_service.services.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel, Field, ConfigDict
from beartype.typing import List, Tuple, Optional, Set, Dict, Union, Any
import re
from datetime import datetime
from beartype import beartype
import logging
from PIL import Image

log = logging.getLogger(__name__)

RX_DELETE = re.compile(r"[- _,()/\\:]")


@beartype
class LoRATag(BaseModel, extra="forbid"):
    id2: str
    id3: str
    weight: str


@beartype
class TagCategory(BaseModel, extra="forbid"):
    tags: Set[str]
    name: str
    aliases: Dict[str, str] = Field(default_factory=dict)

    @beartype
    @staticmethod
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
class Tag(BaseModel, extra="forbid"):
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


class ModelParam(BaseModel, extra="forbid"):
    name: str = ""
    weight: float = 1.0


class ImageTags(BaseModel, extra="forbid"):
    Character: Optional[str] = None


class ImageParams(BaseModel, extra="forbid"):
    original_path: Path
    reference_dir: Path
    prompt: str = ""
    negative_prompt: str = ""
    generation_data: str = ""
    loras: List[ModelParam] = Field(default_factory=list)
    model: str = ""
    size: Tuple[int, int] = (896, 1088)
    ImagePath: Optional[Path] = None
    tags: ImageTags = Field(default_factory=lambda: ImageTags())
    parsed_prompt: Optional[List[ParsedTag]] = None
    parsed_negative_prompt: Optional[List[ParsedTag]] = None
    group_key: str = ""
    in_group_index: int = 0
    image_time: datetime = Field(default_factory=lambda: datetime.now())
    sampler: str = "Euler a"
    steps: int = 25
    cfgScale: float = 7.0
    clipSkip: int = 2
    sdVae: str = "Automatic"
    etaNoiseSeedDelta: int = 31337

    original_metadata_full: Dict[str, Any] = Field(default_factory=dict)

    def get_tag_formatted_prompt(self) -> str:
        assert self.parsed_prompt
        return ",".join(it.text for it in self.parsed_prompt
                        if isinstance(it, Tag))

    def get_tag_formatted_negative_prompt(self) -> str:
        assert self.parsed_negative_prompt
        return ",".join(it.text for it in self.parsed_negative_prompt
                        if isinstance(it, Tag))

    def get_infinite_browser_extra(self) -> dict:
        assert self.parsed_prompt
        meta = {
            "Model":
            self.model,
            "Lora hashes":
            ",".join(f"{it.name}" for it in self.loras),
            "Steps":
            self.steps,
            "TA defaulted":
            json.dumps(self.get_defaulted_tensor_art_prompt(), indent=2),
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
            negativePrompt="",
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


def _try_parse_json(value: Any):
    if isinstance(value, bytes):
        try:
            value = value.decode("utf-8")
        except UnicodeDecodeError:
            return value

    if isinstance(value, str):
        try:
            return json.loads(value)
        except json.JSONDecodeError:
            return value

    if isinstance(value, dict):
        return {k: _try_parse_json(v) for k, v in value.items()}

    if isinstance(value, list):
        return [_try_parse_json(v) for v in value]

    return value


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
                tag, amp, *other = token.split(":")
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


@beartype
def get_image_params(path: Path,
                     reference_dir: Path,
                     fast: bool = False) -> ImageParams:
    json_path = path.with_suffix(".json")

    res = ImageParams(
        original_path=path,
        reference_dir=reference_dir,
        image_time=datetime.fromtimestamp(path.stat().st_mtime),
    )

    if not json_path.exists() and fast:
        return res

    img = Image.open(path)
    if json_path.exists():
        metadata = json.loads(json_path.read_text())["exif_metadata"]
        json_metadata = True
    else:
        metadata = img.info
        json_metadata = False

    res.ImagePath = path
    res.original_metadata_full["exif_metadata"] = {
        k: _try_parse_json(v)
        for k, v in img.info.items()
    }

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
            res.cfgScale = float(sd.CFGScale.strip(","))
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
        if json_metadata:
            full_json = metadata["generation_data"]
        else:
            generation = metadata["generation_data"].strip('\x00')
            try:
                full_json = json.loads(generation)

            except Exception as e:
                return res

        try:
            load = TArtV1MainData.model_validate(full_json)
            res.prompt = load.prompt
            res.negative_prompt = load.negativePrompt
            res.generation_data = json.dumps(full_json)
            res.sampler = load.samplerName
            res.steps = load.steps
            res.model = load.baseModel.modelFileName
            res.clipSkip = load.clipSkip
            res.cfgScale = float(load.cfgScale)
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
            return res

    elif "prompt" in metadata:
        if json_metadata:
            prompt = metadata["prompt"]

        else:
            prompt = json.loads(metadata["prompt"])

        for _, node in prompt.items():
            if node["class_type"] == "BNK_CLIPTextEncodeAdvanced":
                text = node["inputs"]["text"]
                if "EasyNegative" in text or "bad anatomy" in text or "negative" in text:
                    res.negative_prompt = text

                else:
                    res.prompt = text

    # else:
    #     log.warning(f"No generation data for {path}")

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


class ExifMetadataIndexerResult(BaseModel, extra="forbid"):
    file: ImageParams


class ExifMetadataIndexer(BaseIndexer):
    asset_name = "exif_metadata"
    result_model = ExifMetadataIndexerResult

    def can_run(self, path: Path) -> bool:
        return path.suffix in [".png", ".jpg", ".webp", ".jpeg"]

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:

        path = request.file_ref.path
        assert path.exists(), f"{path}"

        params = get_image_params(Path(path), Path(path).parent)

        log.info(f"{path} OK")

        return IndexerOutput(indexer_id=self.asset_name,
                             result=ExifMetadataIndexerResult(file=params))
