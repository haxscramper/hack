import base64
import hashlib
import json
import logging
import math
import re
from datetime import datetime
from pathlib import Path
from pprint import pformat

from beartype import beartype
from beartype.typing import Any, Dict, List, Optional, Set, Tuple, Union

from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.pydantic_utils import try_parse_json
from index_service.services.core.types import IndexerOutput, IndexerRequest, IndexDocument
from PIL import Image, ExifTags
from pydantic import BaseModel, ConfigDict, Field

log = logging.getLogger(__name__)

RX_DELETE = re.compile(r"[- _,()/\\:]")


@beartype
class LoRATag(BaseModel, extra="forbid"):
    id2: str
    id3: str | None
    weight: str | float


def try_float(val: str) -> float | str:
    try:
        return float(val)

    except ValueError:
        return val


def _to_jsonable_metadata(value: Any) -> Any:
    if value is None or isinstance(value, (str, int, float, bool)):
        return value

    if isinstance(value, Path):
        return str(value)

    if isinstance(value, datetime):
        return value.isoformat()

    if isinstance(value, (bytes, bytearray)):
        raw = bytes(value)
        return {
            "__type__": "bytes",
            "length": len(raw),
            "sha256": hashlib.sha256(raw).hexdigest(),
            "base64": base64.b64encode(raw).decode("ascii"),
        }

    if isinstance(value, dict):
        return {str(k): _to_jsonable_metadata(v) for k, v in value.items()}

    if isinstance(value, (list, tuple, set)):
        return [_to_jsonable_metadata(v) for v in value]

    # PIL rationals and similar numeric-like values
    if hasattr(value, "numerator") and hasattr(value, "denominator"):
        try:
            return {
                "__type__": type(value).__name__,
                "numerator": int(value.numerator),
                "denominator": int(value.denominator),
                "float": float(value),
            }
        except Exception:
            return repr(value)

    try:
        json.dumps(value)
        return value
    except Exception:
        return repr(value)


def _decode_gps_info(gps_info: Any) -> Any:
    if not isinstance(gps_info, dict):
        return _to_jsonable_metadata(gps_info)

    decoded: Dict[str, Any] = {}
    for gps_key, gps_value in gps_info.items():
        gps_name = ExifTags.GPSTAGS.get(gps_key, f"GPS_{gps_key}")
        decoded[gps_name] = _to_jsonable_metadata(gps_value)
    return decoded


def _collect_all_embedded_metadata(img: Image.Image) -> Dict[str, Any]:
    info_raw = {k: _to_jsonable_metadata(v) for k, v in img.info.items()}
    info_parsed: Dict[str, Any] = {}

    for k, v in img.info.items():
        if isinstance(v, str):
            info_parsed[k] = _to_jsonable_metadata(try_parse_json(v))
        else:
            info_parsed[k] = _to_jsonable_metadata(v)

    exif_obj = img.getexif()
    exif_by_id: Dict[str, Any] = {}
    exif_by_name: Dict[str, Any] = {}
    gps_info_by_name: Dict[str, Any] = {}

    for tag_id, raw_value in exif_obj.items():
        tag_name = ExifTags.TAGS.get(tag_id, f"Tag_{tag_id}")
        value_json = _to_jsonable_metadata(raw_value)

        exif_by_id[str(tag_id)] = value_json
        exif_by_name[tag_name] = value_json

        if tag_name == "GPSInfo":
            gps_info_by_name = _decode_gps_info(raw_value)

    if hasattr(exif_obj, "get_ifd"):
        for ifd_name, ifd_id in {
                "ExifIFD": 0x8769,
                "GPSInfoIFD": 0x8825,
                "InteropIFD": 0xA005,
        }.items():
            try:
                ifd_data = exif_obj.get_ifd(ifd_id)
                exif_by_name[ifd_name] = _to_jsonable_metadata(ifd_data)
                if ifd_name == "GPSInfoIFD" and not gps_info_by_name:
                    gps_info_by_name = _decode_gps_info(ifd_data)
            except Exception:
                pass

    return {
        "image": {
            "format": img.format,
            "format_description": img.format_description,
            "mode": img.mode,
            "width": img.width,
            "height": img.height,
            "size": [img.width, img.height],
            "is_animated": bool(getattr(img, "is_animated", False)),
            "n_frames": int(getattr(img, "n_frames", 1)),
        },
        "info_raw": info_raw,
        "info_parsed": info_parsed,
        "exif": {
            "by_tag_id": exif_by_id,
            "by_tag_name": exif_by_name,
            "gps_by_name": gps_info_by_name,
        },
    }


@beartype
class Tag(BaseModel, extra="forbid"):
    text: str
    amplifier: float = 1.0
    parens: int = 0

    def __repr__(self) -> str:
        if self.amplifier == 1.0:
            result = self.text

        else:
            result = f"{self.text}:{self.amplifier}"

        return "<{}{}{}{}>".format(
            "(" * self.parens,
            result,
            ")" * self.parens,
            ":",
        )


ParsedTag = Union[Tag, LoRATag, str]


class ModelParam(BaseModel, extra="forbid"):
    name: str = ""
    weight: float = 1.0


class ImageTags(BaseModel, extra="forbid"):
    pass


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
        return ",".join(it.text for it in self.parsed_prompt if isinstance(it, Tag))

    def get_tag_formatted_negative_prompt(self) -> str:
        assert self.parsed_negative_prompt
        return ",".join(
            it.text for it in self.parsed_negative_prompt if isinstance(it, Tag))

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


WARN_IDX = 0


@beartype
class PromptParser:

    def __init__(self):
        self.break_token = "BREAK"

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
            result.append(Tag(
                text=text.strip(),
                amplifier=amplifier,
                parens=level,
            ))

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
                    result.append(
                        LoRATag(
                            id2=parts[0],
                            id3=parts[1],
                            weight=try_float(parts[2]),
                        ))

                elif len(parts) == 2:
                    result.append(
                        LoRATag(
                            id2=parts[0],
                            id3=None,
                            weight=try_float(parts[1]),
                        ))

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
def get_image_params(path: Path, reference_dir: Path, fast: bool = False) -> ImageParams:
    json_path = path.with_suffix(".json")

    res = ImageParams(
        original_path=path,
        reference_dir=reference_dir,
        image_time=datetime.fromtimestamp(path.stat().st_mtime),
    )

    if not json_path.exists() and fast:
        return res

    sidecar_payload: Dict[str, Any] = {}
    metadata: Dict[str, Any] = {}
    json_metadata = False

    with Image.open(path) as img:
        if json_path.exists():
            sidecar_payload = json.loads(json_path.read_text())
            metadata = sidecar_payload.get("exif_metadata", {})
            json_metadata = True
        else:
            metadata = dict(img.info)
            json_metadata = False

        res.ImagePath = path
        res.original_metadata_full = {
            "embedded": _collect_all_embedded_metadata(img),
            "sidecar": _to_jsonable_metadata(sidecar_payload) if sidecar_payload else {},
        }

        if json_metadata:
            res.original_metadata_full["exif_metadata"] = _to_jsonable_metadata(metadata)
        else:
            res.original_metadata_full["exif_metadata"] = res.original_metadata_full[
                "embedded"]["info_parsed"]

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

                        res.loras.append(ModelParam(
                            name=name,
                            weight=float(weight),
                        ))

    elif "generation_data" in metadata:
        if json_metadata:
            full_json = metadata["generation_data"]
        else:
            generation_raw = metadata["generation_data"]
            if isinstance(generation_raw, (bytes, bytearray)):
                generation_raw = generation_raw.decode("utf-8", errors="replace")
            generation = str(generation_raw).strip("\x00")
            try:
                full_json = json.loads(generation)
            except Exception:
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
                res.loras.append(ModelParam(
                    name=lora.modelFileName,
                    weight=lora.weight,
                ))

        except Exception as e:
            e.add_note(pformat(full_json, width=180))
            return res

    elif "prompt" in metadata:
        if json_metadata:
            prompt = metadata["prompt"]
        else:
            prompt_raw = metadata["prompt"]
            if isinstance(prompt_raw, (bytes, bytearray)):
                prompt_raw = prompt_raw.decode("utf-8", errors="replace")
            prompt = json.loads(prompt_raw)

        for _, node in prompt.items():
            if node["class_type"] == "BNK_CLIPTextEncodeAdvanced":
                text = node["inputs"]["text"]
                if ("EasyNegative" in text or "bad anatomy" in text or
                        "negative" in text):
                    res.negative_prompt = text
                else:
                    res.prompt = text

    if res.prompt:
        parser = PromptParser()
        res.parsed_prompt = parser.parse(res.prompt)
        if res.negative_prompt:
            res.parsed_negative_prompt = parser.parse(res.negative_prompt)

    return res


class ExifMetadataIndexerResult(IndexDocument, extra="forbid"):
    file: ImageParams


class ExifMetadataIndexer(BaseIndexer):
    asset_name = "exif_metadata"
    result_model = ExifMetadataIndexerResult
    max_parallel = 8

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)

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

        path = ctx.get_path(request.file_ref)
        assert path.exists(), f"{path}"

        params = get_image_params(Path(path), Path(path).parent)

        log.info(f"{path} OK")

        return IndexerOutput(indexer_id=self.asset_name,
                             result=ExifMetadataIndexerResult(
                                 file=params,
                                 hash=request.get_hash_str(),
                             ))
