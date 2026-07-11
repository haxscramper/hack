from collections import defaultdict, deque
import json
import logging
from datetime import datetime
from pathlib import Path

from beartype.typing import Any, cast
from index_service.services.indexers.comfy_input_indexer import (
    ComfyInput,
    ComfyInputIndexer,
    ComfyInputIndexerResult,
)
from index_service.services.indexers.exif_metadata import (
    ExifMetadataIndexer,
    ExifMetadataIndexerResult,
)
from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.pydantic_utils import to_json_safe
from index_service.services.core.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel

log = logging.getLogger(__name__)


class LoraParams(BaseModel, extra="forbid"):
    model: str
    weight: float


class GenerationParamsIndexerResult(BaseModel, extra="forbid"):
    positive: str
    negative: str
    width: int
    height: int
    sampler: str
    scheduler: str
    steps: int
    cfg: float
    checkpoint: str
    loras: list[LoraParams]
    vae: str | None = None
    clip: str | None = None


def _normalize_sampler_name(name: str) -> tuple[str, str | None]:
    sampler_map = {
        "euler": "euler",
        "eulera": "euler_ancestral",
        "euleracfg++": "euler_ancestral_cfg_pp",
        "eulercfg++": "euler_cfg_pp",
        "heun": "heun",
        "heunpp2": "heunpp2",
        "dpm2": "dpm_2",
        "dpm2a": "dpm_2_ancestral",
        "lms": "lms",
        "dpmfast": "dpm_fast",
        "dpmadaptive": "dpm_adaptive",
        "dpm++2sa": "dpmpp_2s_ancestral",
        "dpm++sde": "dpmpp_sde",
        "dpm++2m": "dpmpp_2m",
        "dpm++2msde": "dpmpp_2m_sde",
        "dpm++3msde": "dpmpp_3m_sde",
        "ddpm": "ddpm",
        "lcm": "lcm",
        "ddim": "ddim",
        "unipc": "uni_pc",
        "unipcbh2": "uni_pc_bh2",
        "ipndm": "ipndm",
        "deis": "deis",
    }

    scheduler_map = {
        "normal": "normal",
        "karras": "karras",
        "exponential": "exponential",
        "sgmuniform": "sgm_uniform",
        "simple": "simple",
        "ddimuniform": "ddim_uniform",
        "beta": "beta",
        "linearquadratic": "linear_quadratic",
        "kloptimal": "kl_optimal",
    }

    key = name.lower().replace(" ", "")

    scheduler: str | None = None
    for suffix, value in scheduler_map.items():
        if key.endswith(suffix) and key != suffix:
            scheduler = value
            key = key[:-len(suffix)]
            break

    if key in sampler_map:
        return sampler_map[key], scheduler

    else:
        return key, "normal"


def _extract_generation_params(nodes: list[ComfyInput]) -> GenerationParamsIndexerResult:
    by_id: dict[str, ComfyInput] = {n.node_id: n for n in nodes}

    # Build dependency graph. A link's origin_id is the dependee, so the
    # node owning the link depends on origin_id.
    dependents: dict[str, list[str]] = defaultdict(list)
    in_degree: dict[str, int] = {n.node_id: 0 for n in nodes}
    for node in nodes:
        deps = {link.origin_id for link in node.links}
        in_degree[node.node_id] = len(deps)
        for dep in deps:
            dependents[dep].append(node.node_id)

    # Kahn topological traversal starting from nodes with no dependencies.
    queue: deque[str] = deque(node_id for node_id, deg in in_degree.items() if deg == 0)
    order: list[str] = []
    while queue:
        node_id = queue.popleft()
        order.append(node_id)
        for nxt in dependents[node_id]:
            in_degree[nxt] -= 1
            if in_degree[nxt] == 0:
                queue.append(nxt)

    # Accumulators with sane defaults.
    positive = ""
    negative = ""
    width = 0
    height = 0
    sampler = ""
    scheduler = ""
    steps = 0
    cfg = 0.0
    checkpoint = ""
    loras: list[LoraParams] = []
    vae: str | None = None
    clip: str | None = None

    def get(node: ComfyInput, key: str, index: int) -> Any:
        """Read a value from named (dict) or positional (list) inputs."""
        if isinstance(node.inputs, dict):
            return node.inputs.get(key)
        if 0 <= index < len(node.inputs):
            return node.inputs[index]
        return None

    for node_id in order:
        node = by_id[node_id]
        match (node.node, node.title):
            case ("CLIPTextEncode", "CLIP Text Encode (Positive Prompt)"):
                positive = get(node, "text", 0) or positive

            case ("CLIPTextEncode", "CLIP Text Encode (Negative Prompt)"):
                negative = get(node, "text", 0) or negative

            case ("CLIPTextEncode", _):
                # Fallback when the title is missing: first encode -> positive.
                if not positive:
                    positive = get(node, "text", 0) or positive
                elif not negative:
                    negative = get(node, "text", 0) or negative

            case ("EmptyLatentImage", _):
                width = get(node, "width", 0) or width
                height = get(node, "height", 1) or height

            case ("KSampler", _):
                steps = get(node, "steps", 2) or steps
                cfg = get(node, "cfg", 3) or cfg
                sampler = get(node, "sampler_name", 4) or sampler
                scheduler = get(node, "scheduler", 5) or scheduler

            case ("CheckpointLoaderSimple", _) | ("CheckpointLoader", _):
                checkpoint = get(node, "ckpt_name", 0) or checkpoint

            case ("LoraLoader", _) | ("LoraLoaderModelOnly", _):
                loras.append(
                    LoraParams(
                        model=get(node, "lora_name", 0),
                        weight=get(node, "strength_model", 1),
                    ))

            case ("Power Lora Loader (rgthree)", _):
                for input in node.inputs:
                    if isinstance(input, dict) and "lora" in input and ("on" not in input
                                                                        or input["on"]):
                        loras.append(
                            LoraParams(
                                model=cast(str, input["lora"]),
                                weight=cast(float, input["strength"]),
                            ))

            case ("VAELoader", _):
                vae = get(node, "vae_name", 0) or vae

            case ("CLIPLoader", _):
                clip = get(node, "clip_name", 0) or clip

            case _:
                pass

    return GenerationParamsIndexerResult(
        positive=positive,
        negative=negative,
        width=width,
        height=height,
        sampler=sampler,
        scheduler=scheduler,
        steps=steps,
        cfg=cfg,
        checkpoint=checkpoint,
        loras=loras,
        vae=vae,
        clip=clip,
    )


class GenerationParamsIndexer(BaseIndexer):
    asset_name = "generation_params"
    result_model = GenerationParamsIndexerResult
    required_assets = (
        ExifMetadataIndexer.asset_name,
        ComfyInputIndexer.asset_name,
    )

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)

    def can_run(self, path: Path) -> bool:
        return path.suffix.lower() in [".png", ".webp", ".jpg", ".jpeg"]

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:

        comfy = getattr(assets[ComfyInputIndexer.asset_name], "result")
        assert isinstance(comfy, ComfyInputIndexerResult), type(comfy)
        exif = getattr(assets[ExifMetadataIndexer.asset_name], "result")
        assert isinstance(exif, ExifMetadataIndexerResult), type(exif)

        def run():
            if comfy.note == "no workflow data detected":
                # TODO: Extract original seed information.
                # TODO: Do not fill generation parameters for files
                # that have no generation data originally
                sampler, scheduler = _normalize_sampler_name(exif.file.sampler)
                return GenerationParamsIndexerResult(
                    positive=exif.file.prompt,
                    negative=exif.file.negative_prompt,
                    width=exif.file.size[0],
                    height=exif.file.size[1],
                    checkpoint=exif.file.model,
                    steps=exif.file.steps,
                    sampler=sampler,
                    cfg=exif.file.cfgScale,
                    scheduler=scheduler or "normal",
                    loras=[
                        LoraParams(model=l.name, weight=l.weight) for l in exif.file.loras
                    ],
                )

            else:
                return _extract_generation_params(comfy.inputs)

        try:
            result = run()

        except Exception as e:
            Path("/tmp/debug.json").write_text(json.dumps(to_json_safe(comfy), indent=2))

            raise e from None

        return IndexerOutput(indexer_id=self.asset_name, result=result)
