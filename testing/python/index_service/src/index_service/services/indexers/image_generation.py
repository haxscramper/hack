from datetime import datetime
import json
from pathlib import Path

from beartype.typing import cast

from index_service.services.indexers.comfy_input_indexer import ComfyInputIndexer, ComfyInputIndexerResult
from index_service.services.indexers.exif_metadata import ExifMetadataIndexer, ExifMetadataIndexerResult
from index_service.services.job_types import BaseIndexer, RunContext, cache_indexer_run
from index_service.services.pydantic_utils import to_json_safe
from index_service.services.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel
import logging

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

    return sampler_map[key], scheduler


class GenerationParamsIndexer(BaseIndexer):
    asset_name = "generation_params"
    result_model = GenerationParamsIndexerResult
    required_assets = (
        ExifMetadataIndexer.asset_name,
        ComfyInputIndexer.asset_name,
    )

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
        Path("/tmp/debug.json").write_text(
            json.dumps(to_json_safe(assets), indent=2))

        comfy = getattr(assets[ComfyInputIndexer.asset_name], "result")
        assert isinstance(comfy, ComfyInputIndexerResult), type(comfy)
        exif = getattr(assets[ExifMetadataIndexer.asset_name], "result")
        assert isinstance(exif, ExifMetadataIndexerResult), type(exif)

        if comfy.note == "no workflow data detected":
            sampler, scheduler = _normalize_sampler_name(exif.file.sampler)
            result = GenerationParamsIndexerResult(
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
                    LoraParams(model=l.name, weight=l.weight)
                    for l in exif.file.loras
                ])

        else:
            result = GenerationParamsIndexerResult()

        return IndexerOutput(indexer_id=self.asset_name, result=result)
