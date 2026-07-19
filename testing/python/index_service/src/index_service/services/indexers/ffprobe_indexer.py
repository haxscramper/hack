import json
from datetime import datetime
from pathlib import Path

from sqlalchemy import Engine

import magic
from beartype import beartype
from beartype.typing import Any, cast
from index_service.services.core.job_types import BaseIndexer, BaseIndexerConfig, RunContext
from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.core.types import IndexerOutput, IndexerRequest, IndexDocument
from plumbum import local
from pydantic import BaseModel
import logging

log = logging.getLogger(__name__)


@beartype
class FFProbeInfoModel(BaseModel):
    duration_seconds: float | None = None
    bitrate_bps: int | None = None
    width: int | None = None
    height: int | None = None
    resolution: str | None = None
    codec_name: str | None = None
    format_name: str | None = None
    fps: float | None = None


@beartype
def run_ffprobe(path: Path) -> FFProbeInfoModel | None:
    log.debug(f"running probe on {path}")
    ffprobe = local["ffprobe"]
    result = ffprobe.run([
        "-v",
        "error",
        "-print_format",
        "json",
        "-show_format",
        "-show_streams",
        str(path),
    ])
    payload = json.loads(result[1])
    streams_raw = payload.get("streams", [])
    format_raw = payload.get("format", {})
    video_stream: Any = None
    for stream in streams_raw:
        if stream.get("codec_type") == "video":
            video_stream = stream
            break

    def parse_int(value: Any) -> int | None:
        if value is None:
            return None
        if value == "N/A":
            return None
        return int(value)

    def parse_float(value: Any) -> float | None:
        if value is None:
            return None
        if value == "N/A":
            return None
        return float(value)

    def parse_fps(value: Any) -> float | None:
        if value is None:
            return None
        if value == "N/A":
            return None
        text = str(value)
        if "/" in text:
            num, den = text.split("/", 1)
            denominator = float(den)
            if denominator == 0:
                return None
            return float(num) / denominator
        return float(text)

    width = parse_int(video_stream.get("width")) if video_stream is not None else None
    height = parse_int(video_stream.get("height")) if video_stream is not None else None
    resolution = (f"{width}x{height}"
                  if width is not None and height is not None else None)
    fps = (parse_fps(video_stream.get("avg_frame_rate"))
           if video_stream is not None else None)

    return FFProbeInfoModel(
        duration_seconds=parse_float(format_raw.get("duration")),
        bitrate_bps=parse_int(format_raw.get("bit_rate")),
        width=width,
        height=height,
        resolution=resolution,
        codec_name=video_stream.get("codec_name") if video_stream is not None else None,
        format_name=format_raw.get("format_name"),
        fps=fps,
    )


class FFProbeIndexerResult(IndexDocument, extra="forbid"):
    probe: FFProbeInfoModel | None


class FFProbeIndexer(BaseIndexer):
    asset_name = "ffprobe"
    result_model = FFProbeIndexerResult

    def __init__(self, config: BaseIndexerConfig, database: Engine) -> None:
        self._magic = magic.Magic(mime=True)
        super().__init__(config=config, database=database)

    def can_run(self, path: Path) -> bool:
        mime = self._magic.from_file(path.absolute())
        return mime.startswith("video/")

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FFProbeIndexerResult(
                probe=run_ffprobe(ctx.get_path(request.file_ref)),
                hash=request.get_hash_str(),
            ),
        )
