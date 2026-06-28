from datetime import datetime
from pathlib import Path

from beartype.typing import cast, Any
from beartype import beartype
from plumbum import local
import json

from index_service.services.job_types import BaseIndexer, RunContext, cache_indexer_run
from index_service.services.types import IndexerOutput, IndexerRequest
from pydantic import BaseModel


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

    width = parse_int(
        video_stream.get("width")) if video_stream is not None else None
    height = parse_int(
        video_stream.get("height")) if video_stream is not None else None
    resolution = f"{width}x{height}" if width is not None and height is not None else None
    fps = parse_fps(video_stream.get(
        "avg_frame_rate")) if video_stream is not None else None

    return FFProbeInfoModel(
        duration_seconds=parse_float(format_raw.get("duration")),
        bitrate_bps=parse_int(format_raw.get("bit_rate")),
        width=width,
        height=height,
        resolution=resolution,
        codec_name=video_stream.get("codec_name")
        if video_stream is not None else None,
        format_name=format_raw.get("format_name"),
        fps=fps,
    )


class FFProbeIndexerResult(BaseModel, extra="forbid"):
    probe: FFProbeInfoModel | None


class FFProbeIndexer(BaseIndexer):
    asset_name = "ffprobe"
    result_model = FFProbeIndexerResult

    def can_run(self, path: Path) -> bool:
        return path.suffix.lower() in {".webm", ".mp4"}

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
                probe=run_ffprobe(ctx.get_path(request.file_ref))),
        )
