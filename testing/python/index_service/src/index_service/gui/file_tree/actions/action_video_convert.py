import hashlib
import json
import logging
import subprocess
from dataclasses import dataclass
from fractions import Fraction
from pathlib import Path

from beartype import beartype

from index_service.gui.file_tree.actions.action_db import OperationRow
from index_service.gui.file_tree.actions.action_handler import ActionHandler
from index_service.gui.file_tree.actions.action_list_model import BaseAction, VideoConvertAction
from index_service.gui.file_tree.columns.video_convert_column import VideoConvertTarget

log = logging.getLogger(__name__)


@beartype
@dataclass
class _ProbeResultType:
    bitrate: int
    width: int
    height: int
    fps: float


class VideoConvertActionHandler(ActionHandler):

    @beartype
    def __init__(self,
                 output_directory: Path,
                 dry_run: bool,
                 vaapi_device: Path = Path("/dev/dri/renderD128")) -> None:
        self.output_directory = output_directory.absolute()
        self.dry_run = dry_run
        self.vaapi_device = vaapi_device

    @beartype
    def _dest_path(self, action: VideoConvertAction) -> Path:
        dest = (self.output_directory / action.file.root /
                action.file.root_relative).absolute()
        return dest.with_suffix(".mkv")

    @beartype
    def do_action(self, row: OperationRow, action: BaseAction) -> None:
        assert isinstance(action, VideoConvertAction)
        src = Path(action.file.path).absolute()
        dest = self._dest_path(action)

        if self.dry_run:
            log.info(
                f"do video_convert (dry-run): planned conversion {src} -> {dest} target={action.target.target}"
            )
            return

        dest.parent.mkdir(parents=True, exist_ok=True)

        probe = self._probe_video(src)
        target_width, target_height, target_fps = self._resolve_target(
            probe, action.target.target)
        target_bitrate = self._infer_target_bitrate(probe, target_width, target_height,
                                                    target_fps)

        vf_parts: list[str] = []
        if target_width != probe.width or target_height != probe.height:
            vf_parts.append(f"scale_vaapi=w={target_width}:h={target_height}:format=nv12")
        else:
            vf_parts.append("scale_vaapi=format=nv12")

        command = [
            "ffmpeg",
            "-y",
            "-hwaccel",
            "vaapi",
            "-hwaccel_output_format",
            "vaapi",
            "-vaapi_device",
            str(self.vaapi_device),
            "-i",
            str(src),
            "-map",
            "0",
            "-vf",
            ",".join(vf_parts),
        ]

        command += [
            "-r",
            f"{target_fps:.6f}",
            "-c:v",
            "h264_vaapi",
            "-b:v",
            str(target_bitrate),
            "-maxrate",
            str(target_bitrate),
            "-bufsize",
            str(target_bitrate * 2),
            "-c:a",
            "copy",
            "-c:s",
            "copy",
            str(dest),
        ]

        log.info(f"do video_convert: executing {' '.join(command)}")
        subprocess.run(command, check=True)

    @beartype
    def undo_action(self, row: OperationRow, action: BaseAction) -> None:
        assert isinstance(action, VideoConvertAction)
        dest = self._dest_path(action)

        if self.dry_run:
            log.info(f"undo video_convert (dry-run): planned remove {dest}")
            return

        log.info(f"undo video_convert: removing {dest}")
        if dest.exists():
            if dest.is_dir():
                raise ValueError(
                    f"Undo path points to a directory, expected file: {dest}")
            dest.unlink()

    @beartype
    def get_hash(self, action: BaseAction) -> str:
        assert isinstance(action, VideoConvertAction)
        src = action.file.path
        target = action.target.target
        payload = f"video_convert|{src}|{self.output_directory}|{target}"
        return hashlib.sha256(payload.encode("utf-8")).hexdigest()

    @beartype
    def verify_consistency_single(self, action: BaseAction) -> None:
        assert isinstance(action, VideoConvertAction)
        src = Path(action.file.path).absolute()
        dest = self._dest_path(action)

        if src == self.output_directory:
            raise ValueError(f"Video convert source cannot be output directory: {src}")

        if src == dest:
            raise ValueError(f"Video convert destination resolves to source path: {src}")

        if self.vaapi_device.exists() is False:
            raise ValueError(f"VAAPI device does not exist: {self.vaapi_device}")

    @beartype
    def _probe_video(self, src: Path) -> _ProbeResultType:
        command = [
            "ffprobe",
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-show_entries",
            "stream=bit_rate,width,height,avg_frame_rate",
            "-show_entries",
            "format=bit_rate",
            "-of",
            "json",
            str(src),
        ]
        completed = subprocess.run(command, check=True, capture_output=True, text=True)
        payload = json.loads(completed.stdout)

        streams = payload.get("streams", [])
        if len(streams) < 1:
            raise ValueError(f"Input file has no video streams: {src}")

        stream = streams[0]
        width = int(stream["width"])
        height = int(stream["height"])

        avg_frame_rate = str(stream["avg_frame_rate"])
        fps = float(Fraction(avg_frame_rate))

        stream_bitrate_raw = stream.get("bit_rate")
        format_bitrate_raw = payload.get("format", {}).get("bit_rate")

        if stream_bitrate_raw is None and format_bitrate_raw is None:
            raise ValueError(
                f"Unable to infer source bitrate from ffprobe output for file: {src}")

        stream_bitrate = int(stream_bitrate_raw) if stream_bitrate_raw is not None else 0
        format_bitrate = int(format_bitrate_raw) if format_bitrate_raw is not None else 0
        bitrate = stream_bitrate if 0 < stream_bitrate else format_bitrate

        if bitrate < 1:
            raise ValueError(f"Source bitrate is invalid for file {src}: {bitrate}")

        return _ProbeResultType(
            bitrate=bitrate,
            width=width,
            height=height,
            fps=fps,
        )

    @beartype
    def _resolve_target(self, probe: _ProbeResultType,
                        target: VideoConvertTarget) -> tuple[int, int, float]:
        match target:
            case VideoConvertTarget.NO_CHANGE:
                return probe.width, probe.height, probe.fps
            case VideoConvertTarget.SD_30:
                box_width, box_height, target_fps = 854, 480, 30.0
            case VideoConvertTarget.HD_30:
                box_width, box_height, target_fps = 1280, 720, 30.0
            case VideoConvertTarget.FULL_HD_30:
                box_width, box_height, target_fps = 1920, 1080, 30.0
            case _:
                raise ValueError(f"Unsupported video convert target: {target}")

        # Fit the source into the target bounding box while preserving aspect
        # ratio, and never upscale beyond the source resolution.
        scale = min(box_width / probe.width, box_height / probe.height, 1.0)
        target_width = self._round_even(probe.width * scale)
        target_height = self._round_even(probe.height * scale)
        target_fps = min(target_fps, probe.fps)
        return target_width, target_height, target_fps

    @beartype
    def _round_even(self, value: float) -> int:
        rounded = int(round(value))
        if rounded % 2 != 0:
            rounded += 1
        return max(rounded, 2)

    @beartype
    def _infer_max_bitrate(self, target_width: int, target_height: int,
                           target_fps: float) -> int:
        # Bits-per-pixel-per-frame heuristic tuned to commonly expected H.264
        # bitrates (roughly 2.5 Mbps @480p30, 5 Mbps @720p30, 8 Mbps @1080p30).
        bits_per_pixel = 0.13
        target_pixels = target_width * target_height
        return int(target_pixels * target_fps * bits_per_pixel)

    @beartype
    def _infer_target_bitrate(
        self,
        probe: _ProbeResultType,
        target_width: int,
        target_height: int,
        target_fps: float,
    ) -> int:
        input_pixels = probe.width * probe.height
        target_pixels = target_width * target_height
        ratio = (target_pixels * target_fps) / (input_pixels * probe.fps)
        bitrate = int(probe.bitrate * ratio)
        max_bitrate = self._infer_max_bitrate(target_width, target_height, target_fps)
        return max(min(bitrate, max_bitrate), 200000)
