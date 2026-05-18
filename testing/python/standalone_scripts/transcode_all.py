#!/usr/bin/env python
# /// script
# dependencies = ["pandas", "odfpy", "plumbum"]
# ///
from __future__ import annotations

import argparse
import logging
import shutil
from pathlib import Path
from typing import Optional

import pandas as pd
from plumbum import local
from plumbum.commands.processes import ProcessExecutionError

ACTION_TO_HEIGHT = {
    "Compress to SD": 480,
    "Compress to HD": 720,
    "Compress to Full HD": 1080,
}

VIDEO_EXTENSIONS = {
    ".mp4",
    ".mkv",
    ".avi",
    ".mov",
    ".webm",
    ".m4v",
    ".wmv",
    ".flv",
    ".mpg",
    ".mpeg",
    ".ts",
    ".mts",
    ".m2ts",
    ".3gp",
}


def setup_logging(output_root: Path) -> logging.Logger:
    output_root.mkdir(parents=True, exist_ok=True)

    logger = logging.getLogger("video_recode")
    logger.setLevel(logging.INFO)
    logger.handlers.clear()

    formatter = logging.Formatter("%(asctime)s [%(levelname)s] %(message)s")

    stream = logging.StreamHandler()
    stream.setFormatter(formatter)
    logger.addHandler(stream)

    file_handler = logging.FileHandler(
        output_root.joinpath("file-convert-failure.log"), encoding="utf-8")
    file_handler.setLevel(logging.ERROR)
    file_handler.setFormatter(formatter)
    logger.addHandler(file_handler)

    return logger


def normalize_rel_path(value: str, input_root: Path) -> Path:
    path = Path(str(value).strip())
    try:
        return path.resolve().relative_to(input_root.resolve())
    except ValueError:
        return Path(path.as_posix().lstrip("/"))


def is_video_path(path: Path) -> bool:
    return path.suffix.lower() in VIDEO_EXTENSIONS


def ensure_parent(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)


def copy_file(src: Path, dst: Path) -> None:
    ensure_parent(dst)
    shutil.copy2(src, dst)


def safe_str(value) -> str:
    if pd.isna(value):
        return ""
    return str(value).strip()


def build_scale_filter(target_height: int) -> str:
    return f"scale=-2:{target_height}:force_original_aspect_ratio=decrease"


def convert_video(
    src: Path,
    dst: Path,
    action: str,
    codec_name: str,
    logger: logging.Logger,
) -> None:
    dst = dst.with_suffix(".mp4")
    ensure_parent(dst)

    ffmpeg = local["ffmpeg"]

    target_height = ACTION_TO_HEIGHT.get(action)
    needs_reencode = codec_name.lower() not in {"hevc", "h265"}
    source_size = src.stat().st_size

    cmd = [
        "-y",
        "-hwaccel",
        "vaapi",
        "-hwaccel_device",
        "/dev/dri/renderD128",
        "-hwaccel_output_format",
        "vaapi",
        "-i",
        str(src),
        "-map",
        "0",
        "-fs",
        str(source_size),
    ]

    video_args = []
    if target_height is not None:
        video_args.extend([
            "-vaapi_device",
            "/dev/dri/renderD128",
            "-vf",
            build_scale_filter(target_height),
            "-c:v",
            "hevc_vaapi",
            "-rc_mode",
            "CQP",
            "-qp",
            "28",
        ])
    elif needs_reencode:
        video_args.extend([
            "-vaapi_device",
            "/dev/dri/renderD128",
            "-c:v",
            "hevc_vaapi",
            "-rc_mode",
            "CQP",
            "-qp",
            "28",
        ])
    else:
        video_args.extend(["-c:v", "copy"])

    cmd.extend(video_args)
    cmd.extend([
        "-c:a",
        "copy",
        "-c:s",
        "copy",
        str(dst),
    ])

    ffmpeg[cmd]()

    output_size = dst.stat().st_size
    saved_bytes = source_size - output_size
    saved_percent = (saved_bytes / source_size) * 100 if source_size else 0.0

    logger.info(
        "Converted video: %s -> %s, size %d -> %d bytes (saved %d bytes, %.1f%%)",
        src,
        dst,
        source_size,
        output_size,
        saved_bytes,
        saved_percent,
    )


def process_video_file(
    src: Path,
    dst: Path,
    row: pd.Series,
    logger: logging.Logger,
) -> None:
    if dst.exists():
        logger.info("Skipping existing output: %s", dst)
        return

    action = safe_str(row.get("action"))
    codec_name = safe_str(row.get("codec_name"))

    try:
        if action == "none" and codec_name.lower() in {"hevc", "h265"}:
            logger.info("Copying unchanged H265 video: %s -> %s", src, dst)
            copy_file(src, dst)
            return

        convert_video(
            src=src,
            dst=dst,
            action=action,
            codec_name=codec_name,
            logger=logger,
        )
    except (ProcessExecutionError, Exception) as ex:
        logger.error("Failed processing video %s: %s", src, ex)
        if not dst.exists():
            copy_file(src, dst)


def load_table(ods_path: Path) -> pd.DataFrame:
    return pd.read_excel(ods_path, engine="odf")


def build_video_action_map(df: pd.DataFrame,
                           input_root: Path) -> dict[Path, pd.Series]:
    result: dict[Path, pd.Series] = {}
    for _, row in df.iterrows():
        rel = normalize_rel_path(row["absolute_path"], input_root)
        result[rel] = row
    return result


def iter_input_files(root: Path) -> list[Path]:
    return [path for path in root.rglob("*") if path.is_file()]


def process_tree(
    input_root: Path,
    output_root: Path,
    video_rows: dict[Path, pd.Series],
    logger: logging.Logger,
) -> None:
    files = iter_input_files(input_root)
    total = len(files)

    for index, src in enumerate(files, start=1):
        rel = src.relative_to(input_root)
        dst = output_root / rel
        if is_video_path(src):
            dst = dst.with_suffix(".mp4")

        progress = f"[{index}/{total}]"

        if dst.exists():
            logger.info("%s Skipping existing output: %s", progress, dst)
            continue

        row = video_rows.get(rel)

        if row is not None and is_video_path(src):
            logger.info("%s Processing video file: %s", progress, src)
            process_video_file(src=src, dst=dst, row=row, logger=logger)
        else:
            logger.info("%s Copying non-video file: %s -> %s", progress, src,
                        dst)
            copy_file(src, dst)


def process_orphan_table_entries(
    input_root: Path,
    output_root: Path,
    video_rows: dict[Path, pd.Series],
    logger: logging.Logger,
) -> None:
    orphan_items = []
    for rel, row in video_rows.items():
        src = input_root / rel
        dst = output_root / rel

        if dst.exists():
            continue

        orphan_items.append((rel, row, src, dst))

    total = len(orphan_items)
    for index, (_, row, src, dst) in enumerate(orphan_items, start=1):
        progress = f"[orphan {index}/{total}]"

        if not src.exists():
            logger.error("%s Table entry missing on disk: %s", progress, src)
            continue

        logger.info("%s Processing table-only video entry: %s", progress, src)
        process_video_file(src=src, dst=dst, row=row, logger=logger)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("ods_file", type=Path)
    parser.add_argument("--input-root", type=Path, required=True)
    parser.add_argument("--output-root", type=Path, required=True)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    input_root = args.input_root.resolve()
    output_root = args.output_root.resolve()

    logger = setup_logging(output_root)

    logger.info("Loading ODS table: %s", args.ods_file)
    df = load_table(args.ods_file)
    video_rows = build_video_action_map(df, input_root)

    logger.info("Processing input tree: %s", input_root)
    process_tree(
        input_root=input_root,
        output_root=output_root,
        video_rows=video_rows,
        logger=logger,
    )

    logger.info("Processing table entries not reached via tree walk")
    process_orphan_table_entries(
        input_root=input_root,
        output_root=output_root,
        video_rows=video_rows,
        logger=logger,
    )

    logger.info("Done")


if __name__ == "__main__":
    main()
