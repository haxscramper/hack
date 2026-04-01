#!/usr/bin/env python
# /// script
# requires-python = ">=3.12"
# dependencies = [
#   "Pillow",
#   "piexif",
# ]
# ///

from __future__ import annotations

import argparse
import datetime as dt
import os
import shutil
import sys
from pathlib import Path

from PIL import Image
import piexif


EXTENSION_BY_FORMAT = {
    "JPEG": ".jpg",
    "PNG": ".png",
    "GIF": ".gif",
    "BMP": ".bmp",
    "TIFF": ".tif",
    "WEBP": ".webp",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("paths", nargs="+", type=Path)
    return parser.parse_args()


def iter_files(paths: list[Path]):
    for path in paths:
        if path.is_dir():
            for sub in path.rglob("*"):
                if sub.is_file():
                    yield sub
        elif path.is_file():
            yield path


def exif_datetime_string(ts: float) -> str:
    return dt.datetime.fromtimestamp(ts).strftime("%Y:%m:%d %H:%M:%S")


def detect_format(path: Path) -> str | None:
    try:
        with Image.open(path) as img:
            if img.format:
                return img.format.upper()
    except Exception:
        return None
    return None


def target_extension_for_format(fmt: str) -> str | None:
    return EXTENSION_BY_FORMAT.get(fmt.upper())


def preserve_times(src_stat: os.stat_result, path: Path) -> None:
    os.utime(path, ns=(src_stat.st_atime_ns, src_stat.st_mtime_ns))


def read_exif_dict(img: Image.Image) -> dict:
    exif_bytes = img.info.get("exif")
    if exif_bytes:
        return piexif.load(exif_bytes)
    return {"0th": {}, "Exif": {}, "GPS": {}, "Interop": {}, "1st": {}, "thumbnail": None}

def _sanitize_exif_value(value):
    if isinstance(value, tuple):
        sanitized = tuple(_sanitize_exif_value(item) for item in value)
        if all(isinstance(item, int) for item in sanitized):
            return tuple(min(max(item, 0), 65535) for item in sanitized)
        return sanitized

    if isinstance(value, list):
        sanitized = [_sanitize_exif_value(item) for item in value]
        if all(isinstance(item, int) for item in sanitized):
            return [min(max(item, 0), 65535) for item in sanitized]
        return sanitized

    return value


def sanitize_exif_dict(exif_dict: dict) -> dict:
    result = {}
    for ifd_name, ifd_value in exif_dict.items():
        if ifd_name == "thumbnail":
            result[ifd_name] = ifd_value
            continue

        clean_ifd = {}
        for tag, value in ifd_value.items():
            clean_ifd[tag] = _sanitize_exif_value(value)
        result[ifd_name] = clean_ifd

    if "thumbnail" not in result:
        result["thumbnail"] = None

    return result

def ensure_exif_dates(path: Path, source_time: float) -> bool:
    with Image.open(path) as img:
        fmt = (img.format or "").upper()
        if fmt != "JPEG":
            return False

        exif_dict = read_exif_dict(img)
        dt_value = exif_datetime_string(source_time).encode()

        changed = False

        if piexif.ImageIFD.DateTime not in exif_dict["0th"]:
            exif_dict["0th"][piexif.ImageIFD.DateTime] = dt_value
            changed = True

        if piexif.ExifIFD.DateTimeOriginal not in exif_dict["Exif"]:
            exif_dict["Exif"][piexif.ExifIFD.DateTimeOriginal] = dt_value
            changed = True

        if piexif.ExifIFD.DateTimeDigitized not in exif_dict["Exif"]:
            exif_dict["Exif"][piexif.ExifIFD.DateTimeDigitized] = dt_value
            changed = True

        if not changed:
            return False

        exif_bytes = piexif.dump(sanitize_exif_dict(exif_dict))
        img.save(path, format="JPEG", exif=exif_bytes, quality="keep")
        return True


def unique_target(path: Path) -> Path:
    if not path.exists():
        return path

    stem = path.stem
    suffix = path.suffix
    parent = path.parent
    index = 1
    while True:
        candidate = parent / f"{stem}.{index}{suffix}"
        if not candidate.exists():
            return candidate
        index += 1


def rename_to_matching_extension(path: Path, correct_ext: str) -> Path:
    current_ext = path.suffix.lower()
    if current_ext == correct_ext:
        return path

    target = path.with_suffix(correct_ext)
    if target.exists() and target != path:
        target = unique_target(target)

    shutil.move(str(path), str(target))
    return target


def process_file(path: Path) -> None:
    src_stat = path.stat()
    source_time = src_stat.st_mtime

    fmt = detect_format(path)
    if fmt is None:
        return

    correct_ext = target_extension_for_format(fmt)
    if correct_ext is None:
        return

    new_path = rename_to_matching_extension(path, correct_ext)
    ensure_exif_dates(new_path, source_time)
    preserve_times(src_stat, new_path)


def main() -> int:
    args = parse_args()
    seen: set[Path] = set()

    for file_path in iter_files(args.paths):
        resolved = file_path.resolve()
        if resolved in seen:
            continue
        seen.add(resolved)
        process_file(file_path)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
