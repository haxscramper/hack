#!/usr/bin/env python

from __future__ import annotations

import argparse
import importlib
import multiprocessing as mp
import os
import subprocess
from dataclasses import dataclass
from pathlib import Path

from beartype import beartype
from beartype.typing import Any, Optional
from loguru import logger


@beartype
@dataclass(frozen=True)
class ArchiveSnapshot:
    timestamp: str
    url: str
    html_path: Path


@beartype
def text_from_value(value: Any) -> Optional[str]:
    match value:
        case None:
            return None
        case str():
            text = value.strip()
            return text or None
        case Path():
            text = str(value).strip()
            return text or None
        case _:
            text = str(value).strip()
            return text or None


@beartype
def path_from_value(value: Any, base_dir: Path) -> Optional[Path]:
    text = text_from_value(value)
    if text is None:
        return None

    path = Path(text)
    return path if path.is_absolute() else base_dir / path


@beartype
def setup_archivebox(setup_function: Any) -> None:
    attempts = [
        {
            "in_memory_db": False,
            "check_db": True,
        },
        {
            "in_memory_db": False,
        },
        {},
    ]

    errors: list[str] = []
    for kwargs in attempts:
        try:
            setup_function(**kwargs)
            return
        except TypeError as error:
            errors.append(f"{kwargs}: {error}")

    joined = "; ".join(errors)
    raise RuntimeError(
        "Failed to call ArchiveBox setup_django with any supported "
        f"signature: {joined}")


@beartype
def load_snapshot_model(input_dir: Path) -> Any:
    os.environ["OUTPUT_DIR"] = str(input_dir)
    os.environ["ARCHIVEBOX_DATA_DIR"] = str(input_dir)
    os.chdir(input_dir)

    setup_errors: list[str] = []
    for module_name in ["archivebox.config.legacy", "archivebox.config"]:
        try:
            module = importlib.import_module(module_name)
            setup_archivebox(getattr(module, "setup_django"))
            break
        except Exception as error:
            setup_errors.append(f"{module_name}: {error}")
    else:
        joined = "; ".join(setup_errors)
        raise RuntimeError(
            f"Failed to initialize ArchiveBox from {input_dir}: {joined}")

    model_errors: list[str] = []
    for module_name in ["core.models", "archivebox.core.models"]:
        try:
            module = importlib.import_module(module_name)
            return getattr(module, "Snapshot")
        except Exception as error:
            model_errors.append(f"{module_name}: {error}")

    joined = "; ".join(model_errors)
    raise RuntimeError(
        "Failed to import ArchiveBox Snapshot model after initialization: "
        f"{joined}")


@beartype
def html_candidates(archive_dir: Path) -> list[Path]:
    preferred = [
        archive_dir / "singlefile.html",
        archive_dir / "wget" / "index.html",
        archive_dir / "dom.html",
        archive_dir / "output.html",
        archive_dir / "singlefile" / "index.html",
        archive_dir / "index.html",
        archive_dir / "readability" / "content.html",
    ]

    paths = [path for path in preferred if path.is_file()]

    discovered = sorted({
        *archive_dir.rglob("*.html"),
        *archive_dir.rglob("*.htm"),
        *archive_dir.rglob("*.xhtml"),
    })

    seen = {path.resolve() for path in paths}
    for path in discovered:
        resolved = path.resolve()
        if resolved not in seen and path.is_file():
            paths.append(path)
            seen.add(resolved)

    return paths


@beartype
def pick_html_path(archive_dir: Path) -> Optional[Path]:
    candidates = html_candidates(archive_dir)
    return candidates[0] if candidates else None


@beartype
def snapshot_record(snapshot: Any,
                    input_dir: Path) -> Optional[ArchiveSnapshot]:
    timestamp = text_from_value(getattr(snapshot, "timestamp", None))
    if timestamp is None:
        raise RuntimeError(
            f"ArchiveBox snapshot is missing timestamp: {snapshot}")

    url = text_from_value(getattr(snapshot, "url", None))
    if url is None:
        raise RuntimeError(f"ArchiveBox snapshot {timestamp} is missing url")

    archive_dir = path_from_value(
        getattr(snapshot, "archive_path", None),
        input_dir,
    )
    archive_dir = archive_dir or input_dir / "archive" / timestamp
    html_path = pick_html_path(archive_dir)

    if html_path is None:
        logger.info(
            f"Skipping snapshot {timestamp} because no HTML capture was "
            f"found in {archive_dir}")
        return None

    return ArchiveSnapshot(
        timestamp=timestamp,
        url=url,
        html_path=html_path,
    )


@beartype
def output_path_for(snapshot: ArchiveSnapshot, output_dir: Path) -> Path:
    return output_dir / f"{snapshot.timestamp}.html"


@beartype
def write_snapshot(snapshot: ArchiveSnapshot,
                   output_dir: Path) -> tuple[Path, bool]:
    output_path = output_path_for(snapshot, output_dir)

    if output_path.is_file():
        return output_path, False

    command = [
        "singlefile",
        snapshot.html_path.resolve().as_uri(),
        str(output_path),
        "--remove-hidden-elements",
        "true",
        "--remove-unused-styles",
        "true",
        "--remove-unused-fonts",
        "true",
        "--block-scripts",
        "true",
        "--block-audios",
        "true",
        "--block-videos",
        "true",
        "--filename-conflict-action",
        "overwrite",
    ]

    subprocess.run(command, check=True)
    return output_path, True


@beartype
def write_snapshot_task(
        task: tuple[ArchiveSnapshot, Path]) -> tuple[str, Path, bool]:
    snapshot, output_dir = task
    output_path, converted = write_snapshot(snapshot, output_dir)
    return snapshot.timestamp, output_path, converted


@beartype
def export_snapshots(input_dir: Path, output_dir: Path) -> None:
    snapshot_model = load_snapshot_model(input_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    records: list[ArchiveSnapshot] = []
    queryset = snapshot_model.objects.all().order_by("timestamp")
    for snapshot in queryset.iterator():
        record = snapshot_record(snapshot, input_dir)
        if record is None:
            continue
        records.append(record)

    if not records:
        logger.info("No snapshots to process")
        return

    workers = max(1, os.cpu_count() or 1)
    logger.info(f"Processing {len(records)} snapshots with {workers} workers")

    ctx = mp.get_context("spawn")
    tasks = ((record, output_dir) for record in records)

    with ctx.Pool(processes=workers) as pool:
        for timestamp, output_path, converted in pool.imap_unordered(
                write_snapshot_task,
                tasks,
                chunksize=1,
        ):
            if converted:
                logger.info(
                    f"Wrote simplified HTML for {timestamp} to {output_path}")
            else:
                logger.info(
                    f"Skipping snapshot {timestamp} because output already exists at "
                    f"{output_path}")


@beartype
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=Path)
    parser.add_argument("output_dir", type=Path)
    return parser.parse_args()


@beartype
def main() -> None:
    args = parse_args()
    export_snapshots(
        args.input_dir.resolve(),
        args.output_dir.resolve(),
    )


if __name__ == "__main__":
    main()
