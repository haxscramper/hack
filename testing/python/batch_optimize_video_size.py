#!/usr/bin/env python
import subprocess
import shutil
import logging
from pathlib import Path
from typing import List
import click
from beartype import beartype
from concurrent.futures import ThreadPoolExecutor



@beartype
def get_video_files(directory: Path) -> List[Path]:
    video_extensions = {
        ".mp4", ".avi", ".mkv", ".mov", ".wmv", ".flv", ".webm", ".m4v", ".gif"
    }
    return [
        f for f in Path(directory).rglob("*")
        if f.is_file() and f.suffix.lower() in video_extensions
    ]


@beartype
def optimize_video(input_file: Path, output_file: Path) -> None:
    if input_file.suffix.lower() == ".gif":
        cmd = [
            "ffmpeg",
            "-i",
            str(input_file),
            "-c:v",
            "libx264",
            "-preset",
            "medium",
            "-crf",
            "25",
            "-vf",
            "scale='min(1920,iw)':'min(1080,ih)':force_original_aspect_ratio=decrease,fps=15",
            "-pix_fmt",
            "yuv420p",
            "-movflags",
            "+faststart",
            str(output_file),
            "-y",
        ]
    else:
        cmd = [
            "ffmpeg",
            "-i",
            str(input_file),
            "-c:v",
            "libx264",
            "-preset",
            "medium",
            "-crf",
            "23",
            "-vf",
            "scale='min(1920,iw)':'min(1080,ih)':force_original_aspect_ratio=decrease",
            "-c:a",
            "aac",
            "-b:a",
            "128k",
            "-movflags",
            "+faststart",
            str(output_file),
            "-y",
        ]

    result = subprocess.run(cmd, capture_output=True, text=True, check=True)


@beartype
def process_video_file(input_file: Path, overwrite: bool) -> None:
    if overwrite:
        temp_output = input_file.with_suffix("_overwrite.mp4")
        final_output = input_file.with_suffix(".mp4")
    else:
        final_output = input_file.parent / f"{input_file.stem}_optimized.mp4"
        temp_output = final_output

    logging.info(f"Processing: {input_file.name}")

    optimize_video(input_file, temp_output)

    if not temp_output.exists():
        logging.error(f"Output file not created: {temp_output}")
        return

    input_size = input_file.stat().st_size
    output_size = temp_output.stat().st_size

    if output_size >= input_size:
        logging.warning(
            f"Optimized file is larger than original: {input_file.name}")
        temp_output.unlink()
        return

    if overwrite:
        if input_file.suffix.lower() != ".mp4":
            shutil.move(str(temp_output), str(final_output))
        else:
            input_file.unlink()
            shutil.move(str(temp_output), str(input_file))

    logging.info(f"Completed: {input_file.name} -> {final_output.name}")


@click.command()
@click.argument(
    "input_dir",
    type=click.Path(exists=True,
                    file_okay=False,
                    dir_okay=True,
                    path_type=Path),
)
@click.option(
    "--overwrite",
    is_flag=True,
    help="Overwrite original files instead of creating copies",
    default=False,
)

@beartype
def main(input_dir: Path, overwrite: bool) -> None:
    video_files = get_video_files(input_dir)

    if not video_files:
        logging.info("No video files found in directory")
        return

    with ThreadPoolExecutor(max_workers=4) as executor:
        futures = [executor.submit(process_video_file, video_file, overwrite) for video_file in video_files]
        for future in futures:
            future.result()



    logging.info("Processing complete")


if __name__ == "__main__":
    main()
