#!/usr/bin/env python
# /// script
# dependencies = [
#   "fal-client",
#   "requests",
# ]
# ///

from __future__ import annotations

import argparse
import logging
import pathlib
from typing import Any

import fal_client
import requests

LOGGER = logging.getLogger("meshy_image_to_3d")
MODEL_ID = "fal-ai/meshy/v6/image-to-3d"




def configure_logging(verbose: bool) -> None:
    level = logging.DEBUG
    logging.basicConfig(
        level=level,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=(
        "Upload an input image to fal.ai Meshy-6 Preview, wait for conversion, "
        "and download all resulting files into the current directory."))
    parser.add_argument(
        "input_file",
        type=pathlib.Path,
        help="Path to the input image file",
    )
    parser.add_argument(
        "--topology",
        choices=["triangle", "quad"],
        default="triangle",
        help="Generated mesh topology",
    )
    parser.add_argument(
        "--target-polycount",
        type=int,
        default=300000,
        help="Target polygon count",
    )
    parser.add_argument(
        "--symmetry-mode",
        choices=["off", "auto", "on"],
        default="auto",
        help="Symmetry mode",
    )
    parser.add_argument(
        "--no-remesh",
        action="store_true",
        help="Disable remeshing",
    )
    parser.add_argument(
        "--no-texture",
        action="store_true",
        help="Disable texture generation",
    )
    parser.add_argument(
        "--enable-pbr",
        action="store_true",
        help="Generate PBR maps in addition to base color",
    )
    parser.add_argument(
        "--texture-prompt",
        type=str,
        help="Optional text prompt to guide texturing",
    )
    parser.add_argument(
        "--texture-image",
        type=pathlib.Path,
        help="Optional image to guide texturing",
    )
    parser.add_argument(
        "--enable-rigging",
        action="store_true",
        help="Enable humanoid rigging",
    )
    parser.add_argument(
        "--rigging-height-meters",
        type=float,
        default=1.7,
        help="Approximate character height in meters for rigging",
    )
    parser.add_argument(
        "--enable-animation",
        action="store_true",
        help="Enable animation preset application",
    )
    parser.add_argument(
        "--animation-action-id",
        type=int,
        default=1001,
        help="Animation action ID",
    )
    parser.add_argument(
        "--pose-mode",
        choices=["", "a-pose", "t-pose"],
        default="",
        help="Optional pose mode",
    )
    parser.add_argument(
        "--disable-safety-checker",
        action="store_true",
        help="Disable the safety checker",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Enable debug logging",
    )
    return parser.parse_args()


def ensure_input_file(path: pathlib.Path) -> pathlib.Path:
    resolved = path.expanduser().resolve()
    if not resolved.is_file():
        raise FileNotFoundError(f"Input file does not exist: {resolved}")
    LOGGER.debug("Validated input file: %s", resolved)
    return resolved


def upload_file(path: pathlib.Path) -> str:
    LOGGER.info("Uploading input file: %s", path)
    url = fal_client.upload_file(str(path))
    LOGGER.info("Uploaded input file to: %s", url)
    return url


def on_queue_update(update: Any) -> None:
    LOGGER.debug("Received queue update object: %r", update)
    if isinstance(update, fal_client.InProgress):
        if getattr(update, "logs", None):
            for entry in update.logs:
                message = entry.get("message", "")
                LOGGER.info("Remote log: %s", message)
        if getattr(update, "status", None):
            LOGGER.info("Remote status: %s", update.status)


def build_arguments(args: argparse.Namespace,
                    image_url: str) -> dict[str, Any]:
    payload: dict[str, Any] = {
        "image_url": image_url,
        "topology": args.topology,
        "target_polycount": args.target_polycount,
        "symmetry_mode": args.symmetry_mode,
        "should_remesh": not args.no_remesh,
        "should_texture": not args.no_texture,
        "enable_pbr": args.enable_pbr,
        "pose_mode": args.pose_mode,
        "enable_rigging": args.enable_rigging,
        "rigging_height_meters": args.rigging_height_meters,
        "enable_animation": args.enable_animation,
        "animation_action_id": args.animation_action_id,
        "enable_safety_checker": False,
    }

    if args.texture_prompt:
        payload["texture_prompt"] = args.texture_prompt

    if args.texture_image:
        texture_image_path = ensure_input_file(args.texture_image)
        LOGGER.info("Uploading texture guide image: %s", texture_image_path)
        payload["texture_image_url"] = fal_client.upload_file(
            str(texture_image_path))
        LOGGER.info("Uploaded texture guide image to: %s",
                    payload["texture_image_url"])

    LOGGER.debug("Built request payload: %r", payload)
    return payload


def sanitize_filename(name: str) -> str:
    safe = pathlib.Path(name).name
    if not safe:
        raise ValueError(f"Invalid file name from API: {name!r}")
    return safe


def download_file(url: str, destination: pathlib.Path) -> None:
    LOGGER.info("Downloading %s -> %s", url, destination)
    with requests.get(url, stream=True, timeout=120) as response:
        response.raise_for_status()
        with destination.open("wb") as handle:
            for chunk in response.iter_content(chunk_size=1024 * 1024):
                if chunk:
                    handle.write(chunk)
    LOGGER.info("Saved file: %s", destination)


def maybe_download_file_entry(entry: Any, prefix: str) -> None:
    if not isinstance(entry, dict):
        LOGGER.debug("Skipping non-dict entry at %s: %r", prefix, entry)
        return

    url = entry.get("url")
    file_name = entry.get("file_name")
    if isinstance(url, str) and isinstance(file_name, str):
        destination = pathlib.Path.cwd() / sanitize_filename(file_name)
        LOGGER.debug("Found downloadable file at %s: %s", prefix, entry)
        download_file(url, destination)
        return

    for key, value in entry.items():
        child_prefix = f"{prefix}.{key}" if prefix else str(key)
        if isinstance(value, dict):
            maybe_download_file_entry(value, child_prefix)
        elif isinstance(value, list):
            for index, item in enumerate(value):
                maybe_download_file_entry(item, f"{child_prefix}[{index}]")


def submit_and_wait(arguments: dict[str, Any]) -> dict[str, Any]:
    LOGGER.info("Submitting request to model: %s", MODEL_ID)
    result = fal_client.subscribe(
        MODEL_ID,
        arguments=arguments,
        with_logs=True,
        on_queue_update=on_queue_update,
    )
    LOGGER.info("Received final result from model")
    LOGGER.debug("Full API result: %r", result)
    return result


def main() -> None:
    args = parse_args()
    configure_logging(args.verbose)
    logging.getLogger("httpcore.http11").setLevel(logging.INFO)
    logging.getLogger("httpcore.connection").setLevel(logging.INFO)
    
    LOGGER.info("Starting Meshy image-to-3D workflow")
    input_path = ensure_input_file(args.input_file)
    image_url = upload_file(input_path)
    arguments = build_arguments(args, image_url)
    result = submit_and_wait(arguments)

    LOGGER.info("Downloading result files into current directory: %s",
                pathlib.Path.cwd())
    maybe_download_file_entry(result, "result")
    LOGGER.info("All downloadable result files have been saved")


if __name__ == "__main__":
    main()
