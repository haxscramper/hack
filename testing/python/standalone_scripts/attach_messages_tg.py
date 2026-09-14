#!/usr/bin/env python

import json
import shutil
import sys
from dataclasses import asdict, dataclass
from enum import StrEnum
from pathlib import Path

from beartype import beartype
from beartype.typing import Any
from loguru import logger
from plumbum import local


@dataclass(frozen=True)
class Message:
    message_id: int | str
    text: str


@dataclass(frozen=True)
class Fingerprint:
    prompt: str
    negative_prompt: str
    seed: str
    width: str
    height: str


@dataclass(frozen=True)
class FileMatch:
    path: Path
    fingerprint: Fingerprint
    candidates: tuple[Message, ...]


class Category(StrEnum):
    MISSING_MESSAGE = "missing-message"
    UNIQUE = "unique"
    AMBIGUOUS = "ambiguous"


@beartype
def message_text_parts_to_string(text_field: Any) -> str:
    match text_field:
        case str():
            return text_field

        case list():
            parts: list[str] = []

            for item in text_field:
                match item:
                    case str():
                        parts.append(item)

                    case dict():
                        value = item.get("text")

                        if isinstance(value, str):
                            parts.append(value)

            return "".join(parts)

        case _:
            return ""


@beartype
def parse_json_strings(value: Any) -> Any:
    match value:
        case str():
            try:
                return parse_json_strings(json.loads(value))
            except json.JSONDecodeError:
                return value

        case dict():
            return {
                key: parse_json_strings(nested)
                for key, nested in value.items()
            }

        case list():
            return [parse_json_strings(nested) for nested in value]

        case _:
            return value


@beartype
def find_first_field(value: Any, field_name: str, default=None) -> Any | None:
    match value:
        case dict():
            if field_name in value:
                return value[field_name]

            for nested in value.values():
                result = find_first_field(nested, field_name, default=default)

                if result is not None:
                    return result

        case list():
            for nested in value:
                result = find_first_field(nested, field_name, default=default)

                if result is not None:
                    return result

    return default


@beartype
def read_png_fingerprint(image_path: Path) -> Fingerprint:
    exiftool = local["exiftool"]
    output = exiftool("-json", "-struct", str(image_path))
    metadata = parse_json_strings(json.loads(output))

    return Fingerprint(
        prompt=str(find_first_field(metadata, "prompt")),
        negative_prompt=str(
            find_first_field(metadata, "negativePrompt", default="")),
        seed=str(find_first_field(metadata, "seed")),
        width=str(find_first_field(metadata, "width")),
        height=str(find_first_field(metadata, "height")),
    )


@beartype
def find_candidate_messages(
    path: Path,
    fingerprint: Fingerprint,
    messages: list[Message],
) -> tuple[Message, ...]:
    candidates = [
        message for message in messages
        if (fingerprint.prompt in message.text
            and fingerprint.negative_prompt in message.text)
    ]

    if len(candidates) <= 1:
        return tuple(candidates)

    logger.warning(f"{path.name} is ambiguous: {len(candidates)} messages")

    for value in (
            fingerprint.seed,
            fingerprint.width,
            fingerprint.height,
    ):
        narrowed = [message for message in candidates if value in message.text]
        logger.info(f"{value} narrowed {len(candidates)} -> {len(narrowed)}")

        if narrowed:
            candidates = narrowed

        if len(candidates) <= 1:
            break

    return tuple(candidates)


@beartype
def destination_relative_path(
    image_path: Path,
    export_dir: Path,
    files_dir: Path,
) -> Path:
    if image_path.is_relative_to(files_dir):
        return image_path.relative_to(files_dir)

    return image_path.relative_to(export_dir)


@beartype
def move_image(image_path: Path, destination: Path) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)

    if destination.exists():
        raise FileExistsError(
            f"Cannot move {image_path} because destination already exists: "
            f"{destination}")

    shutil.move(image_path, destination)


@beartype
def write_fingerprints(matches: list[FileMatch], output_path: Path) -> None:
    fingerprints: dict[str, dict[str, str]] = {}
    source_paths: dict[str, Path] = {}

    for match in matches:
        image_name = match.path.name

        if image_name in fingerprints:
            raise ValueError(
                f"Cannot use image name {image_name!r} as a unique fingerprint "
                f"key because it is shared by {source_paths[image_name]} and "
                f"{match.path}")

        source_paths[image_name] = match.path
        fingerprints[image_name] = asdict(match.fingerprint)

    output_path.write_text(
        json.dumps(fingerprints, ensure_ascii=False, indent=2),
        encoding="utf-8",
    )


@beartype
def write_messages(messages: list[Message], output_path: Path) -> None:
    output_path.write_text(
        json.dumps(
            [message.text for message in messages],
            ensure_ascii=False,
            indent=2,
        ),
        encoding="utf-8",
    )


@beartype
def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit(f"Usage: {sys.argv[0]} <telegram-export-directory>")

    export_dir = Path(sys.argv[1]).resolve()
    result_json = export_dir / "result.json"
    files_dir = export_dir / "files"
    output_dir = export_dir / "categorized"

    data = json.loads(result_json.read_text(encoding="utf-8"))

    messages = [
        Message(
            message_id=raw_message.get("id", index),
            text=text,
        ) for index, raw_message in enumerate(data["messages"])
        if (text := message_text_parts_to_string(raw_message.get("text", "")))
    ]

    messages = list({obj.text: obj for obj in messages}.values())

    logger.info(f"Found {len(messages)} messages")

    image_paths: list[Path] = []
    seen_paths: set[Path] = set()

    for raw_message in data["messages"]:
        file_value = raw_message.get("file")

        if not isinstance(file_value, str):
            continue

        image_path = (export_dir / file_value).resolve()

        if image_path.suffix.lower() != ".png":
            continue

        if not image_path.is_relative_to(export_dir):
            raise ValueError(
                f"Telegram export references a file outside the export "
                f"directory: {image_path}")

        if image_path not in seen_paths:
            seen_paths.add(image_path)
            image_paths.append(image_path)

    matches: list[FileMatch] = []

    for image_path in image_paths:
        fingerprint = read_png_fingerprint(image_path)
        candidates = find_candidate_messages(image_path, fingerprint, messages)

        matches.append(
            FileMatch(
                path=image_path,
                fingerprint=fingerprint,
                candidates=candidates,
            ))

    write_fingerprints(matches, Path("/tmp/fingerprints.json"))
    write_messages(messages, Path("/tmp/messages.json"))

    message_to_files: dict[int | str, list[Path]] = {}

    for match in matches:
        for candidate in match.candidates:
            message_to_files.setdefault(
                candidate.message_id,
                [],
            ).append(match.path)

    for match in matches:
        relative_path = destination_relative_path(
            match.path,
            export_dir,
            files_dir,
        )

        if not match.candidates:
            category = Category.MISSING_MESSAGE
            paired_message = None
        elif (len(match.candidates) == 1
              and len(message_to_files[match.candidates[0].message_id]) == 1):
            category = Category.UNIQUE
            paired_message = match.candidates[0]
        else:
            category = Category.AMBIGUOUS
            paired_message = None

        destination = output_dir / category.value / relative_path
        move_image(match.path, destination)

        if paired_message is not None:
            destination.with_suffix(".txt").write_text(
                paired_message.text,
                encoding="utf-8",
            )


if __name__ == "__main__":
    main()
