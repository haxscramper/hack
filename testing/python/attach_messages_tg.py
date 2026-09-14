#!/usr/bin/env python
import json
import sys
from pathlib import Path


def message_text_parts_to_string(text_field):
    if isinstance(text_field, str):
        return text_field

    if isinstance(text_field, list):
        parts = []
        for item in text_field:
            if isinstance(item, str):
                parts.append(item)
            elif isinstance(item, dict):
                value = item.get("text")
                if isinstance(value, str):
                    parts.append(value)
        return "".join(parts)

    return ""


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit(f"Usage: {sys.argv[0]} <telegram-export-directory>")

    export_dir = Path(sys.argv[1]).resolve()
    result_json = export_dir / "result.json"
    files_dir = export_dir / "files"

    data = json.loads(result_json.read_text(encoding="utf-8"))
    messages = data["messages"]

    file_messages = [msg for msg in messages if msg.get("file")]
    text_messages = [msg for msg in messages if message_text_parts_to_string(msg.get("text", ""))]

    if len(file_messages) != len(text_messages):
        raise ValueError(
            f"Mismatch between file messages ({len(file_messages)}) and text messages ({len(text_messages)})"
        )

    text_iter = reversed(text_messages)

    for file_msg in reversed(file_messages):
        text_msg = next(text_iter)
        file_rel = file_msg["file"]
        image_path = export_dir / file_rel
        if image_path.parent != files_dir:
            image_path = export_dir / file_rel

        text = message_text_parts_to_string(text_msg.get("text", ""))
        output_path = image_path.with_suffix(".txt")
        output_path.write_text(text, encoding="utf-8")


if __name__ == "__main__":
    main()
