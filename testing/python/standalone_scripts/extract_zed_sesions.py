#!/usr/bin/env -S uv run
# /// script
# dependencies = ["zstandard", "pyyaml"]
# ///

import os
import sys
import sqlite3
import json
import string
from pathlib import Path

import zstandard as zstd
import yaml

import re

_UNPRINTABLE = re.compile(
    r"[^\n\x20-\x7e\x85\u00a0-\ud7ff\ue000-\ufffd\U00010000-\U0010ffff]")


# PyYAML's emitter *rejects* literal block style whenever the scalar has trailing
# whitespace on any line, `\r`, or non-printable characters, and silently falls
# back to double-quoted. You must sanitize the string first.
def clean_block_scalar(text):
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    text = text.expandtabs(4)
    text = _UNPRINTABLE.sub("", text)
    # trailing whitespace on a line makes literal style impossible
    text = "\n".join(line.rstrip() for line in text.split("\n"))
    return text


def multiline_presenter(dumper, data):
    if "\n" in data:
        cleaned = clean_block_scalar(data)
        if cleaned.strip():
            return dumper.represent_scalar(
                "tag:yaml.org,2002:str",
                cleaned,
                style="|",
            )
    return dumper.represent_scalar("tag:yaml.org,2002:str", data)


class MultilineSafeDumper(yaml.SafeDumper):
    pass


MultilineSafeDumper.add_representer(str, multiline_presenter)


def get_default_db_path():
    return Path.home() / ".local" / "share" / "zed" / "threads" / "threads.db"


def sanitize_filename(name):
    """Sanitize the chat summary to make it a valid, cross-platform filename."""
    if not name:
        return "unnamed_thread"
    valid_chars = f"-_.() {string.ascii_letters}{string.digits}"
    cleaned = "".join(c for c in name if c in valid_chars)
    return cleaned.strip()[:100] or "unnamed_thread"


def export_chats(export_dir, db_path=None):
    if db_path is None:
        db_path = get_default_db_path()

    if not db_path.exists():
        print(f"Error: Zed threads database not found at {db_path}")
        print("Are you sure Zed is installed and has generated AI chats?")
        sys.exit(1)

    export_path = Path(export_dir)
    export_path.mkdir(parents=True, exist_ok=True)

    # Initialize ZSTD decompressor
    dctx = zstd.ZstdDecompressor()

    # Connect to the SQLite database in Read-Only mode to avoid locking the live Editor
    db_uri = f"file:{db_path.absolute().as_posix()}?mode=ro"

    try:
        conn = sqlite3.connect(db_uri, uri=True)
        conn.row_factory = sqlite3.Row
        cursor = conn.cursor()
        cursor.execute("SELECT * FROM threads")
        rows = cursor.fetchall()
    except sqlite3.OperationalError as e:
        print(f"Error reading from the database: {e}")
        sys.exit(1)

    print(f"Found {len(rows)} threads in the database...")

    success_count = 0
    for row in rows:
        row_dict = dict(row)

        thread_id = row_dict.get("id", "unknown_id")
        summary = row_dict.get("summary", "")
        updated_at = row_dict.get("updated_at", "")

        decompressed_json = None

        for key, value in row_dict.items():
            # Check for ZSTD magic number (0xFD2FB528)
            if isinstance(value,
                          bytes) and value.startswith(b'\x28\xb5\x2f\xfd'):
                try:
                    with dctx.stream_reader(value) as reader:
                        decompressed_bytes = reader.read()
                    decompressed_json = json.loads(
                        decompressed_bytes.decode('utf-8'))
                    break
                except Exception as e:
                    print(
                        f"Failed to decompress/parse column '{key}' in thread {thread_id}: {e}"
                    )
            # Fallback for plain JSON text
            elif isinstance(value, str) and (value.strip().startswith('{')
                                             or value.strip().startswith('[')):
                try:
                    decompressed_json = json.loads(value)
                    break
                except json.JSONDecodeError:
                    pass

        if not decompressed_json:
            print(f"Skipping thread {thread_id}: No valid payload data found.")
            continue

        chat_data = {
            "id": thread_id,
            "summary": summary,
            "updated_at": updated_at,
            "messages": decompressed_json
        }

        safe_summary = sanitize_filename(summary)
        filename = f"{safe_summary}_{thread_id}.yaml" if summary else f"{thread_id}.yaml"
        file_path = export_path / filename

        try:
            with open(file_path, "w", encoding="utf-8") as f:
                # Dumper=yaml.SafeDumper is strictly safer and avoids some meta-tags
                yaml.dump(
                    chat_data,
                    f,
                    Dumper=MultilineSafeDumper,
                    allow_unicode=True,
                    default_flow_style=False,
                    sort_keys=False,
                    width=10_000,
                )
            success_count += 1
        except Exception as e:
            print(f"Error writing to {file_path}: {e}")

    print(
        f"\nSuccessfully exported {success_count} chats to: {export_path.absolute()}"
    )


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(
            "Usage: python export_zed_chats.py <export_directory> [path_to_threads.db]"
        )
        sys.exit(1)

    target_dir = sys.argv[1]
    db_override = sys.argv[2] if len(sys.argv) > 2 else None

    export_chats(target_dir, Path(db_override) if db_override else None)
