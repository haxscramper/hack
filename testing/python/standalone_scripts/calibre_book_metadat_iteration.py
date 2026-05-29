#!/usr/bin/env python

import argparse
import csv
import re
from pathlib import Path

FFN_URL_RE = re.compile(
    r"Original source:\s*(https?://(?:www\.)?fanfiction\.net/[^\s]+)",
    re.IGNORECASE)
AO3_URL_RE = re.compile(
    r"Posted originally on the Archive of Our Own at\s+(https?://(?:www\.)?archiveofourown\.org/[^\s.]+(?:/[^\s.]+)*)",
    re.IGNORECASE,
)

STATUS_LINE_RE = re.compile(r"^\s*Status:\s*", re.IGNORECASE)
SUMMARY_HEADER_RE = re.compile(r"^\s*Summary\s*$", re.IGNORECASE)


def read_text_file(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def split_lines(text: str) -> list[str]:
    return text.replace("\r\n", "\n").replace("\r", "\n").split("\n")


def first_nonempty_line(lines: list[str]) -> str | None:
    for line in lines:
        stripped = line.strip()
        if stripped:
            return stripped
    return None


FFN_MARKER_RE = re.compile(
    r"Exported with the assistance of\s+FicHub\.net",
    re.IGNORECASE,
)
ORIGINAL_SOURCE_RE = re.compile(
    r"Original source:\s*(https?://\S+)",
    re.IGNORECASE,
)


def extract_ffn(text: str) -> tuple[str, str, str] | None:
    if not FFN_MARKER_RE.search(text):
        return None

    url_match = ORIGINAL_SOURCE_RE.search(text)
    if not url_match:
        return None

    lines = split_lines(text)
    title = first_nonempty_line(lines)
    if not title:
        return None

    status_idx = None
    for i, line in enumerate(lines):
        if STATUS_LINE_RE.match(line):
            status_idx = i
            break

    if status_idx is None:
        return None

    summary_lines = []
    i = status_idx - 1
    while i >= 0:
        stripped = lines[i].strip()
        if stripped:
            summary_lines.append(stripped)
        elif summary_lines:
            break
        i -= 1

    summary_lines.reverse()
    summary = "\n".join(summary_lines).strip()

    return title, url_match.group(1), summary


def extract_ao3(text: str) -> tuple[str, str, str] | None:
    url_match = AO3_URL_RE.search(text)
    if not url_match:
        return None

    lines = split_lines(text)
    title = first_nonempty_line(lines)
    if not title:
        return None

    summary_idx = None
    for i, line in enumerate(lines):
        if SUMMARY_HEADER_RE.match(line):
            summary_idx = i
            break

    if summary_idx is None:
        return None

    summary_lines = []
    started = False
    for line in lines[summary_idx + 1:]:
        stripped = line.strip()
        if not started:
            if not stripped:
                continue
            started = True
            summary_lines.append(stripped)
            continue

        if not stripped:
            break

        summary_lines.append(stripped)

    summary = "\n".join(summary_lines).strip()
    return title, url_match.group(1), summary


def extract_record(path: Path) -> tuple[str, str, str] | None:
    text = read_text_file(path)

    record = extract_ffn(text)
    if record is not None:
        return record

    record = extract_ao3(text)
    if record is not None:
        return record

    return None


def collect_txt_files(root: Path) -> list[Path]:
    return sorted(p for p in root.rglob("*")
                  if p.is_file() and p.suffix.lower() == ".txt")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_path")
    parser.add_argument("output_path")
    args = parser.parse_args()

    input_path = Path(args.input_path)
    output_path = Path(args.output_path)

    rows = []
    for path in collect_txt_files(input_path):
        record = extract_record(path)
        if record is not None:
            rows.append(record)

    with output_path.open("w", encoding="utf-8", newline="") as out:
        writer = csv.writer(out)
        writer.writerow(["title", "url", "summary"])
        writer.writerows(rows)


if __name__ == "__main__":
    main()
