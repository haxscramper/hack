#!/usr/bin/env python
import json
import re
import os
import sqlite3
import subprocess
from pathlib import Path
from zipfile import ZipFile
from xml.etree import ElementTree as ET
import logging
from joblib import Memory
import plumbum
import plumbum.commands.processes

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
)

CALIBRE_LIBRARY = Path.home() / "defaultdirs/documents/calibre"
METADATA_DB = CALIBRE_LIBRARY / "metadata.db"

NS = {
    "dc": "http://purl.org/dc/elements/1.1/",
    "opf": "http://www.idpf.org/2007/opf",
    "xhtml": "http://www.w3.org/1999/xhtml",
}


def normalize_text(text: str) -> str:
    text = text.replace("\r", "\n")
    text = re.sub(r"\u00a0", " ", text)
    text = re.sub(r"[ \t]+", " ", text)
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text.strip()


def expect_sub(text: str, sub: str):
    if sub not in text:
        Path("/tmp/debug-book.txt").write_text(text)
        raise ValueError(f"{sub} missing")


epub_reader_text = Memory("/tmp/calibre_book_metadata_cache.cache", verbose=0)


@epub_reader_text.cache
def extract_epub_text(epub_path: Path) -> str:
    outfile = Path("/tmp/result.txt")
    env = {
        "PATH": "/usr/bin:/bin",
        "HOME": os.environ["HOME"],
        "LANG": os.environ.get("LANG", "C.UTF-8"),
    }
    try:
        plumbum.local["/bin/ebook-convert"].with_env(**env).run(
            [str(epub_path), str(outfile)])
        return normalize_text(outfile.read_text())

    except plumbum.commands.processes.ProcessExecutionError as e:
        s_e = str(e)
        if "ValueError: Not a ZIP file":
            return ""

        else:
            raise e from None


def get_document_text(book_dir: Path) -> str | None:
    for path in sorted(book_dir.rglob("*.epub")):
        return extract_epub_text(path)

    return None


@epub_reader_text.cache
def parse_ffnet(text: str) -> dict | None:
    url_match = re.search(
        r"Original source:\s*(https?://(?:www\.)?fanfiction\.net/s/\d+/\d+/[^\s]+)",
        text,
        re.IGNORECASE,
    )

    if not url_match:
        expect_sub(text, "Exported with the assistance of")
        return None

    lines = [line.strip() for line in text.splitlines()]
    lines = [line for line in lines if line]

    by_idx = None
    for i, line in enumerate(lines[:40]):
        if re.fullmatch(r"By:\s+.+", line):
            by_idx = i
            break
    if by_idx is None:
        expect_sub(text, "Exported with the assistance of")
        return None

    title = None
    if by_idx > 0:
        candidate = lines[by_idx - 1].strip()
        if candidate and not candidate.startswith("By:"):
            title = candidate

    desc = None
    for line in lines[by_idx + 1:]:
        if re.match(
                r"^(Status|Published|Updated|Words|Chapters|Rated|Original source):",
                line,
                re.IGNORECASE,
        ):
            break
        if line:
            desc = line.strip()
            break

    if not title or not desc:
        expect_sub(text, "Exported with the assistance of")
        return None

    return {
        "title": title,
        "url": url_match.group(1),
        "description": desc,
        "source": "fanfiction.net",
    }


@epub_reader_text.cache
def parse_ao3(text: str) -> dict | None:
    url_match = re.search(
        r"Posted originally on the Archive of Our Own at (https?://archiveofourown\.org/works/\d+)",
        text,
        re.IGNORECASE,
    )
    if not url_match:
        expect_sub(text, "Posted originally on the Archive of Our Own")
        return None

    m = re.search(
        r"\n(?P<title>[^\n]+)\nby\s+[^\n]+\nSummary\n\n(?P<summary>.+?)(?:\n\n(?:Chapter Text|Notes|Work Text|See the end of the chapter|$))",
        text,
        re.IGNORECASE | re.DOTALL,
    )
    if not m:
        expect_sub(text, "Posted originally on the Archive of Our Own")
        return None

    title = m.group("title").strip()
    summary = normalize_text(m.group("summary"))

    if not title or not summary:
        expect_sub(text, "Posted originally on the Archive of Our Own")
        return None

    return {
        "title": title,
        "url": url_match.group(1),
        "description": summary,
        "source": "archiveofourown.org",
    }


def parse_document(text: str) -> dict | None:
    for parser in (parse_ffnet, parse_ao3):
        result = parser(text)
        if result:
            return result
    return None


def iter_calibre_books():
    conn = sqlite3.connect(METADATA_DB)
    conn.row_factory = sqlite3.Row
    try:
        rows = conn.execute(
            "SELECT id, path, title FROM books ORDER BY id").fetchall()
        for row in rows:
            yield row["id"], CALIBRE_LIBRARY / row["path"], row["title"]
    finally:
        conn.close()


def main():
    results = []

    book_list = list(iter_calibre_books())
    for idx, group in enumerate(book_list):
        book_id, book_dir, calibre_title = group
        if not book_dir.exists():
            continue

        logging.info(f"[{idx}]/[{len(book_list)}]")

        text = get_document_text(book_dir)

        if not text:
            continue
        parsed = parse_document(text)
        if not parsed:
            continue
        parsed["book_id"] = book_id
        parsed["calibre_title"] = calibre_title
        parsed["path"] = str(book_dir)
        results.append(parsed)

    Path("/tmp/result.json").write_text(
        json.dumps(results, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
