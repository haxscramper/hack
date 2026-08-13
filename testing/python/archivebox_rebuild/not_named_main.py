#!/usr/bin/env python

from __future__ import annotations

import argparse
import importlib
import logging
import os
from copy import deepcopy
from dataclasses import dataclass
from pathlib import Path

from beartype import beartype
from beartype.typing import Any, Optional
from lxml import etree, html
from readability import Document
from loguru import logger


@beartype
@dataclass(frozen=True)
class ArchiveSnapshot:
    timestamp: str
    url: str
    title: str
    archive_dir: Path
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
            "check_db": True
        },
        {
            "in_memory_db": False
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
        f"Failed to call ArchiveBox setup_django with any supported signature: {joined}"
    )


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
            snapshot_model = getattr(module, "Snapshot")
            return snapshot_model
        except Exception as error:
            model_errors.append(f"{module_name}: {error}")

    joined = "; ".join(model_errors)
    raise RuntimeError(
        f"Failed to import ArchiveBox Snapshot model after initialization: {joined}"
    )


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
    paths: list[Path] = []
    for path in preferred:
        if path.is_file():
            paths.append(path)

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

    title = text_from_value(getattr(snapshot, "title", None)) or url
    archive_dir = path_from_value(getattr(snapshot, "archive_path", None),
                                  input_dir)
    archive_dir = archive_dir or input_dir / "archive" / timestamp
    html_path = pick_html_path(archive_dir)

    if html_path is None:
        logger.info(
            f"Skipping snapshot {timestamp} because no HTML capture was found in {archive_dir}"
        )
        return None

    return ArchiveSnapshot(
        timestamp=timestamp,
        url=url,
        title=title,
        archive_dir=archive_dir,
        html_path=html_path,
    )


@beartype
def normalized_source_html(snapshot: ArchiveSnapshot) -> str:
    source_root = html.document_fromstring(snapshot.html_path.read_bytes(),
                                           base_url=snapshot.url)
    source_root.make_links_absolute(snapshot.url, resolve_base_href=True)
    return html.tostring(source_root, encoding="unicode", method="html")


@beartype
def cleaned_summary_root(summary_html: str,
                         source_url: str) -> html.HtmlElement:
    summary_root = html.document_fromstring(summary_html, base_url=source_url)
    for node in summary_root.xpath("//script|//style|//noscript|//iframe"):
        node.drop_tree()
    return summary_root


@beartype
def extracted_title(document: Document, snapshot: ArchiveSnapshot) -> str:
    candidate = text_from_value(document.short_title())
    return candidate or snapshot.title


@beartype
def summary_nested_nodes(
        summary_root: html.HtmlElement) -> list[html.HtmlElement]:
    match summary_root.tag.lower():
        case "html":
            nodes = list(summary_root.xpath("/html/body/*"))
        case "body":
            nodes = list(summary_root)
        case _:
            nodes = [summary_root]
    return nodes


@beartype
def final_html_document(snapshot: ArchiveSnapshot) -> str:
    source_html = normalized_source_html(snapshot)
    document = Document(source_html, url=snapshot.url)
    title = extracted_title(document, snapshot)
    summary_root = cleaned_summary_root(document.summary(html_partial=False),
                                        snapshot.url)

    root = etree.Element("html")
    head = etree.SubElement(root, "head")
    etree.SubElement(head, "meta", charset="utf-8")
    etree.SubElement(head,
                     "meta",
                     name="viewport",
                     content="width=device-width, initial-scale=1")
    etree.SubElement(head,
                     "meta",
                     name="archivebox-timestamp",
                     content=snapshot.timestamp)
    etree.SubElement(head,
                     "meta",
                     name="archivebox-source",
                     content=snapshot.url)
    title_node = etree.SubElement(head, "title")
    title_node.text = title

    body = etree.SubElement(root, "body")
    header = etree.SubElement(body, "header")
    heading = etree.SubElement(header, "h1")
    heading.text = title
    source_paragraph = etree.SubElement(header, "p")
    source_link = etree.SubElement(source_paragraph, "a", href=snapshot.url)
    source_link.text = snapshot.url

    main = etree.SubElement(body, "main")
    article = etree.SubElement(main, "article")
    nested_nodes = summary_nested_nodes(summary_root)

    if len(nested_nodes) == 0:
        article.text = summary_root.text_content().strip()
    else:
        for node in nested_nodes:
            article.append(deepcopy(node))

    return "<!DOCTYPE html>\n" + html.tostring(
        root, encoding="unicode", method="html", pretty_print=False)


@beartype
def write_snapshot(snapshot: ArchiveSnapshot, output_dir: Path) -> Path:
    output_path = output_dir / f"{snapshot.timestamp}.html"
    output_path.write_text(final_html_document(snapshot), encoding="utf-8")
    return output_path


@beartype
def export_snapshots(input_dir: Path, output_dir: Path) -> None:
    snapshot_model = load_snapshot_model(input_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    queryset = snapshot_model.objects.all().order_by("timestamp")
    for snapshot in queryset.iterator():
        record = snapshot_record(snapshot, input_dir)
        if record is None:
            continue
        logger.info(
            f"Exporting snapshot {record.timestamp} from {record.html_path}")
        output_path = write_snapshot(record, output_dir)
        logger.info(f"Wrote simplified article to {output_path}")


@beartype
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=Path)
    parser.add_argument("output_dir", type=Path)
    return parser.parse_args()


@beartype
def main() -> None:
    args = parse_args()
    export_snapshots(args.input_dir.resolve(), args.output_dir.resolve())


if __name__ == "__main__":
    main()
