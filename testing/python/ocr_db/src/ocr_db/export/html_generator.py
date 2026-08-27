import logging
import os
from dataclasses import dataclass
from typing import List, Optional, Tuple

from sqlalchemy import select
from sqlalchemy.orm import sessionmaker

from ocr_db.ocr_db import DocumentRecord, ElementRecord, InputFileRecord, PageRecord

logger = logging.getLogger(__name__)

SENTENCE_TERMINATORS = {'.', '!', '?', '"', '\'', '”', '’'}
SKIPPED_LABELS = {"page_footer"}


@dataclass
class MergeState:
    pending_text: Optional[str] = None
    pending_was_hyphenated: bool = False


def _load_pages(session_factory: sessionmaker,
                absolute_path: str) -> List[Tuple[int, List[ElementRecord]]]:
    """Load (page_number, elements) pairs ordered by page and element index."""
    with session_factory() as session:
        input_rec = session.scalar(
            select(InputFileRecord).where(
                InputFileRecord.absolute_path == absolute_path))
        if not input_rec:
            logger.warning(f"No input file record for {absolute_path}")
            return []
        doc = session.scalar(
            select(DocumentRecord).where(
                DocumentRecord.file_sha256 == input_rec.file_sha256))
        if not doc:
            logger.warning(f"No document record for {absolute_path}")
            return []

        pages = list(
            session.scalars(
                select(PageRecord).where(
                    PageRecord.document_id == doc.id).order_by(
                        PageRecord.page_number)))

        result = []
        for page in pages:
            elements = list(
                session.scalars(
                    select(ElementRecord).where(
                        ElementRecord.page_id == page.id).order_by(
                            ElementRecord.element_index)))
            for e in elements:
                session.expunge(e)
            result.append((page.page_number, elements))
        return result


def _get_page_number_str(elements: List[ElementRecord]) -> Optional[str]:
    for e in elements:
        if e.label == "page_footer" and e.text.strip():
            return e.text.strip()
    return None


def _get_content_elements(
        elements: List[ElementRecord]) -> List[ElementRecord]:
    """Enabled, non-skipped elements with text, in order of appearance."""
    return [
        e for e in elements
        if e.enabled and e.label not in SKIPPED_LABELS and e.text
    ]


def _strip_trailing_page_number(text: str,
                                page_number_str: Optional[str]) -> str:
    if not page_number_str:
        return text
    if text.rstrip().endswith(page_number_str):
        return text.rstrip()[:-len(page_number_str)]
    return text


def _detect_continuation(last_text: str, first_text: str) -> Tuple[bool, bool]:
    """Returns (should_merge, is_word_split)."""
    if not last_text or not first_text:
        return False, False

    last_stripped = last_text.rstrip()
    first_stripped = first_text.lstrip()

    if not last_stripped or not first_stripped:
        return False, False

    last_char = last_stripped[-1]
    first_char = first_stripped[0]

    if last_char.isalpha() and first_char.islower():
        if last_char not in SENTENCE_TERMINATORS:
            return True, True

    if last_char == '-':
        if first_char.islower():
            return True, True

    if last_char not in SENTENCE_TERMINATORS and first_char.islower():
        return True, False

    return False, False


def _merge_texts(last_text: str, first_text: str, is_word_split: bool) -> str:
    last_stripped = last_text.rstrip()
    first_stripped = first_text.lstrip()

    if is_word_split:
        if last_stripped.endswith('-'):
            return last_stripped[:-1] + first_stripped
        return last_stripped + first_stripped
    else:
        return last_stripped + " " + first_stripped


def map_label_to_markdown(label: str, text: str) -> str:
    if label in ("h1", "section_header"):
        return f"## {text}"
    if label == "h2":
        return f"### {text}"
    if label == "h3":
        return f"#### {text}"
    if label == "list_item":
        return f"- {text}"
    return text


def generate_markdown_content(session_factory: sessionmaker,
                              absolute_path: str) -> str:
    pages = _load_pages(session_factory, absolute_path)

    preview_parts: List[str] = []
    doc_parts: List[str] = []

    pending_merge_text: Optional[str] = None
    pending_merge_label: Optional[str] = None

    def flush_pending():
        nonlocal pending_merge_text, pending_merge_label
        if pending_merge_text is not None:
            md = map_label_to_markdown(pending_merge_label, pending_merge_text)
            preview_parts.append(md)
            doc_parts.append(md)
            pending_merge_text = None
            pending_merge_label = None

    for page_number, elements in pages:
        preview_parts.append(f"\n---\n\n**Page {page_number}**\n")

        page_number_str = _get_page_number_str(elements)
        content = _get_content_elements(elements)

        for idx, element in enumerate(content):
            text = element.text
            is_last_on_page = (idx == len(content) - 1)

            if is_last_on_page:
                text = _strip_trailing_page_number(text, page_number_str)

            if idx == 0 and pending_merge_text is not None:
                should_merge, is_word_split = _detect_continuation(
                    pending_merge_text, text)
                if should_merge and pending_merge_label == element.label:
                    text = _merge_texts(pending_merge_text, text,
                                        is_word_split)
                    pending_merge_text = None
                    pending_merge_label = None
                else:
                    flush_pending()

            # Defer the last paragraph on the page for cross-page merging
            if is_last_on_page and element.label in ("text", "p"):
                flush_pending()
                pending_merge_text = text
                pending_merge_label = element.label
            else:
                md = map_label_to_markdown(element.label, text)
                preview_parts.append(md)
                doc_parts.append(md)

    flush_pending()

    final_preview_md = "\n\n".join(preview_parts)
    final_doc_md = "\n\n".join(doc_parts)

    if output_md_path:
        try:
            with open(output_md_path, "w", encoding="utf-8") as f:
                f.write(final_doc_md)
            logger.info(f"Markdown saved to {output_md_path}")
        except Exception as e:
            logger.error(f"Failed to write markdown: {e}")

    return final_preview_md
