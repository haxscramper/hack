import logging
from typing import List, Optional
import os

from models import PageData, DocTag
from llm_processor import merge_blocks_llm, flag_issues_llm
import ebooklib
from ebooklib import epub

logger = logging.getLogger(__name__)

import re
from dataclasses import dataclass
from typing import List, Optional, Tuple


SENTENCE_TERMINATORS = {'.', '!', '?', '"', '\'', '\u201d', '\u2019'}
SKIPPED_TAGS = {"page_footer"}


@dataclass
class MergeState:
    """Tracks the pending text fragment from the previous page that needs merging."""
    pending_text: Optional[str] = None
    pending_was_hyphenated: bool = False


def _get_page_number_str(page: 'PageData') -> Optional[str]:
    """Extract the page footer text to detect page-number contamination."""
    for tag in _iter_leaf_tags(page.spatial_tags):
        if tag.tag_name == "page_footer":
            t = (tag.user_edited_text if tag.user_edited_text is not None else tag.text or "").strip()
            if t:
                return t
    return None


def _iter_leaf_tags(tags: List['DocTag']):
    """Yield all leaf DocTags depth-first."""
    for tag in tags:
        if tag.children:
            yield from _iter_leaf_tags(tag.children)
        else:
            yield tag


def _get_content_tags(page: 'PageData') -> List['DocTag']:
    """Return the flat list of non-removed, non-footer leaf tags with text, sorted top-to-bottom."""
    results = []
    for tag in _iter_leaf_tags(page.spatial_tags):
        if tag.user_removed:
            continue
        raw_name = tag.user_tag_override or tag.tag_name
        if raw_name in SKIPPED_TAGS:
            continue
        text = tag.user_edited_text if tag.user_edited_text is not None else tag.text
        if not text:
            continue
        results.append(tag)
    # Sort by vertical position
    results.sort(key=lambda t: (t.bbox.y if t.bbox else 0))
    return results


def _strip_trailing_page_number(text: str, page_number_str: Optional[str]) -> str:
    """Remove a page number that OCR glued onto the end of the last text block."""
    if not page_number_str:
        return text
    # e.g. "counter-economy which han112" -> "counter-economy which han"
    if text.rstrip().endswith(page_number_str):
        return text.rstrip()[:-len(page_number_str)]
    return text


def _detect_continuation(last_text: str, first_text: str) -> Tuple[bool, bool]:
    """
    Determine whether two text blocks across a page boundary should be merged.
    
    Returns (should_merge, is_word_split).
    
    is_word_split means the word itself is broken (no space/hyphen at boundary),
    so we concatenate directly. Otherwise we join with a space.
    """
    if not last_text or not first_text:
        return False, False

    last_stripped = last_text.rstrip()
    first_stripped = first_text.lstrip()

    if not last_stripped or not first_stripped:
        return False, False

    last_char = last_stripped[-1]
    first_char = first_stripped[0]

    # Case 1: Word split — last block ends with a letter/digit mid-word,
    # first block starts with a lowercase letter (e.g., "han" / "dles")
    if last_char.isalpha() and first_char.islower():
        # Check that it doesn't look like a completed sentence
        if last_char not in SENTENCE_TERMINATORS:
            return True, True

    # Case 2: Hyphenated word split — last block ends with "-"
    if last_char == '-':
        # Distinguish soft hyphen (word break) from real hyphen (e.g., "semi-hidden")
        # Heuristic: if first_char is lowercase, it's a word-break hyphen
        if first_char.islower():
            return True, True  # We'll strip the hyphen during merge

    # Case 3: Sentence continues — last block doesn't end with sentence-terminal punctuation
    # and first block starts lowercase
    if last_char not in SENTENCE_TERMINATORS and first_char.islower():
        return True, False

    return False, False


def _merge_texts(last_text: str, first_text: str, is_word_split: bool) -> str:
    """Join two text fragments from across a page break."""
    last_stripped = last_text.rstrip()
    first_stripped = first_text.lstrip()

    if is_word_split:
        # If it ends with hyphen that's a word-break, remove the hyphen
        if last_stripped.endswith('-'):
            return last_stripped[:-1] + first_stripped
        # Direct concatenation for mid-word splits like "han" + "dles"
        return last_stripped + first_stripped
    else:
        # Sentence continuation, keep space
        return last_stripped + " " + first_stripped

def map_tag_to_html(tag_name: str) -> str:
    mapping = {
        "section_header": "h2",
        "text": "p",
        "list": "ul",
        "list_item": "li",
        "table": "table",
        "footnote": "p",
        "formula": "p",
        "page_footer": "footer",
        "picture": "div",
        "root": "div",
        "unspecified": "div",
        "h1": "h1",
        "h2": "h2",
        "h3": "h3",
        "p": "p",
    }
    return mapping.get(tag_name, "div")

def generate_html_content(pages_data: List[PageData], output_epub_path: Optional[str] = None) -> str:
    preview_parts = ["<html><head><style>body { font-family: sans-serif; }</style></head><body>"]
    epub_parts = ["<html><head><style>body { font-family: sans-serif; }</style></head><body>"]

    toc_links = []
    header_counter = 0

    # Cross-page merge state: holds the last text block's info if it might continue
    pending_merge_text: Optional[str] = None
    pending_merge_html_tag: Optional[str] = None
    pending_merge_header_id: Optional[str] = None

    def _effective_text(tag: DocTag) -> str:
        return tag.user_edited_text if tag.user_edited_text is not None else (tag.text or "")

    def flush_pending():
        """Emit the pending block — it won't be merged with anything."""
        nonlocal pending_merge_text, pending_merge_html_tag, pending_merge_header_id
        if pending_merge_text is not None:
            id_attr = f' id="{pending_merge_header_id}"' if pending_merge_header_id else ""
            html = f"<{pending_merge_html_tag}{id_attr}>{pending_merge_text}</{pending_merge_html_tag}>"
            preview_parts.append(html)
            epub_parts.append(html)
            pending_merge_text = None
            pending_merge_html_tag = None
            pending_merge_header_id = None

    for page_idx, page in enumerate(pages_data):
        preview_parts.append(f"<hr/><h3>Page {page.page_number}</h3>")

        page_number_str = _get_page_number_str(page)
        content_tags = _get_content_tags(page)

        for tag_idx, tag in enumerate(content_tags):
            raw_tag_name = tag.user_tag_override or tag.tag_name
            html_tag = map_tag_to_html(raw_tag_name)
            text = _effective_text(tag)

            is_last_on_page = (tag_idx == len(content_tags) - 1)

            # Clean page number contamination from last block on page
            if is_last_on_page:
                text = _strip_trailing_page_number(text, page_number_str)

            # If there's a pending block from the previous page, try to merge
            # with the FIRST content tag on this page
            if tag_idx == 0 and pending_merge_text is not None:
                should_merge, is_word_split = _detect_continuation(
                    pending_merge_text, text
                )
                if should_merge and pending_merge_html_tag == html_tag:
                    text = _merge_texts(pending_merge_text, text, is_word_split)
                    # Carry over the header id from the pending block if any
                    header_id = pending_merge_header_id
                    pending_merge_text = None
                    pending_merge_html_tag = None
                    pending_merge_header_id = None
                else:
                    # No merge — flush the pending block as-is, then process current
                    flush_pending()
                    header_id = None
            else:
                header_id = None

            # Compute header ID for TOC if needed
            if html_tag in ("h1", "h2", "h3"):
                if header_id is None:
                    header_counter += 1
                    header_id = f"header_{header_counter}"
                toc_links.append(epub.Link("document.xhtml#" + header_id, text, header_id))

            # If this is the last content block on the page, defer emission
            # so we can try merging with the next page's first block
            if is_last_on_page and html_tag == "p":
                # Only defer paragraph blocks for cross-page merge
                flush_pending()  # flush any prior pending (shouldn't happen, but safety)
                pending_merge_text = text
                pending_merge_html_tag = html_tag
                pending_merge_header_id = header_id
            else:
                id_attr = f' id="{header_id}"' if header_id else ""
                tag_html = f"<{html_tag}{id_attr}>{text}</{html_tag}>"
                preview_parts.append(tag_html)
                epub_parts.append(tag_html)

    # Flush any remaining pending block from the last page
    flush_pending()

    preview_parts.append("</body></html>")
    epub_parts.append("</body></html>")

    final_preview_html = "\n".join(preview_parts)
    final_epub_html = "\n".join(epub_parts)

    if output_epub_path:
        try:
            book = epub.EpubBook()
            book.set_identifier('id123456')
            title = os.path.splitext(os.path.basename(output_epub_path))[0]
            book.set_title(title)
            book.set_language('en')

            c1 = epub.EpubHtml(title='Document', file_name='document.xhtml', lang='en')
            c1.content = final_epub_html

            book.add_item(c1)
            book.toc = list(toc_links)
            book.add_item(epub.EpubNcx())
            book.add_item(epub.EpubNav())

            style = 'BODY { color: white; }'
            nav_css = epub.EpubItem(uid="style_nav", file_name="style/nav.css", media_type="text/css", content=style)
            book.add_item(nav_css)

            book.spine = ['nav', c1]
            epub.write_epub(output_epub_path, book, {})
            logger.info(f"EPUB saved to {output_epub_path}")
        except Exception as e:
            logger.error(f"Failed to generate EPUB: {e}")

    return final_preview_html
