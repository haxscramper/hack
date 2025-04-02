#!/usr/bin/env python

import logging
import re
from datetime import datetime
from pathlib import Path
from beartype import beartype
from beartype.typing import Dict, List, Any, Optional, Tuple
import mistune

import click
import pytz
from notion_client import Client

logging.basicConfig(format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)


@beartype
class OrgModeRenderer(mistune.HTMLRenderer):

    def __init__(self):
        super().__init__()
        self.list_level = 0

    def text(self, text):
        return text

    def link(self, link, title, text):
        return f"[[{link}][{text}]]"

    def emphasis(self, text):
        return f"/{text}/"

    def strong(self, text):
        return f"*{text}*"

    def codespan(self, text):
        return f"~{text}~"

    def strikethrough(self, text):
        return f"+{text}+"

    def paragraph(self, text):
        return f"{text}\n\n"

    def heading(self, text, level):
        prefix = "*" * level
        return f"{prefix} {text}\n\n"

    def list(self, body, ordered):
        self.list_level -= 1
        return body

    def list_item(self, text):
        prefix = "-" * (self.list_level + 1)
        return f"{prefix} {text.strip()}\n"

    def block_code(self, code, info=None):
        lang = info or ""
        return f"#+BEGIN_SRC {lang}\n{code}\n#+END_SRC\n\n"

    def block_quote(self, text):
        return f"#+BEGIN_QUOTE\n{text}#+END_QUOTE\n\n"

    def finalize(self, data):
        return data.rstrip()


@beartype
def markdown_to_org(text: str) -> str:
    # Special handling for checkboxes before passing to mistune
    checkbox_pattern = r"- $$([ xX])$$\s+(.*)"
    text = text.strip('\n')
    lines = []

    for line in text.split("\n"):
        checkbox_match = re.match(checkbox_pattern, line)
        if checkbox_match:
            checked = checkbox_match.group(1).upper() == "X"
            content = checkbox_match.group(2)
            lines.append(f"- [{'X' if checked else ' '}] {content}")
        else:
            lines.append(line)

    # Convert non-checkbox markdown to org using mistune
    markdown_text = "\n".join(lines)
    markdown = mistune.create_markdown(renderer=OrgModeRenderer())
    return markdown(markdown_text)


@beartype
def get_notes_from_notion(token: str, database_id: str,
                          max_notes: int) -> List[Dict[str, Any]]:
    notion = Client(auth=token)
    notes = []
    start_cursor = None

    while True:
        query_params = {
            "database_id": database_id,
            "sorts": [{
                "property": "Due Date",
                "direction": "ascending"
            }]
        }

        if start_cursor:
            query_params["start_cursor"] = start_cursor

        response = notion.databases.query(**query_params)
        results = response.get("results", [])

        for page in results:
            notes.append(page)
            if max_notes > 0 and len(notes) >= max_notes:
                return notes

        next_cursor = response.get("next_cursor")
        if not next_cursor:
            break

        start_cursor = next_cursor

    logger.info(f"Retrieved {len(notes)} notes from Notion database")
    return notes


@beartype
def get_note_content(notion: Client, page_id: str) -> List[Dict[str, Any]]:
    blocks = []
    start_cursor = None

    while True:
        query_params = {"block_id": page_id}
        if start_cursor:
            query_params["start_cursor"] = start_cursor

        response = notion.blocks.children.list(**query_params)
        blocks.extend(response.get("results", []))

        next_cursor = response.get("next_cursor")
        if not next_cursor:
            break

        start_cursor = next_cursor

    return blocks


@beartype
def convert_to_org_timestamp(dt: datetime) -> str:
    utc_dt = dt.astimezone(pytz.UTC)
    ftime = utc_dt.strftime("%Y-%m-%d %a %H:%M:%S")
    return f"[{ftime} +04]"


@beartype
def parse_and_replace_datetimes(text: str) -> str:
    patterns = [
        # ISO format with timezone: 2025-01-04T12:48:00.000+04:00
        (r"(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d{3}[+-]\d{2}:\d{2})",
         lambda x: datetime.fromisoformat(x)),

        # Month day, year hour:minute AM/PM: March 11, 2025 12:49 PM
        (r"(\w+ \d+, \d{4} \d+:\d+ [AP]M)",
         lambda x: datetime.strptime(x, "%B %d, %Y %I:%M %p")),

        # Month day, year: March 11, 2025
        (r"(\w+ \d+, \d{4})", lambda x: datetime.strptime(x, "%B %d, %Y"))
    ]

    result = text
    for pattern, parser in patterns:
        matches = re.findall(pattern, text)
        for match in matches:
            dt = parser(match)
            org_timestamp = convert_to_org_timestamp(dt)
            result = result.replace(match, org_timestamp)

    return result


@beartype
def process_text_block(text: str) -> List[str]:
    items = []

    # Convert markdown to org-mode
    text = markdown_to_org(text)

    # Handle checkbox items
    checkbox_pattern = r"- $$([ xX])$$\s+(.*)"
    for line in text.split("\n"):
        line = line.strip()
        if not line:
            continue

        checkbox_match = re.match(checkbox_pattern, line)
        if checkbox_match:
            checked = checkbox_match.group(1).upper() == "X"
            content = checkbox_match.group(2)
            content = parse_and_replace_datetimes(content)
            items.append(f"- [{'X' if checked else ' '}] {content}")
        else:
            line = parse_and_replace_datetimes(line)
            items.append(f"- {line}")

    return items


@beartype
def convert_blocks_to_org(blocks: List[Dict[str, Any]]) -> str:
    org_content = ""

    for block in blocks:
        block_type = block.get("type")

        # org_content += f"block-type {block_type}"

        if block_type == "paragraph":
            text_content = block.get("paragraph", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                if text.strip():
                    items = process_text_block(text)
                    for item_text in items:
                        org_content += f"{item_text}\n"

        elif block_type == "to_do":
            checked = block.get("to_do", {}).get("checked", False)
            text_content = block.get("to_do", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                text = markdown_to_org(text)
                text = parse_and_replace_datetimes(text)
                org_content += f"- [{'X' if checked else ' '}] {text}\n"

        elif block_type == "bulleted_list_item" or block_type == "numbered_list_item":
            text_content = block.get(block_type, {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                text = markdown_to_org(text)
                text = parse_and_replace_datetimes(text)
                org_content += f"- {text}\n"

        elif block_type == "heading_1":
            text_content = block.get("heading_1", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                org_content += f"* {text}\n"

        elif block_type == "heading_2":
            text_content = block.get("heading_2", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                org_content += f"** {text}\n"

        elif block_type == "heading_3":
            text_content = block.get("heading_3", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                org_content += f"*** {text}\n"

        elif block_type == "code":
            text_content = block.get("code", {}).get("rich_text", [])
            language = block.get("code", {}).get("language", "")
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                org_content += f"#+BEGIN_SRC {language}\n{text}\n#+END_SRC\n"

        elif block_type == "quote":
            text_content = block.get("quote", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                text = markdown_to_org(text)
                org_content += f"#+BEGIN_QUOTE\n{text}\n#+END_QUOTE\n"

        else:
            logger.error(f"Unhandled notion block type {block_type} {block}")

    return org_content


@beartype
def note_to_org_subtree(notion: Client, note: Dict[str, Any]) -> str:
    properties = note.get("properties", {})

    # Get note title
    title_prop = properties.get("Task", {}).get("title", [])
    if title_prop:
        title = "".join(t.get("plain_text") for t in title_prop)
    else:
        title = "Untitled note"

    title = parse_and_replace_datetimes(title)

    # Get due date
    due_date = None
    due_date_prop = properties.get("Due Date").get("date", {})
    if due_date_prop:
        due_date_str = due_date_prop.get("start")
        if due_date_str:
            due_date = datetime.fromisoformat(
                due_date_str.replace("Z", "+00:00"))

    # Create org header
    org_content = f"* {title}\n"
    if due_date:
        org_timestamp = convert_to_org_timestamp(due_date)
        org_content += f"SCHEDULED: {org_timestamp}\n"

    # Get and convert note content
    blocks = get_note_content(notion, note["id"])
    org_content += convert_blocks_to_org(blocks)

    return org_content


@click.command()
@click.option("--outfile",
              required=True,
              type=click.Path(),
              help="Output org-mode file")
@click.option("--notion_database", required=True, help="Notion database ID")
@click.option("--notion_token", required=True, help="Notion API token")
@click.option("--max_notes",
              default=-1,
              help="Maximum number of notes to process (-1 for unlimited)")
def main(outfile: str, notion_database: str, notion_token: str,
         max_notes: int) -> None:
    notion = Client(auth=notion_token)
    output_path = Path(outfile)

    # Get notes from Notion
    notes = get_notes_from_notion(notion_token, notion_database, max_notes)
    logger.info(f"Processing {len(notes)} notes")

    # Convert notes to org-mode and append to file
    with output_path.open("a", encoding="utf-8") as f:
        for note in notes:
            org_content = note_to_org_subtree(notion, note)
            f.write(org_content + "\n")

    logger.info(f"Notes exported to {output_path}")


if __name__ == "__main__":
    main()
