#!/usr/bin/env python

import logging
import re
from datetime import datetime
from pathlib import Path
from beartype import beartype
from beartype.typing import Dict, List, Any, Optional, Tuple
import mistune
import json
import itertools

import click
import pytz
from notion_client import Client

logging.basicConfig(
    format="%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s"
)
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
    return markdown(markdown_text).rstrip("\n")


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

    def fetch_blocks_recursively(block_id: str) -> List[Dict[str, Any]]:
        blocks = []
        start_cursor = None

        while True:
            query_params = {"block_id": block_id}
            if start_cursor:
                query_params["start_cursor"] = start_cursor

            response = notion.blocks.children.list(**query_params)
            results = response.get("results", [])

            for block in results:
                blocks.append(block)
                # Check if block has children
                if block.get("has_children", False):
                    child_blocks = fetch_blocks_recursively(block["id"])
                    block["children"] = child_blocks

                else:
                    block["children"] = []

            next_cursor = response.get("next_cursor")
            if not next_cursor:
                break

            start_cursor = next_cursor

        return blocks

    blocks = fetch_blocks_recursively(page_id)
    Path("/tmp/debug.json").write_text(json.dumps(blocks, indent=2))
    return blocks


@beartype
def convert_to_org_timestamp(dt: datetime) -> str:
    if dt.hour == 0 and dt.minute == 0 and dt.second == 0:
        ftime = dt.strftime("%Y-%m-%d")
        return f"[{ftime} +04]"
    else:
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
        (r"(\w+ \d+, \d{4})", lambda x: datetime.strptime(x, "%b %d, %Y")
         if len(x.split()[0]) <= 3 else datetime.strptime(x, "%B %d, %Y")),

        # Simple date format: 2025-01-04
        (r"(\d{4}-\d{2}-\d{2})(?!T)",
         lambda x: datetime.strptime(x, "%Y-%m-%d")),
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
def convert_blocks_to_org(blocks: List[Dict[str, Any]]) -> str:

    @beartype
    def retext(t: str) -> str:
        return parse_and_replace_datetimes(markdown_to_org(t))

    @beartype
    def aux(block: Dict[str, Any], level: int) -> List[str]:
        res = ""
        block_type = block.get("type")

        if block_type == "paragraph":
            text_content = block.get("paragraph", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                if text.strip():
                    items = retext(text)
                    for item_text in items:
                        res += f"{item_text}"

        elif block_type == "to_do":
            checked = block.get("to_do", {}).get("checked", False)
            text_content = block.get("to_do", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                res += f"- [{'X' if checked else ' '}] {retext(text)}"

        elif block_type == "bulleted_list_item" or block_type == "numbered_list_item":
            text_content = block.get(block_type, {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                res += f"- {retext(text)}"

        elif block_type == "heading_1":
            text_content = block.get("heading_1", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                res += f"** {retext(text)}"

        elif block_type == "heading_2":
            text_content = block.get("heading_2", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                res += f"*** {retext(text)}"

        elif block_type == "heading_3":
            text_content = block.get("heading_3", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                res += f"**** {retext(text)}"

        elif block_type == "code":
            text_content = block.get("code", {}).get("rich_text", [])
            language = block.get("code", {}).get("language", "")
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                res += f"#+BEGIN_SRC {language}\n{text}\n#+END_SRC"

        elif block_type == "quote":
            text_content = block.get("quote", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                text = markdown_to_org(text)
                res += f"#+BEGIN_QUOTE\n{text}\n#+END_QUOTE"

        elif block_type == "toggle":
            text_content = block.get("toggle", {}).get("rich_text", [])
            if text_content:
                text = "".join([t.get("plain_text", "") for t in text_content])
                text = markdown_to_org(text)
                res += f"#+BEGIN_DETAIL\n{text}\n#+END_DETAIL"

        elif block_type == "image":
            res += "!IMAGE!"

        elif block_type == "table":

            def rich_text_to_org(rich_text_list: list) -> str:
                result = ""
                for text_obj in rich_text_list:
                    content = text_obj["plain_text"]
                    annotations = text_obj["annotations"]

                    if annotations["bold"]:
                        content = f"*{content}*"
                    if annotations["italic"]:
                        content = f"/{content}/"
                    if annotations["code"]:
                        content = f"~{content}~"
                    if annotations["strikethrough"]:
                        content = f"+{content}+"

                    if text_obj.get("href"):
                        content = f"[[{text_obj['href']}][{content}]]"

                    result += content
                return result

            rows = []
            for child in block["children"]:
                if child["type"] == "table_row":
                    cells = child["table_row"]["cells"]
                    org_cells = []
                    for cell in cells:
                        cell_content = rich_text_to_org(cell)
                        org_cells.append(cell_content)
                    rows.append("| " + " | ".join(org_cells) + " |")

            if block["table"]["has_column_header"] and rows:
                header_separator = "|" + "|".join([
                    "-" * (len(cell.strip()) + 2)
                    for cell in rows[0].split("|")[1:-1]
                ]) + "|"
                rows.insert(1, header_separator)

            res += "\n".join(rows)

        else:
            raise ValueError(
                f"Unhandled notion block type {block_type} {block}")

        res = "\n".join("  " * level + s for s in res.split("\n"))

        return [res] + list(
            itertools.chain(*[aux(n, level + 1) for n in block["children"]]))

        # nested = convert_blocks_to_org(block["children"])

        # logger.info(f"{nested}")

        # org_content += nested
        # org_content += "\n".join(["  " + s for s in nested.split("\n")])

    out = []
    for b in blocks:
        out += aux(b, 0)

    return "\n".join(out)


@beartype
def note_to_org_subtree(notion: Client, note: Dict[str, Any]) -> str:
    properties = note.get("properties", {})

    # Get note title
    title_prop = properties.get("Task", {}).get("title", [])
    if title_prop:
        title = "".join(t.get("plain_text") for t in title_prop)
    else:
        title = "Untitled note"

    logger.info(f"{title}")
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
    with output_path.open("w", encoding="utf-8") as f:
        for note in notes:
            org_content = note_to_org_subtree(notion, note)
            f.write(org_content + "\n\n")

    logger.info(f"Notes exported to {output_path}")


if __name__ == "__main__":
    main()
