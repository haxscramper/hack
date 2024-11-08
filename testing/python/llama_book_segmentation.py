#!/usr/bin/env python

from pathlib import Path
import ebooklib
from ebooklib import epub
from bs4 import BeautifulSoup
from ollama import Client
import html
import logging
import json


def extract_text(book: epub.EpubBook) -> list[str]:
    chapters = []
    spine = book.spine[0][0]
    nav = book.get_item_with_id(spine)
    toc = book.toc

    for chapter in toc:
        if isinstance(chapter, tuple):
            chapter = chapter[0]
        if not isinstance(chapter, epub.Link):
            continue

        chapter_id = chapter.href.split('#')[0]
        item = book.get_item_with_href(chapter_id)
        if item:
            soup = BeautifulSoup(item.get_content(), "html.parser")
            chapters.append(soup.get_text().strip())

    return chapters


def get_scene_breakdown(text: str, client: Client) -> dict:
    print(len(text))
    function_schema = {
        "name": "analyze_scenes",
        "description": "Analyze text and break it down into scenes and beats",
        "parameters": {
            "type": "object",
            "properties": {
                "scenes": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "summary": {
                                "type": "string",
                                "description": "Brief summary of the scene"
                            },
                            "reason": {
                                "type":
                                "string",
                                "description":
                                "Explanation why this was identified as a separate scene"
                            },
                            "beats": {
                                "type": "array",
                                "items": {
                                    "type": "object",
                                    "properties": {
                                        "summary": {
                                            "type":
                                            "string",
                                            "description":
                                            "Brief summary of the beat"
                                        },
                                        "reason": {
                                            "type":
                                            "string",
                                            "description":
                                            "Explanation why this was identified as a beat"
                                        },
                                        "text": {
                                            "type":
                                            "string",
                                            "description":
                                            "Original text of the beat"
                                        }
                                    },
                                    "required": ["summary", "reason", "text"]
                                }
                            }
                        },
                        "required": ["summary", "reason", "beats"]
                    }
                }
            },
            "required": ["scenes"]
        }
    }

    prompt = f"""Analyze this text and break it down into scenes. For each scene identify individual story beats.
    Keep original text for each beat. Text to analyze:
    
    {text}"""

    print(text)
    response = client.chat(
        model="llama3.2",
        messages=[{
            "role":
            "system",
            "content":
            "You are a literary analyst breaking down text into scenes and beats"
        }, {
            "role": "user",
            "content": prompt
        }],
        tools=[function_schema])

    print(response)
    print(json.dumps(response, indent=2))
    return response["message"]["tool_call"]["arguments"]


def generate_html(breakdown: dict) -> str:
    html_content = """
    <html>
    <head>
        <style>
            .scene { background: #ffcdd2; padding: 10px; margin: 10px 0; }
            .beat { background: #fff9c4; padding: 10px; margin: 5px 0; }
        </style>
    </head>
    <body>
    """

    for chapter in breakdown:
        for scene in chapter["scenes"]:
            html_content += f"""
            <div class="scene">
                <strong>Scene: {html.escape(scene["summary"])}</strong><br>
                Reason: {html.escape(scene["reason"])}
            </div>
            """

            for beat in scene["beats"]:
                html_content += f"""
                <div class="beat">
                    Beat: {html.escape(beat["summary"])}
                </div>
                """

    html_content += "</body></html>"
    return html_content


def main() -> None:
    input_path = Path("/tmp/input.epub")
    output_path = Path("/tmp/scenes.html")

    book = epub.read_epub(str(input_path))
    text = extract_text(book)

    client = Client(host="http://localhost:11434")
    breakdown = []
    breakdown.append(get_scene_breakdown(Path("/tmp/text.txt").read_text(), client))

    html_content = generate_html(breakdown)
    output_path.write_text(html_content)


if __name__ == "__main__":
    main()
