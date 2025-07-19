#!/usr/bin/env python

import asyncio
import logging
import re
from pathlib import Path
from typing import Dict, List, Tuple
from urllib.parse import urljoin, urlparse

import aiohttp
from bs4 import BeautifulSoup
from ebooklib import epub
from dataclasses import dataclass
import hashlib

logging.basicConfig(
    level=logging.INFO,
    format=
    "%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s",
)
logger = logging.getLogger(__name__)


@dataclass
class ChapterInfo:
    title: str
    url: str
    arc_name: str
    arc_url: str
    is_interlude: bool
    order: int


class WormScraper:

    def __init__(self) -> None:
        self.base_url = "https://worm.fandom.com"
        self.session: aiohttp.ClientSession = None

    async def __aenter__(self) -> "WormScraper":
        self.session = aiohttp.ClientSession()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb) -> None:
        if self.session:
            await self.session.close()

    async def fetch_page(self, url: str) -> str:
        cache_dir = Path("/tmp/worm_download")
        cache_dir.mkdir(exist_ok=True)

        url_hash = hashlib.md5(url.encode()).hexdigest()
        cache_file = cache_dir / f"{url_hash}.html"

        if cache_file.exists():
            logger.info(f"Loading from cache: {url}")
            return cache_file.read_text(encoding="utf-8")

        # logger.info(f"Fetching: {url}")
        async with self.session.get(url) as response:
            response.raise_for_status()
            content = await response.text()

        cache_file.write_text(content, encoding="utf-8")
        await asyncio.sleep(2)
        return content

    async def get_chapter_list(self) -> List[ChapterInfo]:
        # url = f"{self.base_url}/wiki/Worm_Chapter_List"
        url = f"{self.base_url}/wiki/Ward_Chapter_List"
        html = await self.fetch_page(url)
        soup = BeautifulSoup(html, "html.parser")

        chapters = []
        order = 0

        content_div = soup.find("div", {"class": "mw-parser-output"})
        table = content_div.find("table")

        for td in table.find_all("td"):
            h3 = td.find("h3")
            if not h3:
                continue

            span = h3.find("span", {"class": "mw-headline"})
            if not span:
                continue

            arc_link = span.find("a")
            if arc_link:
                arc_name = arc_link.get_text().strip()
                arc_url = urljoin(self.base_url, arc_link.get("href"))
            else:
                arc_name = span.get_text().strip()
                arc_url = ""

            ul = td.find("ul")
            if ul:
                for li in ul.find_all("li"):
                    chapter_link = li.find("a")
                    if chapter_link:
                        chapter_title = chapter_link.get_text().strip()
                        chapter_url = urljoin(self.base_url,
                                              chapter_link.get("href"))
                        is_interlude = "interlude" in chapter_title.lower()

                        chapters.append(
                            ChapterInfo(title=chapter_title,
                                        url=chapter_url,
                                        arc_name=arc_name,
                                        arc_url=arc_url,
                                        is_interlude=is_interlude,
                                        order=order))
                        order += 1

        return chapters

    async def get_page_content(self, url: str) -> tuple[str, str]:
        html = await self.fetch_page(url)
        soup = BeautifulSoup(html, "html.parser")
        
        title_element = soup.find("h1", {"class": "page-header__title"})
        title = title_element.get_text().strip() if title_element else "Unknown"
        
        content_div = soup.find("div", {"class": "mw-parser-output"})
        if not content_div:
            return title, ""
        
        content_parts = []
        
        for element in content_div.find_all(["h2", "h3", "h4", "h5", "h6", "p"]):
            if element.name in ["h2", "h3", "h4", "h5", "h6"]:
                span = element.find("span", {"class": "mw-headline"})
                if span:
                    header_text = span.get_text().strip()
                    if header_text.lower() in ["references", "navigation", "site navigation"]:
                        break
                    level = int(element.name[1])
                    content_parts.append(f"<h{level + 1}>{header_text}</h{level + 1}>")
            elif element.name == "p":
                text = element.get_text().strip()
                if text:
                    content_parts.append(f"<p>{text}</p>")
        
        content = "\n".join(content_parts)
        return title, content
async def create_epub(output_path: Path) -> None:
    async with WormScraper() as scraper:
        logger.info("Fetching chapter list...")
        chapters = await scraper.get_chapter_list()
        
        book = epub.EpubBook()
        book.set_identifier("ward-novel")
        book.set_title("ward summary")
        book.add_author("Wildbow")
        book.set_language("en")
        
        spine = ["nav"]
        toc = []
        
        current_arc = None
        arc_chapters = []
        processed_arcs = set()
        
        for chapter in chapters:
            if chapter.arc_name != current_arc:
                if arc_chapters:
                    toc.append((epub.Section(current_arc), arc_chapters.copy()))
                    arc_chapters.clear()
                
                current_arc = chapter.arc_name
                
                if chapter.arc_url and chapter.arc_name not in processed_arcs:
                    try:
                        logger.info(f"Processing arc summary: {chapter.arc_name}")
                        arc_summary_title, arc_summary_content = await scraper.get_page_content(chapter.arc_url)
                        
                        arc_chapter = epub.EpubHtml(
                            title=arc_summary_title,
                            file_name=f"arc_{len(toc)}_summary.xhtml",
                            lang="en"
                        )
                        arc_chapter.content = f"""
                        <html>
                        <head><title>{arc_summary_title}</title></head>
                        <body>
                        <h1>{arc_summary_title}</h1>
                        {arc_summary_content}
                        </body>
                        </html>
                        """
                        
                        book.add_item(arc_chapter)
                        spine.append(arc_chapter)
                        arc_chapters.append(arc_chapter)
                        processed_arcs.add(chapter.arc_name)
                    except Exception as e:
                        logger.error(f"Failed to fetch arc summary for {chapter.arc_name}: {e}")
            
            try:
                logger.info(f"Fetching chapter: {chapter.title}")
                title, content = await scraper.get_page_content(chapter.url)
                
                epub_chapter = epub.EpubHtml(
                    title=title,
                    file_name=f"chapter_{chapter.order}.xhtml",
                    lang="en"
                )
                epub_chapter.content = f"""
                <html>
                <head><title>{title}</title></head>
                <body>
                <h1>{title}</h1>
                {content}
                </body>
                </html>
                """
                
                book.add_item(epub_chapter)
                spine.append(epub_chapter)
                arc_chapters.append(epub_chapter)
                
            except Exception as e:
                logger.error(f"Failed to fetch chapter {chapter.title}: {e}")
        
        if arc_chapters:
            toc.append((epub.Section(current_arc), arc_chapters))
        
        book.toc = toc
        book.spine = spine
        
        book.add_item(epub.EpubNcx())
        book.add_item(epub.EpubNav())
        
        logger.info(f"Writing EPUB to {output_path}")
        epub.write_epub(str(output_path), book)


async def main() -> None:
    output_path = Path("/tmp/ward_summary.epub")
    await create_epub(output_path)
    logger.info(f"EPUB created successfully: {output_path}")


if __name__ == "__main__":
    asyncio.run(main())
