#!/usr/bin/env python

import json
import logging
import hashlib
import asyncio
import aiohttp
from pathlib import Path
from beartype.typing import Dict, List, Optional, Union
from beartype import beartype
from urllib.parse import urljoin, urlparse
from dataclasses import dataclass
from pydantic import BaseModel, Field
from ebooklib import epub
from bs4 import BeautifulSoup
import re
from pprint import pprint, pformat

logging.basicConfig(
    level=logging.DEBUG,
    format="%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s"
)


class PageConfig(BaseModel):
    title: str
    url: Optional[str] = None
    order: int = -1
    children: Optional[List["PageConfig"]] = None
    depth: int = 0
    output_file: Optional[str] = None


class BookConfig(BaseModel):
    title: str
    author: str
    base_url: str
    pages: List[PageConfig]
    with_images: bool
    cache_dir: str = "cache"
    output_file: str = "book.epub"


PageConfig.model_rebuild()


@dataclass
class ProcessedPage:
    url: str
    title: str
    content: str
    images: List[str]
    order: int


@dataclass
class TocEntry:
    title: str
    level: int
    anchor: str
    page_id: str


class EpubGenerator:

    def __init__(self, config: BookConfig):
        self.config = config
        self.cache_dir = Path(config.cache_dir)
        self.cache_dir.mkdir(exist_ok=True)
        self.session: Optional[aiohttp.ClientSession] = None

    async def __aenter__(self):
        self.session = aiohttp.ClientSession()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        if self.session:
            await self.session.close()

    def _get_cache_path(self, url: str) -> Path:
        url_hash = hashlib.md5(url.encode()).hexdigest()
        return self.cache_dir / f"{url_hash}.html"

    def _get_error_cache_path(self, url: str) -> Path:
        url_hash = hashlib.md5(url.encode()).hexdigest()
        return self.cache_dir / f"{url_hash}.error"

    def _get_image_cache_path(self, url: str) -> Path:
        url_hash = hashlib.md5(url.encode()).hexdigest()
        parsed = urlparse(url)
        ext = Path(parsed.path).suffix or ".jpg"
        return self.cache_dir / f"img_{url_hash}{ext}"

    async def _download_content(self, url: str) -> str:
        cache_path = self._get_cache_path(url)
        error_cache_path = self._get_error_cache_path(url)

        if cache_path.exists():
            return cache_path.read_text(encoding="utf-8")

        if error_cache_path.exists():
            raise Exception(f"Previously failed to download {url}")

        try:
            logging.debug(f"Downloading {url}")
            async with self.session.get(url) as response:
                response.raise_for_status()
                content = await response.text()

            cache_path.write_text(content, encoding="utf-8")
            return content
        except Exception as e:
            error_cache_path.write_text(str(e), encoding="utf-8")
            logging.error(f"Failed to download {url}: {e}")
            raise

    async def _download_image(self, url: str) -> bytes:
        cache_path = self._get_image_cache_path(url)
        error_cache_path = self._get_error_cache_path(url)

        if cache_path.exists():
            logging.debug(f"Loading cached image for {url}")
            return cache_path.read_bytes()

        if error_cache_path.exists():
            logging.debug(
                f"Skipping image {url} due to previous download failure")
            raise Exception(f"Previously failed to download image {url}")

        try:
            logging.debug(f"Downloading image {url}")
            async with self.session.get(url) as response:
                response.raise_for_status()
                content = await response.read()

            cache_path.write_bytes(content)
            return content
        except Exception as e:
            error_cache_path.write_text(str(e), encoding="utf-8")
            logging.error(f"Failed to download image {url}: {e}")
            raise

    def _process_links(self, soup: BeautifulSoup, base_url: str,
                       url_mapping: Dict[str,
                                         str], book_base_url: str) -> None:
        for link in soup.find_all("a", href=True):
            href = link["href"]
            full_url = urljoin(base_url, href)

            if full_url in url_mapping:
                link["href"] = url_mapping[full_url]
            elif full_url.startswith(book_base_url):
                link["href"] = f"#{hashlib.md5(full_url.encode()).hexdigest()}"

    def _extract_images(self, soup: BeautifulSoup, base_url: str) -> List[str]:
        images = []
        for img in soup.find_all("img", src=True):
            src = img["src"]
            full_url = urljoin(base_url, src)
            images.append(full_url)

            img_hash = hashlib.md5(full_url.encode()).hexdigest()
            parsed = urlparse(full_url)
            ext = Path(parsed.path).suffix or ".jpg"
            img_filename = f"img_{img_hash}{ext}"
            img["src"] = f"images/{img_filename}"

        return images

    def _clean_content(self, soup: BeautifulSoup) -> str:
        return str(soup)

    def _generate_anchor(self, title: str) -> str:
        anchor = re.sub(r"[^\w\s-]", "", title.lower())
        anchor = re.sub(r"[-\s]+", "-", anchor)
        return anchor.strip("-")

    def _extract_toc_entries(self, soup: BeautifulSoup, page_id: str,
                             base_depth: int) -> List[TocEntry]:
        toc_entries = []
        heading_tags = soup.find_all(re.compile(r"^h[1-6]$"))

        for heading in heading_tags:
            level = int(heading.name[1]) + base_depth
            title = heading.get_text().strip()

            if not heading.get("id"):
                anchor = self._generate_anchor(title)
                heading["id"] = anchor
            else:
                anchor = heading["id"]

            toc_entries.append(
                TocEntry(title=title,
                         level=level,
                         anchor=anchor,
                         page_id=page_id))

        return toc_entries

    async def _process_page(self, page_config: PageConfig, depth: int,
                            base_url: str,
                            url_mapping: Dict[str, str]) -> ProcessedPage:
        full_url = urljoin(base_url, page_config.url)
        content = await self._download_content(full_url)

        soup = BeautifulSoup(content, "html.parser")
        images = self._extract_images(soup, full_url)
        self._process_links(soup, full_url, url_mapping, base_url)

        content_soup = BeautifulSoup(self._clean_content(soup), "html.parser")
        clean_content = content_soup.prettify()

        processed_page = ProcessedPage(url=full_url,
                                       title=page_config.title,
                                       content=clean_content,
                                       images=images,
                                       order=page_config.order)
        processed_page.depth = depth
        return processed_page

    def _flatten_pages(self,
                       pages: List[PageConfig],
                       depth: int = 0) -> List[PageConfig]:
        flattened = []
        order = 0
        for page in pages:
            page_copy = PageConfig(url=page.url,
                                   title=page.title,
                                   order=order,
                                   children=None)
            order += 1
            page_copy.depth = depth
            flattened.append(page_copy)
            if page.children:
                flattened.extend(self._flatten_pages(page.children, depth + 1))
        return flattened

    def _build_toc_structure(self, toc_entries: List[TocEntry]) -> List:
        if not toc_entries:
            return []

        result = []
        stack = []

        for entry in toc_entries:
            link = epub.Link(f"{entry.page_id}.xhtml#{entry.anchor}",
                             entry.title, entry.anchor)

            while stack and stack[-1]["level"] >= entry.level:
                stack.pop()

            item = {"level": entry.level, "link": link, "children": []}

            if not stack:
                result.append(item)
            else:
                stack[-1]["children"].append(item)

            stack.append(item)

        def convert_to_epub_format(items):
            epub_items = []
            for item in items:
                if item["children"]:
                    children = convert_to_epub_format(item["children"])
                    epub_items.append((item["link"], children))
                else:
                    epub_items.append(item["link"])
            return epub_items

        return convert_to_epub_format(result)

    async def generate_epub(self, book_config: BookConfig) -> None:
        flat_pages = self._flatten_pages(book_config.pages)
        digest_list = set()
        url_mapping = {}

        dbg_path = Path(self.cache_dir).joinpath(
            Path(book_config.output_file).name + "debug").with_suffix(".py")
        logging.info(f"debug to {dbg_path}")
        dbg_path.write_text("# pyright: reportUndefinedVariable=false\n" +
                            pformat(flat_pages))

        for page in flat_pages:
            full_url = urljoin(book_config.base_url, page.url)
            digest = hashlib.md5(full_url.encode()).hexdigest()
            if digest in digest_list:
                raise ValueError(f"{full_url} is already used for {digest}")

            digest_list.add(digest)

            chapter_id = f"chapter_{digest}"
            url_mapping[full_url] = f"{chapter_id}.xhtml"

        processed_pages = []
        all_toc_entries = []

        for page in flat_pages:
            depth = getattr(page, 'depth', 0)
            processed_page = await self._process_page(page, depth,
                                                      book_config.base_url,
                                                      url_mapping)
            processed_pages.append(processed_page)

            full_url = urljoin(book_config.base_url, page.url)
            chapter_id = f"chapter_{hashlib.md5(full_url.encode()).hexdigest()}"

            page_toc_entry = TocEntry(title=page.title,
                                      level=depth + 1,
                                      anchor=chapter_id,
                                      page_id=chapter_id)
            all_toc_entries.append(page_toc_entry)

            soup = BeautifulSoup(processed_page.content, "html.parser")
            content_toc_entries = self._extract_toc_entries(
                soup, chapter_id, depth + 1)
            all_toc_entries.extend(content_toc_entries)

            processed_page.content = str(soup)

        processed_pages.sort(key=lambda x: x.order)

        all_images = []
        for page in processed_pages:
            all_images.extend(page.images)

        unique_images = list(set(all_images))
        image_data = {}
        if book_config.with_images:
            for img_url in unique_images:
                try:
                    img_data = await self._download_image(img_url)
                    image_data[img_url] = img_data
                except Exception as e:
                    pass

        book = epub.EpubBook()
        book.set_identifier("atomic_rockets_book")
        book.set_title(book_config.title)
        book.set_language("en")
        book.add_author(book_config.author)

        chapters = []
        for page in processed_pages:
            chapter_id = f"chapter_{hashlib.md5(page.url.encode()).hexdigest()}"
            chapter = epub.EpubHtml(title=page.title,
                                    file_name=f"{chapter_id}.xhtml",
                                    lang="en")

            depth = getattr(page, 'depth', 0)
            heading_level = min(depth + 1, 6)
            chapter.content = f"<h{heading_level} id=\"{chapter_id}\">{page.title}</h{heading_level}>{page.content}"

            book.add_item(chapter)
            chapters.append(chapter)

        for img_url, img_data in image_data.items():
            img_hash = hashlib.md5(img_url.encode()).hexdigest()
            parsed = urlparse(img_url)
            ext = Path(parsed.path).suffix or ".jpg"
            img_filename = f"img_{img_hash}{ext}"

            media_type = "image/jpeg"
            if ext.lower() in [".png"]:
                media_type = "image/png"
            elif ext.lower() in [".gif"]:
                media_type = "image/gif"
            elif ext.lower() in [".svg"]:
                media_type = "image/svg+xml"

            img_item = epub.EpubImage()
            img_item.file_name = f"images/{img_filename}"
            img_item.media_type = media_type
            img_item.content = img_data
            book.add_item(img_item)

        toc_structure = self._build_toc_structure(all_toc_entries)
        book.toc = toc_structure

        book.add_item(epub.EpubNcx())
        book.add_item(epub.EpubNav())

        book.spine = ["nav"] + chapters

        word_count, image_count = self._get_statistics(processed_pages)

        output_path = Path(book_config.output_file)
        if not output_path.is_absolute():
            output_path = Path(book_config.cache_dir).joinpath(output_path)

        epub.write_epub(str(output_path), book)
        logging.info(
            f"EPUB generated: {output_path} with {word_count} words and {image_count} images"
        )

    def _get_statistics(
            self, processed_pages: List[ProcessedPage]) -> tuple[int, int]:
        total_words = 0
        total_images = 0

        for page in processed_pages:
            soup = BeautifulSoup(page.content, "html.parser")
            text_content = soup.get_text()
            word_count = len(text_content.split())
            image_count = len(page.images)

            total_words += word_count
            total_images += image_count

        return total_words, total_images

    def _collect_book_configs(
        self,
        pages: List[PageConfig],
        base_config: BookConfig,
        parent_book_config: Optional[BookConfig] = None
    ) -> tuple[List[BookConfig], List[PageConfig]]:
        book_configs = []
        remaining_pages = []
        for page in pages:
            if page.output_file:
                if page.children:
                    book_config = BookConfig(
                        title=page.title,
                        author=base_config.author,
                        base_url=base_config.base_url,
                        pages=page.children,
                        with_images=base_config.with_images,
                        cache_dir=base_config.cache_dir,
                        output_file=page.output_file)
                    book_configs.append(book_config)

                    child_book_configs, leftover_children = self._collect_book_configs(
                        page.children, base_config, book_config)
                    book_configs.extend(child_book_configs)

                    if leftover_children:
                        book_config.pages.extend(leftover_children)

                if page.url:
                    page_copy = PageConfig(title=page.title,
                                           url=page.url,
                                           order=page.order,
                                           children=None,
                                           depth=page.depth)
                    if parent_book_config:
                        parent_book_config.pages.append(page_copy)
                    else:
                        remaining_pages.append(page_copy)
            else:
                if page.children:
                    child_book_configs, filtered_children = self._collect_book_configs(
                        page.children, base_config, parent_book_config)
                    book_configs.extend(child_book_configs)

                    if filtered_children:
                        page_copy = PageConfig(title=page.title,
                                               url=page.url,
                                               order=page.order,
                                               children=filtered_children,
                                               depth=page.depth)
                        if parent_book_config:
                            parent_book_config.pages.append(page_copy)
                        else:
                            remaining_pages.append(page_copy)
                    elif page.url:
                        page_copy = PageConfig(title=page.title,
                                               url=page.url,
                                               order=page.order,
                                               children=None,
                                               depth=page.depth)
                        if parent_book_config:
                            parent_book_config.pages.append(page_copy)
                        else:
                            remaining_pages.append(page_copy)
                else:
                    page_copy = PageConfig(title=page.title,
                                           url=page.url,
                                           order=page.order,
                                           children=None,
                                           depth=page.depth)
                    if parent_book_config:
                        parent_book_config.pages.append(page_copy)
                    else:
                        remaining_pages.append(page_copy)

        return book_configs, remaining_pages

    async def generate_books(self) -> None:
        book_configs, remaining_pages = self._collect_book_configs(
            self.config.pages, self.config)

        for book_config in book_configs:
            logging.info(
                f"Generating book: {book_config.title} -> {book_config.output_file}"
            )
            await self.generate_epub(book_config)

        if remaining_pages:
            logging.info(
                f"Generating root book: {self.config.title} -> {self.config.output_file}"
            )
            root_config = BookConfig(title=self.config.title,
                                     author=self.config.author,
                                     base_url=self.config.base_url,
                                     pages=remaining_pages,
                                     with_images=self.config.with_images,
                                     cache_dir=self.config.cache_dir,
                                     output_file=self.config.output_file)
            await self.generate_epub(root_config)


import sys


async def main() -> None:
    config_path = Path(sys.argv[1])

    if not config_path.exists():
        logging.error(f"Configuration file {config_path} not found")
        return

    config_data = json.loads(config_path.read_text())
    config = BookConfig(**config_data)

    async with EpubGenerator(config) as generator:
        await generator.generate_books()


if __name__ == "__main__":
    asyncio.run(main())
