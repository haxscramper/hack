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
from rich.console import Console
from rich.pretty import pprint
from PIL import Image
import io

logging.basicConfig(
    level=logging.DEBUG,
    format=
    "%(asctime)s %(name)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s"
)

logging.getLogger("asyncio").setLevel(logging.WARNING)
logging.getLogger("PIL").setLevel(logging.WARNING)
logging.getLogger("PIL.PngImagePlugin").setLevel(logging.WARNING)
logging.getLogger("PIL.TiffImagePlugin").setLevel(logging.WARNING)


def render_rich_pprint(
    obj,
    width: int = 150,
    color: bool = False,
    max_string: int | None = None,
) -> str:

    console = Console(record=True, width=width, force_terminal=color)
    with console.capture() as capture:
        pprint(
            obj,
            console=console,
            max_length=width,
            max_string=max_string,
            indent_guides=False,
        )
    return capture.get()


class PageConfig(BaseModel):
    title: str
    url: Optional[str] = None
    order: int = -1
    children: List["PageConfig"] = Field(default_factory=list)
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
    force_optimize_image: bool = False
    use_optimized_image: bool = True


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

    def _optimize_image(self, content: bytes) -> bytes:
        with Image.open(io.BytesIO(content)) as img:
            width, height = img.size
            file_size_kb = len(content) / 1024

            if file_size_kb <= 50 and width <= 480 and height <= 320:
                return content

            max_dimension = 1920

            if width > height:
                new_width = min(width, max_dimension)
                new_height = int((height * new_width) / width)
            else:
                new_height = min(height, max_dimension)
                new_width = int((width * new_height) / height)

            img_resized = img.resize((new_width, new_height),
                                     Image.Resampling.LANCZOS)

            output = io.BytesIO()
            format_name = img.format or "JPEG"
            quality = 85

            if file_size_kb > 2000:
                quality = 60
            elif file_size_kb > 1000:
                quality = 70

            img_resized.save(output,
                             format=format_name,
                             quality=quality,
                             optimize=True)
            tmp = output.getvalue()
            # logging.info(f"Downscaled from {len(content)} to {len(tmp)}, {int(float(len(tmp)) / float(len(content)) * 100)}%")
            if len(tmp) < len(content):
                return tmp

            else:
                return content

    def _get_image_cache_path(self, url: str) -> Path:
        url_hash = hashlib.md5(url.encode()).hexdigest()
        parsed = urlparse(url)
        ext = Path(parsed.path).suffix or ".jpg"
        return self.cache_dir / f"img_{url_hash}{ext}"

    async def _download_image(self, url: str) -> Optional[bytes]:
        cache_path = self._get_image_cache_path(url)
        optimized_cache_path = cache_path.with_suffix(
            f".opt{cache_path.suffix}")
        error_cache_path = self._get_error_cache_path(url)

        if error_cache_path.exists():
            # logging.warning(f"Previously failed to download image {url}")
            return None

        if not cache_path.exists():
            try:
                logging.debug(f"Downloading image {url}")
                async with self.session.get(url) as response:
                    response.raise_for_status()
                    content = await response.read()
                cache_path.write_bytes(content)
            except Exception as e:
                error_cache_path.write_text(str(e), encoding="utf-8")
                logging.error(f"Failed to download image {url}: {e}")
                return None

        if self.config.use_optimized_image:
            if self.config.force_optimize_image or not optimized_cache_path.exists(
            ):
                original_content = cache_path.read_bytes()
                try: 
                    optimized_content = self._optimize_image(original_content)
                    optimized_cache_path.write_bytes(optimized_content)
                    return optimized_content
                    
                except Image.UnidentifiedImageError:
                    return original_content
                    
            else:
                return optimized_cache_path.read_bytes()

        else:
            return cache_path.read_bytes()

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
            if page.url:
                page_copy = page.model_copy(update=dict(order=order, children=[]))
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

    def pprint_cache(self, value, book_config: BookConfig,
                     name: str | Path) -> None:
        dbg_name = Path(book_config.output_file).stem + "_" + name + "_debug"
        dbg_path = Path(self.cache_dir).joinpath(dbg_name).with_suffix(".py")
        logging.info(f"debug to {dbg_path}")
        dbg_path.write_text("# pyright: reportUndefinedVariable=false\n" +
                            render_rich_pprint(value))

    async def generate_epub(self,
                            book_config: BookConfig) -> tuple[str, int, int]:
        flat_pages = self._flatten_pages(book_config.pages)
        digest_list = set()
        url_mapping = {}

        for page in flat_pages:
            full_url = urljoin(book_config.base_url, page.url)
            digest = hashlib.md5(full_url.encode()).hexdigest()
            if digest in digest_list:
                raise ValueError(
                    f"{full_url} is already used for {digest}")

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
                img_data = await self._download_image(img_url)
                image_data[img_url] = img_data

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

        if output_path.exists():
            output_path.unlink()

        epub.write_epub(str(output_path), book)
        return output_path, word_count, image_count

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
    ) -> tuple[List[BookConfig], List[PageConfig]]:
        book_configs = []


        def aux(pages: List[PageConfig]) -> List[PageConfig]:
            result = []
            for page in pages:
                if page.output_file:
                    book_config = BookConfig(
                        title=page.title,
                        author=base_config.author,
                        base_url=base_config.base_url,
                        pages=[],
                        with_images=base_config.with_images,
                        cache_dir=base_config.cache_dir,
                        output_file=page.output_file)

                    leftover_children = aux(page.children)

                    page_copy = page.model_copy(update=dict(children=[]))
                    book_config.pages.append(page_copy)

                    book_config.pages.extend(leftover_children)
                    book_configs.append(book_config)

                else:
                    filtered_children = aux(page.children)

                    if filtered_children or page.url:
                        page_copy = page.model_copy(update=dict(
                            children=filtered_children))
                        result.append(page_copy)

            return result

        sub_pages = aux(pages)

        return book_configs, sub_pages

    async def generate_books(self) -> None:
        self.pprint_cache(self.config, self.config, "before_split")
        book_configs, remaining_pages = self._collect_book_configs(
            self.config.pages, self.config)

        self.pprint_cache(book_configs, self.config, "split_book_configs")
        self.pprint_cache(remaining_pages, self.config, "remaining_pages")

        import csv
        csv_path = Path(self.cache_dir).joinpath("tmp.csv")
        logging.info(f"csv path {csv_path}")
        with open(csv_path, "w+") as file:
            writer = csv.writer(file)
            writer.writerow(["path", "words", "images"])

            for book_config in book_configs:
                path, words, images = await self.generate_epub(book_config)
                size = int(Path(path).stat().st_size / 1024)
                logging.info(
                    f"wrote {path} with {words} words and {images} images. Book size is {size:,} KB"
                )
                writer.writerow([path, words, images])

        if remaining_pages:
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
    logging.info("+" * 120)
    config_path = Path(sys.argv[1])

    if not config_path.exists():
        logging.error(f"Configuration file {config_path} not found")
        return

    config_data = json.loads(config_path.read_text())
    config = BookConfig(**config_data)

    async with EpubGenerator(config) as generator:
        await generator.generate_books()

    logging.info("-" * 120)


if __name__ == "__main__":
    asyncio.run(main())
