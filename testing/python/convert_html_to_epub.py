#!/usr/bin/env python

import logging
import re
from pathlib import Path
from typing import List, Optional
from dataclasses import dataclass
from urllib.parse import urljoin, urlparse
import base64
import mimetypes

import click
from bs4 import BeautifulSoup, Tag
from ebooklib import epub
import requests

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s")

@dataclass
class TocEntry:
    title: str
    level: int
    anchor: str

@dataclass
class ImageInfo:
    src: str
    filename: str
    content: bytes
    media_type: str

class HtmlToEpubConverter:
    def __init__(self, input_path: Path, output_path: Path):
        self.input_path = input_path
        self.output_path = output_path
        self.book = epub.EpubBook()
        self.images: List[ImageInfo] = []
        self.toc_entries: List[TocEntry] = []
        
    def convert(self) -> None:
        logging.debug(f"Starting conversion from {self.input_path} to {self.output_path}")
        
        html_content = self._read_html_file()
        soup = BeautifulSoup(html_content, "html.parser")
        
        self._setup_book_metadata(soup)
        self._process_images(soup)
        self._extract_toc_entries(soup)
        self._create_chapter(soup)
        self._add_images_to_book()
        self._create_toc()
        self._finalize_book()
        
        logging.debug(f"Conversion completed successfully")
    
    def _read_html_file(self) -> str:
        try:
            with open(self.input_path, "r", encoding="utf-8") as f:
                return f.read()
        except UnicodeDecodeError:
            logging.debug("UTF-8 decoding failed, trying with latin-1")
            with open(self.input_path, "r", encoding="latin-1") as f:
                return f.read()

    
    def _setup_book_metadata(self, soup: BeautifulSoup) -> None:
        title_tag = soup.find("title")
        title = title_tag.get_text().strip() if title_tag else self.input_path.stem
        
        self.book.set_identifier("id123456")
        self.book.set_title(title)
        self.book.set_language("en")
        self.book.add_author("Unknown")
        
        logging.debug(f"Book metadata set: title='{title}'")
    
    def _process_images(self, soup: BeautifulSoup) -> None:
        img_tags = soup.find_all("img")
        
        for i, img in enumerate(img_tags):
            src = img.get("src")
            if not src:
                continue
                
            try:
                image_info = self._download_or_read_image(src, i)
                if image_info:
                    self.images.append(image_info)
                    img["src"] = f"images/{image_info.filename}"
                    logging.debug(f"Processed image: {image_info.filename}")
            except Exception as e:
                logging.warning(f"Failed to process image {src}: {e}")
    
    def _download_or_read_image(self, src: str, index: int) -> Optional[ImageInfo]:
        if src.startswith("data:"):
            return self._process_data_url_image(src, index)
        elif src.startswith(("http://", "https://")):
            return self._download_remote_image(src, index)
        else:
            return self._read_local_image(src, index)
    
    def _process_data_url_image(self, src: str, index: int) -> Optional[ImageInfo]:
        try:
            header, data = src.split(",", 1)
            media_type = header.split(";")[0].split(":")[1]
            content = base64.b64decode(data)
            
            ext = mimetypes.guess_extension(media_type) or ".png"
            filename = f"image_{index}{ext}"
            
            return ImageInfo(src=src, filename=filename, content=content, media_type=media_type)
        except Exception:
            return None
    
    def _download_remote_image(self, src: str, index: int) -> Optional[ImageInfo]:
        try:
            response = requests.get(src, timeout=10)
            response.raise_for_status()
            
            content_type = response.headers.get("content-type", "image/png")
            ext = mimetypes.guess_extension(content_type) or ".png"
            filename = f"image_{index}{ext}"
            
            return ImageInfo(src=src, filename=filename, content=response.content, media_type=content_type)
        except Exception:
            return None
    
    def _read_local_image(self, src: str, index: int) -> Optional[ImageInfo]:
        try:
            from urllib.parse import unquote
            decoded_src = unquote(src)
            image_path = self.input_path.parent / decoded_src
            if not image_path.exists():
                return None
                
            content = image_path.read_bytes()
            media_type = mimetypes.guess_type(str(image_path))[0] or "image/png"
            filename = f"image_{index}{image_path.suffix}"
            
            return ImageInfo(src=src, filename=filename, content=content, media_type=media_type)
        except Exception:
            return None
            
    def _extract_toc_entries(self, soup: BeautifulSoup) -> None:
        heading_tags = soup.find_all(re.compile(r"^h[1-6]$"))
        
        for heading in heading_tags:
            level = int(heading.name[1])
            title = heading.get_text().strip()
            
            if not heading.get("id"):
                anchor = self._generate_anchor(title)
                heading["id"] = anchor
            else:
                anchor = heading["id"]
            
            self.toc_entries.append(TocEntry(title=title, level=level, anchor=anchor))
            logging.debug(f"TOC entry: {title} (level {level}, anchor {anchor})")
    
    def _generate_anchor(self, title: str) -> str:
        anchor = re.sub(r"[^\w\s-]", "", title.lower())
        anchor = re.sub(r"[-\s]+", "-", anchor)
        return anchor.strip("-")
    
    def _create_chapter(self, soup: BeautifulSoup) -> None:
        body = soup.find("body")
        if not body:
            formatted_content = soup.prettify()
        else:
            body_soup = BeautifulSoup("".join(str(child) for child in body.children), "html.parser")
            formatted_content = body_soup.prettify()
        
        chapter_html = f"""<!DOCTYPE html>
    <html>
    <head>
        <meta charset="utf-8"/>
        <title>Chapter</title>
    </head>
    <body>
    {formatted_content}
    </body>
    </html>"""
        
        chapter = epub.EpubHtml(title="Chapter", file_name="chapter.xhtml", lang="en")
        chapter.content = chapter_html
        
        self.book.add_item(chapter)
        self.book.spine = ["nav", chapter]

    
    def _add_images_to_book(self) -> None:
        for image_info in self.images:
            epub_image = epub.EpubImage()
            epub_image.file_name = f"images/{image_info.filename}"
            epub_image.media_type = image_info.media_type
            epub_image.content = image_info.content
            
            self.book.add_item(epub_image)
    
    def _create_toc(self) -> None:
        if not self.toc_entries:
            self.book.toc = []
            return
        
        def build_nested_toc(entries: List[TocEntry]) -> List:
            result = []
            i = 0
            while i < len(entries):
                entry = entries[i]
                link = epub.Link(f"chapter.xhtml#{entry.anchor}", entry.title, "chapter")
                
                children = []
                j = i + 1
                while j < len(entries) and entries[j].level > entry.level:
                    if entries[j].level == entry.level + 1:
                        child_entries = [entries[j]]
                        k = j + 1
                        while k < len(entries) and entries[k].level > entries[j].level:
                            child_entries.append(entries[k])
                            k += 1
                        children.extend(build_nested_toc(child_entries))
                        j = k
                    else:
                        j += 1
                
                if children:
                    result.append((link, children))
                else:
                    result.append(link)
                
                i = j if j > i + 1 else i + 1
            
            return result
        
        self.book.toc = build_nested_toc(self.toc_entries)
        self.book.add_item(epub.EpubNcx())
        self.book.add_item(epub.EpubNav())
        
    
    def _finalize_book(self) -> None:
        epub.write_epub(str(self.output_path), self.book, {})

@click.command()
@click.option("--input", "input_path", required=True, type=click.Path(exists=True, path_type=Path), help="Input HTML file path")
@click.option("--output", "output_path", required=True, type=click.Path(path_type=Path), help="Output EPUB file path")
def main(input_path: Path, output_path: Path) -> None:
    converter = HtmlToEpubConverter(input_path, output_path)
    converter.convert()

if __name__ == "__main__":
    main()
