#!/usr/bin/env python

from pathlib import Path
import ebooklib
from ebooklib import epub
from typing import List


def create_epub_from_txt_files(files: List[str], output_epub: str) -> None:
    book = epub.EpubBook()
    book.set_identifier("id123456")
    book.set_title("Combined ePub Document")
    book.set_language("en")
    book.add_author("Author Name")

    chapters = {}
    for file_path in files:
        path = Path(file_path)
        year_month = f"{path.parts[0]}/{path.parts[1]}"
        if year_month not in chapters:
            chapters[year_month] = []

        with path.open("r", encoding="utf-8") as file:
            content = file.read()
            chapter = epub.EpubHtml(title=path.stem,
                                    file_name=f"{path.stem}.xhtml",
                                    lang="en")
            chapter.set_content(f"<h1>{path.stem}</h1><p>{content}</p>")
            chapters[year_month].append(chapter)
            book.add_item(chapter)

    for year_month, chapter_list in chapters.items():
        section_title = year_month.replace("/", " - ")
        toc_section = (epub.Section(section_title), tuple(chapter_list))
        book.toc.append(toc_section)

    book.add_item(epub.EpubNcx())
    book.add_item(epub.EpubNav())
    style = 'body { font-family: Arial, sans-serif; }'
    nav_css = epub.EpubItem(uid="style_nav",
                            file_name="style/nav.css",
                            media_type="text/css",
                            content=style)
    book.add_item(nav_css)

    epub.write_epub(output_epub, book, {})


if __name__ == "__main__":
    files = sorted([str(it) for it in Path("~/tmp/blog_posts").expanduser().rglob("*.txt")])
    create_epub_from_txt_files(files, str(Path("~/tmp/output.epub").expanduser()))
