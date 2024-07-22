#!/usr/bin/env python
import requests
from bs4 import BeautifulSoup
from urllib.parse import urlparse
from pathlib import Path
from typing import List, Tuple
import logging

OUT_DIR: Path = Path("~/tmp/blog_posts").expanduser()

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def download_post(url: str) -> Tuple[str, str, str]:
    response = requests.get(url)
    soup = BeautifulSoup(response.content, "html.parser")
    title = soup.find("h3", class_="post-title").get_text(strip=True)
    date = soup.find("h2", class_="date-header").get_text(strip=True)
    content = soup.find("div", class_="post-body")
    return date, title, content


def save_post(url: str, out_dir: Path) -> None:
    parsed_url = urlparse(url)
    path_parts = Path(parsed_url.path).parts
    year, month, slug = path_parts[1], path_parts[2], path_parts[3]
    safe_title = slug.replace("/", "-")

    directory = out_dir / year / month
    directory.mkdir(parents=True, exist_ok=True)

    filename = f"{safe_title}.txt"
    filepath = directory / filename

    if filepath.exists():
        logger.info(f"File {filepath} already exists. Skipping download.")
        return

    date, title, content = download_post(url)

    with filepath.open("w", encoding="utf-8") as file:
        file.write(f"{title}\n\n{content}")

    logger.info(f"Saved post to {filepath}")


def main(out_dir: Path) -> None:
    for post in Path(
            "~/tmp/linklist.txt").expanduser().read_text().splitlines():
        save_post(post, out_dir)


if __name__ == "__main__":
    main(OUT_DIR)
