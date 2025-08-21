#!/usr/bin/env python

import json
import logging
from pathlib import Path
from typing import List, Dict, Any
from bs4 import BeautifulSoup

logging.basicConfig(
    level=logging.DEBUG,
    format="%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s"
)


def parse_sitemap_to_config(html_content: str) -> Dict[str, Any]:
    soup = BeautifulSoup(html_content, "html.parser")

    config = {"pages": []}

    order_counter = 1

    def process_ul_list(ul_element,
                        parent_order: int = 0) -> List[Dict[str, Any]]:
        nonlocal order_counter
        pages = []

        for li in ul_element.find_all("li", recursive=False):
            link = li.find("a", href=True)
            if not link:
                continue

            href = link.get("href")
            title = link.get_text(strip=True)

            if not href or not title:
                continue

            page = {"url": href, "title": title, "order": order_counter}
            order_counter += 1

            nested_ul = li.find("ul")
            if nested_ul:
                children = process_ul_list(nested_ul, order_counter)
                if children:
                    page["children"] = children

            pages.append(page)

        return pages

    all_ul_elements = soup.find_all("ul")
    for ul in all_ul_elements:
        if ul.parent and ul.parent.name == "li":
            continue

        pages = process_ul_list(ul)
        config["pages"].extend(pages)

    return config


import sys


def main() -> None:
    html_file = Path(sys.argv[1])
    output_file = Path(sys.argv[2])

    if not html_file.exists():
        logging.error(f"HTML file {html_file} not found")
        return

    html_content = html_file.read_text(encoding="utf-8")
    config = parse_sitemap_to_config(html_content)

    output_file.write_text(json.dumps(config, indent=2), encoding="utf-8")
    logging.info(f"Configuration written to {output_file}")


if __name__ == "__main__":
    main()
