#!/usr/bin/env python

import os
import json
from PIL import Image
from dominate import document
from dominate.tags import table, tr, td, img, caption, tbody
from dominate.util import text
from typing import List, Tuple, Optional, Union
from pathlib import Path
from imagehash import phash
from operator import itemgetter
import pdfkit

def extract_artist_from_json(json_data: Union[dict, list]) -> Optional[str]:
    if isinstance(json_data, dict):
        for key, value in json_data.items():
            if key == "text" and "by" in value:
                return value.split("by")[-1].strip()
            
            elif isinstance(value, (dict, list)):
                result = extract_artist_from_json(value)
                if result:
                    return result
    elif isinstance(json_data, list):
        for item in json_data:
            result = extract_artist_from_json(item)
            if result:
                return result
    return None


columns = 6

def create_html_table(image_data: List[Tuple[Path, str]]) -> str:
    doc = document(title="Image Gallery")
    with doc:
        with table(border=1, style='border-collapse: collapse; width: 100%;').add(tbody()):
            for row in range(0, len(image_data), columns):
                stride = image_data[row:row + columns]
                with tr():
                    for img_path, _ in stride:
                        with td():
                            img(src=str(img_path.resolve()), width="300px")

                with tr():
                    for _, artist in stride:
                        with td(style="text-align:center;"):
                            text(artist)

    return doc.render()


def process_images(directory: Path) -> List[Tuple[Path, str]]:
    image_data = []
    for file in directory.glob("*"):
        if file.suffix == ".png":
            with Image.open(str(file)) as img:
                metadata = img.info
                if "prompt" in metadata:
                    prompt_data = json.loads(metadata["prompt"])
                    artist = extract_artist_from_json(prompt_data)
                    if artist:
                        image_data.append((file, artist))
 
    return image_data

def sort_images_by_similarity(image_data: List[Tuple[str, str]]) -> List[Tuple[str, str]]:
    image_hashes: List[Tuple[str, str, str]] = []
    for file, artist in image_data:
        with Image.open(str(file)) as img:
            image_hashes.append((file, artist, str(phash(img))))


    if not image_hashes:
        return []
    

    first_hash = image_hashes[0][2]

    distances = [(img, artist, bin(int(hash_val, 16) ^ int(first_hash, 16)).count('1'))
                 for img, artist, hash_val in image_hashes]

    sorted_data = sorted(distances, key=itemgetter(2))
    return [(img, artist) for img, artist, _ in sorted_data]

def generate_pdf_preview(html_content: str, output_path: str, page_width: int = 1200) -> None:
    options = {
        'page-width': f'{page_width}px',
        'page-height': '20000px',  # Set a large height to accommodate the content
        'margin-top': '0',
        'margin-right': '0',
        'margin-bottom': '0',
        'margin-left': '0',
        'encoding': 'UTF-8',
        'enable-local-file-access': None,
        'disable-smart-shrinking': None,
    }
    pdfkit.from_string(html_content, output_path, options=options)

directory: str = Path("power_artists")
image_data: List[Tuple[Path, str]] = process_images(directory)
image_data = sort_images_by_similarity(image_data)
html_content: str = create_html_table(image_data)
with open("image_gallery.html", "w") as file:
    file.write(html_content)

pdf_output_path: str = 'image_gallery_preview.pdf'
page_width: int = 1920  # Adjust the page width as needed
generate_pdf_preview(html_content, pdf_output_path, page_width)
