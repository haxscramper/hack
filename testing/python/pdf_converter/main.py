import io
import json
import base64
import logging
import hashlib
import time
from pathlib import Path
from typing import List, Optional
from datetime import timedelta

import click
import requests
from PIL import Image as PILImage
from pdf2image import convert_from_path, pdfinfo_from_path
from xdg_base_dirs import xdg_cache_home
from docling_core.types.doc.document import DocTagsDocument
from docling_core.types.doc import DoclingDocument

# --- Configuration ---
OLLAMA_URL = "http://localhost:11434/api/chat"
MODEL_NAME = "ibm/granite-docling"  # Name as it appears in 'ollama list'

# Setup logging
logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s | %(levelname)s | %(message)s',
    datefmt='%H:%M:%S'
)
logger = logging.getLogger(__name__)

def get_cache_path(pdf_path: Path, page_num: int) -> Path:
    """Generates an XDG-compliant cache path for the specific page."""
    cache_dir = xdg_cache_home() / "docling-vlm-cache"
    cache_dir.mkdir(parents=True, exist_ok=True)

    with open(pdf_path, 'rb') as f:
        file_prefix = f.read(16).hex()[:16]

    name_hash = hashlib.md5(str(pdf_path.absolute()).encode()).hexdigest()[:16]
    return cache_dir / f"{file_prefix}_{name_hash}_{page_num}.json"

def format_eta(seconds: float) -> str:
    if seconds < 0: return "00:00:00"
    return str(timedelta(seconds=int(seconds)))

def call_ollama_vlm(image: PILImage.Image) -> str:
    """Sends image to Ollama API and returns the response text."""
    buffered = io.BytesIO()
    image.save(buffered, format="PNG")
    base64_image = base64.b64encode(buffered.getvalue()).decode("utf-8")

    # Ollama API structure for Vision models
    payload = {
        "model": MODEL_NAME,
        "messages": [{
            "role": "user",
            "content": "Convert this page to docling.",
            "images": [base64_image]
        }],
        "stream": False,
        "options": {
            "temperature": 0
        }
    }

    response = requests.post(OLLAMA_URL, json=payload, timeout=600)
    response.raise_for_status()
    
    # Ollama returns the result in 'message' -> 'content'
    return response.json().get("message", {}).get("content", "")

@click.command()
@click.argument('pdf_input', type=click.Path(exists=True, path_type=Path))
@click.option('--pages', help='Range of pages (e.g., "1-5" or "3").', default=None)
@click.option('--output', '-o', type=click.Path(path_type=Path), help='Output HTML file path.')
def main(pdf_input: Path, pages: Optional[str], output: Optional[Path]):
    """Process PDF to Docling HTML using local Ollama instance with Granite Docling."""
    
    if not output:
        output = pdf_input.with_suffix(".html")

    # 1. Determine Page Range
    try:
        info = pdfinfo_from_path(pdf_input)
        max_pages = info["Pages"]
    except Exception as e:
        logger.error(f"Could not read PDF info: {e}")
        return
    
    first_page, last_page = 1, max_pages
    if pages:
        parts = pages.split('-')
        first_page = max(1, int(parts[0]))
        if len(parts) > 1:
            last_page = min(max_pages, int(parts[1]))
        else:
            last_page = first_page

    total_to_convert = (last_page - first_page) + 1
    logger.info(f"Target: {pdf_input.name} | Range: {first_page}-{last_page} ({total_to_convert} pages)")

    # 2. Convert PDF to images
    logger.info("Rasterizing PDF pages to images...")
    all_pages = convert_from_path(pdf_input, first_page=first_page, last_page=last_page)

    doctags_list = []
    image_list = []
    
    # Timing statistics
    start_time = time.perf_counter()

    # 3. Processing Loop
    for i, page_img in enumerate(all_pages):
        current_page_idx = i + 1
        current_page_num = first_page + i
        cache_file = get_cache_path(pdf_input, current_page_num)
        
        # Calculate Statistics
        elapsed = time.perf_counter() - start_time
        pps = current_page_idx / elapsed if elapsed > 0 else 0
        remaining = total_to_convert - current_page_idx
        eta_seconds = remaining / pps if pps > 0 else 0
        
        stats_header = f"[{current_page_idx}/{total_to_convert}] PPS: {pps:.2f} | ETA: {format_eta(eta_seconds)}"
        
        response_text = ""
        if cache_file.exists():
            logger.info(f"{stats_header} | Page {current_page_num}: Using Cache")
            with open(cache_file, 'r') as f:
                response_text = json.load(f).get("text", "")
        else:
            logger.info(f"{stats_header} | Page {current_page_num}: Requesting Ollama ({MODEL_NAME})")
            try:
                response_text = call_ollama_vlm(page_img)
                # Save to cache immediately to prevent loss if script crashes later
                with open(cache_file, 'w') as f:
                    json.dump({
                        "page": current_page_num, 
                        "text": response_text, 
                        "model": MODEL_NAME,
                        "timestamp": time.time()
                    }, f)
            except Exception as e:
                logger.error(f"Error calling Ollama on page {current_page_num}: {e}")
                response_text = "<doctag></doctag>"

        # Ensure wrapping for docling-core parser
        clean_text = response_text.strip()
        if not clean_text.startswith("<doctag>"):
            clean_text = f"<doctag>{clean_text}</doctag>"

        doctags_list.append(clean_text)
        image_list.append(page_img)

    # 4. Final Assembly using docling-core
    total_duration = format_eta(time.perf_counter() - start_time)
    logger.info(f"Processing finished in {total_duration}. Reconstructing document structure...")
    
    try:
        # Step 1: Parse raw tags into internal DocTagsDocument
        doctags_doc = DocTagsDocument.from_doctags_and_image_pairs(doctags_list, image_list)
        
        # Step 2: Load into a structured DoclingDocument
        docling_doc = DoclingDocument.load_from_doctags(doctag_document=doctags_doc)
        
        # Step 3: Export final HTML
        docling_doc.save_as_html(filename=output)
        logger.info(f"Success! Output file: {output}")
    except Exception as e:
        logger.critical(f"Failed to assemble structured document: {e}")

if __name__ == '__main__':
    main()
