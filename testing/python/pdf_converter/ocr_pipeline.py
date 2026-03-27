import io
import json
import base64
import logging
import hashlib
import time
from pathlib import Path
from typing import List, Optional
from datetime import timedelta

import requests
from PIL import Image as PILImage
from pdf2image import convert_from_path, pdfinfo_from_path
from xdg_base_dirs import xdg_cache_home

from models import PageData, DocTag
from config import AppConfig

# --- Configuration ---
OLLAMA_URL = "http://localhost:11434/api/chat"
MODEL_NAME = "ibm/granite-docling"  # Name as it appears in 'ollama list'

logger = logging.getLogger(__name__)

def get_cache_path(pdf_path: Path, page_num: int) -> Path:
    """Generates an XDG-compliant cache path for the specific page image."""
    cache_dir = xdg_cache_home() / "docling-vlm-cache" / "images"
    cache_dir.mkdir(parents=True, exist_ok=True)

    with open(pdf_path, 'rb') as f:
        file_prefix = f.read(16).hex()[:16]

    name_hash = hashlib.md5(str(pdf_path.absolute()).encode()).hexdigest()[:16]
    return cache_dir / f"{file_prefix}_{name_hash}_{page_num}.png"

def format_eta(seconds: float) -> str:
    if seconds < 0: return "00:00:00"
    return str(timedelta(seconds=int(seconds)))

def call_ollama_vlm(image: PILImage.Image) -> str:
    """Sends image to Ollama API and returns the response text."""
    buffered = io.BytesIO()
    image.save(buffered, format="PNG")
    base64_image = base64.b64encode(buffered.getvalue()).decode("utf-8")

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
    
    return response.json().get("message", {}).get("content", "")

def parse_spatial_tags(raw_xml: str, page_num: int) -> List[DocTag]:
    """
    Parses the raw XML from Ollama/Docling to extract spatial tags.
    This is a stub for the full XML parser that securely extracts bounding boxes
    and builds the tree structure.
    """
    # For now, return a placeholder root tag containing everything.
    # A full XML/HTML parser (like lxml or BeautifulSoup) would be used here
    # to extract <doctag bbox="..."> tags and convert them to the DocTag model.
    root_tag = DocTag(
        id=f"page{page_num}-root",
        tag_name="root",
        text=raw_xml
    )
    return [root_tag]

def process_pdf(pdf_path: Path, output_dir: Path, pages: Optional[str] = None):
    """Processes a single PDF file and saves per-page JSON dumps in the output directory."""
    if not pdf_path.exists():
        logger.error(f"Input file not found: {pdf_path}")
        return

    pdf_output_dir = output_dir / pdf_path.stem
    pdf_output_dir.mkdir(parents=True, exist_ok=True)

    # 1. Determine Page Range
    try:
        info = pdfinfo_from_path(str(pdf_path))
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
    logger.info(f"Processing PDF: {pdf_path.name} | Range: {first_page}-{last_page} ({total_to_convert} pages)")

    # 2. Convert PDF to images
    logger.info("Rasterizing PDF pages to images...")
    all_pages = convert_from_path(str(pdf_path), first_page=first_page, last_page=last_page)
    
    start_time = time.perf_counter()

    # 3. Processing Loop
    for i, page_img in enumerate(all_pages):
        current_page_idx = i + 1
        current_page_num = first_page + i
        
        # Save image to cache so GUI can load it
        image_cache_file = get_cache_path(pdf_path, current_page_num)
        page_img.save(image_cache_file, format="PNG")
        
        # JSON dump output file
        json_output_file = pdf_output_dir / f"page_{current_page_num:03d}.json"
        
        # Calculate Statistics
        elapsed = time.perf_counter() - start_time
        pps = current_page_idx / elapsed if elapsed > 0 else 0
        remaining = total_to_convert - current_page_idx
        eta_seconds = remaining / pps if pps > 0 else 0
        
        stats_header = f"[{current_page_idx}/{total_to_convert}] PPS: {pps:.2f} | ETA: {format_eta(eta_seconds)}"
        
        try:
            if json_output_file.exists():
                logger.info(f"{stats_header} | Page {current_page_num}: Using cached OCR dump (re-creating spatial tags)")
                with open(json_output_file, 'r') as f:
                    page_data_json = json.load(f)
                    clean_text = page_data_json.get("raw_docling_response", "")
            else:
                logger.info(f"{stats_header} | Page {current_page_num}: Requesting VLM")
                response_text = call_ollama_vlm(page_img)
                
                clean_text = response_text.strip()
                if not clean_text.startswith("<doctag>"):
                    clean_text = f"<doctag>{clean_text}</doctag>"
                
            spatial_tags = parse_spatial_tags(clean_text, current_page_num)
            
            page_data = PageData(
                page_number=current_page_num,
                raw_docling_response=clean_text,
                spatial_tags=spatial_tags,
                image_cache_path=str(image_cache_file.absolute())
            )
            
            # Save dump
            with open(json_output_file, 'w') as f:
                f.write(page_data.model_dump_json(indent=2))
                
        except Exception as e:
            logger.error(f"Error processing page {current_page_num}: {e}")

def run_headless_pipeline(config: AppConfig):
    """Entry point for the headless OCR pipeline."""
    for input_path in config.input_dirs:
        input_path = Path(input_path)
        if input_path.is_file() and input_path.suffix.lower() == '.pdf':
            process_pdf(input_path, config.output_dir)
        elif input_path.is_dir():
            for pdf_file in input_path.rglob('*.pdf'):
                process_pdf(pdf_file, config.output_dir)
