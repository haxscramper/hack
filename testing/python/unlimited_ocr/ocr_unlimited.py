#!/usr/bin/env python
from __future__ import annotations

import io
import re
import shutil
from dataclasses import dataclass
from pathlib import Path

import fitz
import torch
from beartype import beartype
from beartype.typing import Any
from loguru import logger
from PIL import Image, ImageDraw, ImageFont
from transformers import AutoModel, AutoTokenizer

from ocr_models import OcrBBox, OcrElement, OcrPage

DEFAULT_MODEL_ID = "baidu/Unlimited-OCR"


@dataclass(frozen=True)
class ExtractedImageElement:
    page_number: int
    element_index: int
    image_blob: bytes


@dataclass(frozen=True)
class UnlimitedChunkOcrResult:
    """Unlimited-OCR specific result for one processed chunk."""
    chunk_index: int
    raw_text: str
    pages: list[OcrPage]
    extracted_images: list[ExtractedImageElement]


@beartype
def render_pdf_pages(input_pdf: Path, dst_dir: Path, dpi: int) -> list[Path]:
    dst_dir.mkdir(parents=True, exist_ok=True)
    page_paths: list[Path] = []

    doc = fitz.open(input_pdf)
    zoom = dpi / 72.0
    matrix = fitz.Matrix(zoom, zoom)

    for page_index, page in enumerate(doc, start=1):
        pix = page.get_pixmap(matrix=matrix, alpha=False)
        page_path = dst_dir / f"page_{page_index:04d}.png"
        pix.save(page_path.as_posix())
        page_paths.append(page_path)

    doc.close()
    if not page_paths:
        raise RuntimeError(f"PDF contains no pages: {input_pdf}")
    return page_paths


@beartype
def unpack_infer_multi_result(result: Any) -> str:
    match result:
        case tuple() as value:
            if not value:
                raise RuntimeError("infer_multi returned an empty tuple")
            first = value[0]
            if not isinstance(first, str):
                raise RuntimeError(
                    f"infer_multi[0] must be str, got {type(first)}")
            return first
        case str() as value:
            return value
        case _:
            raise RuntimeError(
                f"infer_multi returned unsupported type: {type(result)}")


@beartype
def parse_bbox(text: str) -> OcrBBox:
    match = re.fullmatch(
        r"\[\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\]",
        text.strip(),
    )
    if match is None:
        raise ValueError(f"Invalid bbox syntax: {text}")

    x1, y1, x2, y2 = map(int, match.groups())
    if x2 < x1 or y2 < y1:
        raise ValueError(
            f"Invalid bbox coordinates with negative width or height: {text}")
    return OcrBBox(x1=x1, y1=y1, x2=x2, y2=y2)


@beartype
def parse_page_content(page_text: str, page_number: int) -> OcrPage:
    det_pattern = re.compile(
        r"<\|det\|>\s*(?P<label>[^\[]+?)\s*(?P<bbox>\[\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\])\s*<\|/det\|>\s*(?P<text>.*?)(?=(?:<\|det\|>|<PAGE>|$))",
        re.DOTALL,
    )
    ref_det_pattern = re.compile(
        r"<\|ref\|>\s*(?P<label>.*?)\s*<\|/ref\|>\s*<\|det\|>\s*(?P<bbox>\[\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\])\s*<\|/det\|>\s*(?P<text>.*?)(?=(?:<\|ref\|>|<\|det\|>|<PAGE>|$))",
        re.DOTALL,
    )

    elements: list[OcrElement] = []

    for match in det_pattern.finditer(page_text):
        elements.append(
            OcrElement(
                label=match.group("label").strip(),
                bbox=parse_bbox(match.group("bbox")),
                text=match.group("text").strip(),
            ))

    for match in ref_det_pattern.finditer(page_text):
        elements.append(
            OcrElement(
                label=match.group("label").strip(),
                bbox=parse_bbox(match.group("bbox")),
                text=match.group("text").strip(),
            ))

    return OcrPage(page_number=page_number, elements=elements)


@beartype
def parse_ocr_output(raw_text: str, page_offset: int) -> list[OcrPage]:
    if not raw_text.strip():
        return []

    pages: list[OcrPage] = []
    absolute_page = page_offset + 1

    for page_text in raw_text.split("<PAGE>"):
        page_text = page_text.strip()
        if not page_text:
            continue
        pages.append(parse_page_content(page_text, absolute_page))
        absolute_page += 1

    if raw_text.strip() and not pages:
        raise RuntimeError(
            "Model output is non-empty, but no structured OCR tokens were parsed"
        )
    return pages


@beartype
def scale_bbox(bbox: OcrBBox, width: int,
               height: int) -> tuple[int, int, int, int]:
    x1 = max(0, min(width - 1, int(bbox.x1 / 999 * width)))
    y1 = max(0, min(height - 1, int(bbox.y1 / 999 * height)))
    x2 = max(0, min(width - 1, int(bbox.x2 / 999 * width)))
    y2 = max(0, min(height - 1, int(bbox.y2 / 999 * height)))

    if x2 < x1:
        x1, x2 = x2, x1
    if y2 < y1:
        y1, y2 = y2, y1
    return x1, y1, x2, y2


@beartype
def color_for_label(label: str) -> tuple[int, int, int]:
    seed = sum(ord(char) for char in label)
    return (50 + (seed * 37) % 180, 50 + (seed * 67) % 180,
            50 + (seed * 97) % 180)


@beartype
def annotate_and_extract(
    source_page: Path,
    page: OcrPage,
    annotated_pages_dir: Path,
    extracted_images_dir: Path,
) -> list[ExtractedImageElement]:
    annotated_pages_dir.mkdir(parents=True, exist_ok=True)
    extracted_images_dir.mkdir(parents=True, exist_ok=True)

    image = Image.open(source_page).convert("RGB")
    draw = ImageDraw.Draw(image)
    font = ImageFont.load_default()
    width, height = image.size

    extracted: list[ExtractedImageElement] = []
    image_crop_index = 0

    for element_index, element in enumerate(page.elements):
        x1, y1, x2, y2 = scale_bbox(element.bbox, width, height)
        color = color_for_label(element.label)

        draw.rectangle((x1, y1, x2, y2), outline=color, width=2)
        text_bbox = draw.textbbox((x1, y1), element.label, font=font)
        draw.rectangle(text_bbox, fill=(255, 255, 255))
        draw.text((x1, y1), element.label, fill=color, font=font)

        if element.label.strip().casefold() == "image":
            crop = image.crop((x1, y1, x2, y2))
            crop_path = (
                extracted_images_dir /
                f"page_{page.page_number:04d}_image_{image_crop_index:04d}.png"
            )
            crop.save(crop_path)
            image_crop_index += 1

            buffer = io.BytesIO()
            crop.save(buffer, format="PNG")
            extracted.append(
                ExtractedImageElement(
                    page_number=page.page_number,
                    element_index=element_index,
                    image_blob=buffer.getvalue(),
                ))

    image.save(annotated_pages_dir / f"page_{page.page_number:04d}.png")
    return extracted


class UnlimitedOcrProcessor:
    """Runs Unlimited-OCR over page images of a file, chunk by chunk."""

    @beartype
    def __init__(
        self,
        model_id: str = DEFAULT_MODEL_ID,
        image_size: int = 1024,
        max_length: int = 32768,
        dpi: int = 300,
        prompt_image: str = "<image>document parsing.",
        prompt_pdf: str = "<image>Multi page parsing.",
    ) -> None:
        self.image_size = image_size
        self.max_length = max_length
        self.dpi = dpi
        self.prompt_image = prompt_image
        self.prompt_pdf = prompt_pdf

        self.tokenizer = AutoTokenizer.from_pretrained(model_id,
                                                       trust_remote_code=True)
        self.model = AutoModel.from_pretrained(
            model_id,
            trust_remote_code=True,
            torch_dtype=torch.bfloat16,
        ).cuda().eval()

    @beartype
    def render_pages(self, source_file: Path,
                     rendered_dir: Path) -> list[Path]:
        ext = source_file.suffix.lower()
        if ext == ".pdf":
            return render_pdf_pages(source_file, rendered_dir, dpi=self.dpi)
        rendered_dir.mkdir(parents=True, exist_ok=True)
        page_copy = rendered_dir / f"page_0001{ext}"
        shutil.copy2(source_file, page_copy)
        return [page_copy]

    @beartype
    def _run_infer_multi(self, image_files: list[Path],
                         output_path: Path) -> str:
        result = self.model.infer_multi(
            self.tokenizer,
            prompt=self.prompt_pdf,
            image_files=[str(path) for path in image_files],
            output_path=str(output_path),
            image_size=self.image_size,
            save_results=False,
            max_length=self.max_length,
        )
        return unpack_infer_multi_result(result)

    @beartype
    def _run_infer_single(self, image_file: Path, output_path: Path) -> str:
        result = self.model.infer(
            self.tokenizer,
            prompt=self.prompt_image,
            image_file=str(image_file),
            output_path=str(output_path),
            base_size=1024,
            image_size=self.image_size,
            crop_mode=True,
            save_results=False,
            eval_mode=True,
            max_length=self.max_length,
        )
        if not isinstance(result, str):
            raise RuntimeError(
                f"infer did not return str with eval_mode=True, got {type(result)}"
            )
        return result

    @beartype
    def process_chunk(
        self,
        source_file: Path,
        chunk_index: int,
        chunk_page_files: list[Path],
        page_offset: int,
        chunk_dir: Path,
    ) -> UnlimitedChunkOcrResult:
        """OCR one chunk of page images, returning Unlimited-OCR specific data."""
        pages_dir = chunk_dir / "pages"
        annotated_pages_dir = chunk_dir / "annotated_pages"
        extracted_images_dir = chunk_dir / "images"
        model_tmp_dir = chunk_dir / "_model"
        for directory in (pages_dir, annotated_pages_dir, extracted_images_dir,
                          model_tmp_dir):
            directory.mkdir(parents=True, exist_ok=True)

        local_page_paths: list[Path] = []
        for source_page in chunk_page_files:
            destination = pages_dir / source_page.name
            shutil.copy2(source_page, destination)
            local_page_paths.append(destination)

        is_single_image = source_file.suffix.lower() != ".pdf"
        if is_single_image:
            raw_text = self._run_infer_single(local_page_paths[0],
                                              model_tmp_dir)
        else:
            raw_text = self._run_infer_multi(local_page_paths, model_tmp_dir)

        (chunk_dir / "raw_text.txt").write_text(raw_text, encoding="utf-8")

        pages = parse_ocr_output(raw_text, page_offset=page_offset)
        if is_single_image and not pages:
            pages = [OcrPage(page_number=1, elements=[])]

        local_pages_by_number = {
            page_offset + idx + 1: path
            for idx, path in enumerate(local_page_paths)
        }
        known_pages = set(local_pages_by_number.keys())
        discarded = [
            page.page_number for page in pages
            if page.page_number not in known_pages
        ]
        if discarded:
            logger.error(
                "Discarding out-of-range parsed pages for chunk {}: {}",
                chunk_index, sorted(discarded))
            pages = [p for p in pages if p.page_number in known_pages]

        extracted_images: list[ExtractedImageElement] = []
        for page in pages:
            source_page = local_pages_by_number[page.page_number]
            extracted_images.extend(
                annotate_and_extract(
                    source_page=source_page,
                    page=page,
                    annotated_pages_dir=annotated_pages_dir,
                    extracted_images_dir=extracted_images_dir,
                ))

        return UnlimitedChunkOcrResult(
            chunk_index=chunk_index,
            raw_text=raw_text,
            pages=pages,
            extracted_images=extracted_images,
        )
