#!/usr/bin/env python
from __future__ import annotations

import argparse
import json
import math
import re
import shutil
from dataclasses import dataclass
from pathlib import Path

import fitz
import torch
from beartype import beartype
from beartype.typing import Any, Iterable, Optional
from loguru import logger
from PIL import Image, ImageDraw, ImageFont
from pydantic import BaseModel, Field, ValidationError
from transformers import AutoModelForCausalLM, AutoTokenizer

IMAGE_EXTENSIONS = {".png", ".jpg", ".jpeg", ".webp", ".bmp", ".tif", ".tiff"}
DONE_MARKER = ".done"
DEFAULT_MODEL_ID = "baidu/Unlimited-OCR"


class OcrBBox(BaseModel):
    x1: int = Field(ge=0)
    y1: int = Field(ge=0)
    x2: int = Field(ge=0)
    y2: int = Field(ge=0)


class OcrElement(BaseModel):
    label: str = Field(min_length=1)
    bbox: OcrBBox
    text: str = ""


class OcrPage(BaseModel):
    page_number: int = Field(ge=1)
    elements: list[OcrElement] = Field(default_factory=list)


class OcrChunkResult(BaseModel):
    chunk_index: int = Field(ge=0)
    page_start: int = Field(ge=1)
    page_end: int = Field(ge=1)
    raw_text_file: str
    structured_json_file: str
    pages_dir: str
    annotated_pages_dir: str
    extracted_images_dir: str
    pages: list[OcrPage]


class OcrDocumentResult(BaseModel):
    source_file: str
    relative_source: str
    output_dir: str
    chunks: list[OcrChunkResult]


@beartype
@dataclass(frozen=True)
class ParsedChunk:
    raw_text: str
    pages: list[OcrPage]


@beartype
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("input_path", type=Path)
    parser.add_argument("output_dir", type=Path)
    parser.add_argument(
        "--mirror-from",
        type=Path,
        default=None,
        help="Base directory used to compute mirrored relative paths.",
    )
    parser.add_argument("--model-id", default=DEFAULT_MODEL_ID)
    parser.add_argument("--chunk-size", type=int, default=20)
    parser.add_argument("--dpi", type=int, default=300)
    parser.add_argument("--max-length", type=int, default=32768)
    parser.add_argument("--image-size", type=int, default=1024)
    parser.add_argument("--prompt-image", default="<image>document parsing.")
    parser.add_argument("--prompt-pdf", default="<image>Multi page parsing.")
    parser.add_argument("--overwrite", action="store_true")
    return parser.parse_args()


@beartype
def collect_inputs(input_path: Path) -> list[Path]:
    if input_path.is_file():
        ext = input_path.suffix.lower()
        if ext != ".pdf" and ext not in IMAGE_EXTENSIONS:
            raise ValueError(f"Unsupported input file extension: {ext}")
        return [input_path]

    if not input_path.is_dir():
        raise ValueError(f"Input path does not exist: {input_path}")

    files = sorted(p for p in input_path.rglob("*") if p.is_file() and (
        p.suffix.lower() == ".pdf" or p.suffix.lower() in IMAGE_EXTENSIONS))
    if not files:
        raise RuntimeError(
            f"No supported files found in directory: {input_path}")
    return files


@beartype
def resolve_mirror_from(input_path: Path, mirror_from: Optional[Path]) -> Path:
    if mirror_from is not None:
        if not mirror_from.exists():
            raise ValueError(f"--mirror-from does not exist: {mirror_from}")
        return mirror_from.resolve()

    if input_path.is_dir():
        return input_path.resolve()
    return input_path.resolve().parent


@beartype
def mirrored_output_base(source_file: Path, mirror_from: Path,
                         output_root: Path) -> Path:
    source_file = source_file.resolve()
    try:
        relative = source_file.relative_to(mirror_from)
    except ValueError as error:
        raise ValueError(
            f"Cannot mirror {source_file} from base {mirror_from}; pass a correct --mirror-from"
        ) from error

    return (output_root / relative).with_suffix("")


@beartype
def load_model_and_tokenizer(model_id: str) -> tuple[Any, Any]:
    tokenizer = AutoTokenizer.from_pretrained(model_id, trust_remote_code=True)
    model = AutoModelForCausalLM.from_pretrained(
        model_id,
        trust_remote_code=True,
        torch_dtype=torch.bfloat16,
    ).cuda().eval()
    return model, tokenizer


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
def split_chunks(items: list[Path],
                 chunk_size: int) -> Iterable[tuple[int, list[Path]]]:
    if chunk_size <= 0:
        raise ValueError(f"chunk_size must be > 0, got {chunk_size}")
    for i in range(0, len(items), chunk_size):
        yield i // chunk_size, items[i:i + chunk_size]


@beartype
def unpack_infer_multi_result(result: Any) -> str:
    if isinstance(result, tuple):
        if not result:
            raise RuntimeError("infer_multi returned an empty tuple")
        if not isinstance(result[0], str):
            raise RuntimeError(
                f"infer_multi[0] is not a string: {type(result[0])}")
        return result[0]
    if isinstance(result, str):
        return result
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
        raise ValueError(f"Invalid bbox coordinates (x2/y2 < x1/y1): {text}")
    return OcrBBox(x1=x1, y1=y1, x2=x2, y2=y2)


@beartype
def parse_page_content(page_text: str, page_number: int) -> OcrPage:
    # Primary format:
    # <|det|>label [x1, y1, x2, y2]<|/det|>content
    det_pattern = re.compile(
        r"<\|det\|>\s*(?P<label>[^\[]+?)\s*(?P<bbox>\[\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\])\s*<\|/det\|>\s*(?P<text>.*?)(?=(?:<\|det\|>|<PAGE>|$))",
        re.DOTALL,
    )

    # Secondary format seen in model internals:
    # <|ref|>label<|/ref|><|det|>[x1, y1, x2, y2]<|/det|>content
    ref_det_pattern = re.compile(
        r"<\|ref\|>\s*(?P<label>.*?)\s*<\|/ref\|>\s*<\|det\|>\s*(?P<bbox>\[\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\])\s*<\|/det\|>\s*(?P<text>.*?)(?=(?:<\|ref\|>|<\|det\|>|<PAGE>|$))",
        re.DOTALL,
    )

    elements: list[OcrElement] = []

    for m in det_pattern.finditer(page_text):
        bbox = parse_bbox(m.group("bbox"))
        elements.append(
            OcrElement(
                label=m.group("label").strip(),
                bbox=bbox,
                text=m.group("text").strip(),
            ))

    for m in ref_det_pattern.finditer(page_text):
        bbox = parse_bbox(m.group("bbox"))
        elements.append(
            OcrElement(
                label=m.group("label").strip(),
                bbox=bbox,
                text=m.group("text").strip(),
            ))

    return OcrPage(page_number=page_number, elements=elements)


@beartype
def parse_ocr_output(raw_text: str, page_offset: int) -> list[OcrPage]:
    if not raw_text.strip():
        return []

    pages_raw = raw_text.split("<PAGE>")
    pages: list[OcrPage] = []
    absolute_page = page_offset + 1

    for page_text in pages_raw:
        page_text = page_text.strip()
        if not page_text:
            continue
        page = parse_page_content(page_text, absolute_page)
        pages.append(page)
        absolute_page += 1

    if raw_text.strip() and not pages:
        raise RuntimeError(
            "Model output is non-empty, but no structured OCR tokens were parsed"
        )

    return pages


@beartype
def scale_bbox(bbox: OcrBBox, width: int,
               height: int) -> tuple[int, int, int, int]:
    x1 = int(bbox.x1 / 999 * width)
    y1 = int(bbox.y1 / 999 * height)
    x2 = int(bbox.x2 / 999 * width)
    y2 = int(bbox.y2 / 999 * height)

    x1 = max(0, min(width - 1, x1))
    y1 = max(0, min(height - 1, y1))
    x2 = max(0, min(width - 1, x2))
    y2 = max(0, min(height - 1, y2))

    if x2 < x1:
        x1, x2 = x2, x1
    if y2 < y1:
        y1, y2 = y2, y1
    return x1, y1, x2, y2


@beartype
def color_for_label(label: str) -> tuple[int, int, int]:
    seed = sum(ord(c) for c in label)
    r = 50 + (seed * 37) % 180
    g = 50 + (seed * 67) % 180
    b = 50 + (seed * 97) % 180
    return r, g, b


@beartype
def annotate_and_extract(
    source_page: Path,
    page: OcrPage,
    annotated_pages_dir: Path,
    extracted_images_dir: Path,
) -> None:
    annotated_pages_dir.mkdir(parents=True, exist_ok=True)
    extracted_images_dir.mkdir(parents=True, exist_ok=True)

    image = Image.open(source_page).convert("RGB")
    draw = ImageDraw.Draw(image)
    font = ImageFont.load_default()
    width, height = image.size

    image_crop_index = 0
    for element in page.elements:
        x1, y1, x2, y2 = scale_bbox(element.bbox, width, height)
        color = color_for_label(element.label)

        draw.rectangle((x1, y1, x2, y2), outline=color, width=2)
        label_text = element.label
        text_bbox = draw.textbbox((x1, y1), label_text, font=font)
        draw.rectangle(text_bbox, fill=(255, 255, 255))
        draw.text((x1, y1), label_text, fill=color, font=font)

        if element.label == "image":
            crop = image.crop((x1, y1, x2, y2))
            crop_name = f"page_{page.page_number:04d}_image_{image_crop_index:04d}.png"
            crop.save(extracted_images_dir / crop_name)
            image_crop_index += 1

    annotated_path = annotated_pages_dir / f"page_{page.page_number:04d}.png"
    image.save(annotated_path)


@beartype
def run_chunk_infer_multi(
    model: Any,
    tokenizer: Any,
    image_files: list[Path],
    prompt: str,
    output_path: Path,
    image_size: int,
    max_length: int,
) -> str:
    result = model.infer_multi(
        tokenizer,
        prompt=prompt,
        image_files=[str(p) for p in image_files],
        output_path=str(output_path),
        image_size=image_size,
        save_results=False,
        max_length=max_length,
    )
    return unpack_infer_multi_result(result)


@beartype
def run_chunk_infer_single(
    model: Any,
    tokenizer: Any,
    image_file: Path,
    prompt: str,
    output_path: Path,
    image_size: int,
    max_length: int,
) -> str:
    result = model.infer(
        tokenizer,
        prompt=prompt,
        image_file=str(image_file),
        output_path=str(output_path),
        base_size=1024,
        image_size=image_size,
        crop_mode=True,
        save_results=False,
        eval_mode=True,
        max_length=max_length,
    )
    if not isinstance(result, str):
        raise RuntimeError(
            f"infer did not return a string in eval_mode=True: {type(result)}")
    return result


@beartype
def process_pdf(
    model: Any,
    tokenizer: Any,
    source_file: Path,
    output_base: Path,
    chunk_size: int,
    dpi: int,
    prompt: str,
    image_size: int,
    max_length: int,
) -> OcrDocumentResult:
    output_base.mkdir(parents=True, exist_ok=True)
    rendered_dir = output_base / "_rendered_pages"
    rendered_pages = render_pdf_pages(source_file, rendered_dir, dpi=dpi)

    chunks: list[OcrChunkResult] = []

    for chunk_index, chunk_pages in split_chunks(rendered_pages, chunk_size):
        chunk_dir = output_base / "chunks" / f"chunk_{chunk_index:04d}"
        chunk_dir.mkdir(parents=True, exist_ok=True)

        pages_dir = chunk_dir / "pages"
        annotated_pages_dir = chunk_dir / "annotated_pages"
        extracted_images_dir = chunk_dir / "images"
        model_tmp_dir = chunk_dir / "_model"

        pages_dir.mkdir(parents=True, exist_ok=True)
        annotated_pages_dir.mkdir(parents=True, exist_ok=True)
        extracted_images_dir.mkdir(parents=True, exist_ok=True)
        model_tmp_dir.mkdir(parents=True, exist_ok=True)

        local_page_paths: list[Path] = []
        for src in chunk_pages:
            dst = pages_dir / src.name
            shutil.copy2(src, dst)
            local_page_paths.append(dst)

        raw_text = run_chunk_infer_multi(
            model=model,
            tokenizer=tokenizer,
            image_files=local_page_paths,
            prompt=prompt,
            output_path=model_tmp_dir,
            image_size=image_size,
            max_length=max_length,
        )

        raw_text_path = chunk_dir / "raw_text.txt"
        raw_text_path.write_text(raw_text, encoding="utf-8")

        page_offset = chunk_index * chunk_size
        pages = parse_ocr_output(raw_text, page_offset=page_offset)

        local_pages_by_number = {
            page_offset + idx + 1: path
            for idx, path in enumerate(local_page_paths)
        }

        for page in pages:
            source_page = local_pages_by_number.get(page.page_number)
            if source_page is None:
                raise RuntimeError(
                    f"Parsed page number {page.page_number} does not exist in chunk {chunk_index}"
                )
            annotate_and_extract(
                source_page=source_page,
                page=page,
                annotated_pages_dir=annotated_pages_dir,
                extracted_images_dir=extracted_images_dir,
            )

        structured_json_path = chunk_dir / "structured_data.json"
        structured_json_path.write_text(
            json.dumps([p.model_dump() for p in pages],
                       indent=2,
                       ensure_ascii=False),
            encoding="utf-8",
        )

        page_start = chunk_index * chunk_size + 1
        page_end = page_start + len(chunk_pages) - 1

        chunk_result = OcrChunkResult(
            chunk_index=chunk_index,
            page_start=page_start,
            page_end=page_end,
            raw_text_file=str(raw_text_path),
            structured_json_file=str(structured_json_path),
            pages_dir=str(pages_dir),
            annotated_pages_dir=str(annotated_pages_dir),
            extracted_images_dir=str(extracted_images_dir),
            pages=pages,
        )
        chunks.append(chunk_result)

    return OcrDocumentResult(
        source_file=str(source_file),
        relative_source="",
        output_dir=str(output_base),
        chunks=chunks,
    )


@beartype
def process_image(
    model: Any,
    tokenizer: Any,
    source_file: Path,
    output_base: Path,
    prompt: str,
    image_size: int,
    max_length: int,
) -> OcrDocumentResult:
    output_base.mkdir(parents=True, exist_ok=True)

    chunk_dir = output_base / "chunks" / "chunk_0000"
    pages_dir = chunk_dir / "pages"
    annotated_pages_dir = chunk_dir / "annotated_pages"
    extracted_images_dir = chunk_dir / "images"
    model_tmp_dir = chunk_dir / "_model"

    pages_dir.mkdir(parents=True, exist_ok=True)
    annotated_pages_dir.mkdir(parents=True, exist_ok=True)
    extracted_images_dir.mkdir(parents=True, exist_ok=True)
    model_tmp_dir.mkdir(parents=True, exist_ok=True)

    page_copy = pages_dir / f"page_0001{source_file.suffix.lower()}"
    shutil.copy2(source_file, page_copy)

    raw_text = run_chunk_infer_single(
        model=model,
        tokenizer=tokenizer,
        image_file=page_copy,
        prompt=prompt,
        output_path=model_tmp_dir,
        image_size=image_size,
        max_length=max_length,
    )

    raw_text_path = chunk_dir / "raw_text.txt"
    raw_text_path.write_text(raw_text, encoding="utf-8")

    pages = parse_ocr_output(raw_text, page_offset=0)
    if not pages:
        pages = [OcrPage(page_number=1, elements=[])]

    for page in pages:
        if page.page_number != 1:
            raise RuntimeError(
                f"Single image produced unexpected page number: {page.page_number}"
            )
        annotate_and_extract(
            source_page=page_copy,
            page=page,
            annotated_pages_dir=annotated_pages_dir,
            extracted_images_dir=extracted_images_dir,
        )

    structured_json_path = chunk_dir / "structured_data.json"
    structured_json_path.write_text(
        json.dumps([p.model_dump() for p in pages],
                   indent=2,
                   ensure_ascii=False),
        encoding="utf-8",
    )

    chunk_result = OcrChunkResult(
        chunk_index=0,
        page_start=1,
        page_end=1,
        raw_text_file=str(raw_text_path),
        structured_json_file=str(structured_json_path),
        pages_dir=str(pages_dir),
        annotated_pages_dir=str(annotated_pages_dir),
        extracted_images_dir=str(extracted_images_dir),
        pages=pages,
    )

    return OcrDocumentResult(
        source_file=str(source_file),
        relative_source="",
        output_dir=str(output_base),
        chunks=[chunk_result],
    )


@beartype
def process_file(
    model: Any,
    tokenizer: Any,
    source_file: Path,
    output_root: Path,
    mirror_from: Path,
    chunk_size: int,
    dpi: int,
    prompt_image: str,
    prompt_pdf: str,
    image_size: int,
    max_length: int,
    overwrite: bool,
) -> None:
    output_base = mirrored_output_base(source_file, mirror_from, output_root)
    output_base.mkdir(parents=True, exist_ok=True)

    marker = output_base / DONE_MARKER
    if marker.exists() and not overwrite:
        logger.info(f"Skipping already processed file: {source_file}")
        return

    ext = source_file.suffix.lower()
    if ext == ".pdf":
        doc_result = process_pdf(
            model=model,
            tokenizer=tokenizer,
            source_file=source_file,
            output_base=output_base,
            chunk_size=chunk_size,
            dpi=dpi,
            prompt=prompt_pdf,
            image_size=image_size,
            max_length=max_length,
        )
    elif ext in IMAGE_EXTENSIONS:
        doc_result = process_image(
            model=model,
            tokenizer=tokenizer,
            source_file=source_file,
            output_base=output_base,
            prompt=prompt_image,
            image_size=image_size,
            max_length=max_length,
        )
    else:
        raise ValueError(
            f"Unsupported extension for file {source_file}: {ext}")

    relative_source = source_file.resolve().relative_to(mirror_from).as_posix()
    doc_result.relative_source = relative_source  # type: ignore[attr-defined]

    doc_json_path = output_base / "document_result.json"
    doc_json_path.write_text(
        doc_result.model_dump_json(indent=2),
        encoding="utf-8",
    )

    marker.write_text("done\n", encoding="utf-8")
    logger.info(f"Finished: {source_file} -> {output_base}")


@beartype
def main() -> None:
    args = parse_args()

    input_path = args.input_path.resolve()
    output_dir = args.output_dir.resolve()
    mirror_from = resolve_mirror_from(input_path, args.mirror_from)
    files = collect_inputs(input_path)

    output_dir.mkdir(parents=True, exist_ok=True)
    model, tokenizer = load_model_and_tokenizer(args.model_id)

    failures: list[tuple[Path, Exception]] = []

    for file_path in files:
        try:
            process_file(
                model=model,
                tokenizer=tokenizer,
                source_file=file_path,
                output_root=output_dir,
                mirror_from=mirror_from,
                chunk_size=args.chunk_size,
                dpi=args.dpi,
                prompt_image=args.prompt_image,
                prompt_pdf=args.prompt_pdf,
                image_size=args.image_size,
                max_length=args.max_length,
                overwrite=args.overwrite,
            )
        except (ValidationError, Exception) as error:
            failures.append((file_path, error))
            logger.exception(f"Failed processing {file_path}: {error}")

    if failures:
        lines = "\n".join(f"- {path}: {err}" for path, err in failures)
        raise RuntimeError(f"OCR processing finished with failures:\n{lines}")


if __name__ == "__main__":
    main()
