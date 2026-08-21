#!/usr/bin/env python
from __future__ import annotations

import io
import re
import shutil
import tempfile
from loguru import logger
from contextlib import asynccontextmanager
from pathlib import Path
from uuid import uuid4

import fitz
import torch
import uvicorn
from fastapi import FastAPI, File, HTTPException, UploadFile
from PIL import Image, ImageDraw, ImageFont
from transformers import AutoModel, AutoTokenizer

from pydantic import BaseModel, Field


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


DEFAULT_MODEL_ID = "baidu/Unlimited-OCR"
CHUNK_PAGES = 16


def render_pdf_pages(input_pdf: Path, dst_dir: Path, dpi: int) -> list[Path]:
    dst_dir.mkdir(parents=True, exist_ok=True)
    page_paths: list[Path] = []
    doc = fitz.open(input_pdf)
    matrix = fitz.Matrix(dpi / 72.0, dpi / 72.0)
    for page_index, page in enumerate(doc, start=1):
        pix = page.get_pixmap(matrix=matrix, alpha=False)
        page_path = dst_dir / f"page_{page_index:04d}.png"
        pix.save(page_path.as_posix())
        page_paths.append(page_path)
    doc.close()
    if not page_paths:
        raise RuntimeError(f"PDF contains no pages: {input_pdf}")
    return page_paths


def unpack_infer_multi_result(result) -> str:
    if isinstance(result, tuple):
        if not result or not isinstance(result[0], str):
            raise RuntimeError(
                f"infer_multi returned unexpected tuple: {type(result)}")
        return result[0]
    if isinstance(result, str):
        return result
    raise RuntimeError(
        f"infer_multi returned unsupported type: {type(result)}")


BBOX_RE = re.compile(r"\[\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\]")

DET_PATTERN = re.compile(
    r"<\|det\|>\s*(?P<label>[^\[]+?)\s*(?P<bbox>\[\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\])\s*<\|/det\|>\s*(?P<text>.*?)(?=(?:<\|det\|>|<\|ref\|>|<PAGE>|$))",
    re.DOTALL,
)
REF_DET_PATTERN = re.compile(
    r"<\|ref\|>\s*(?P<label>.*?)\s*<\|/ref\|>\s*<\|det\|>\s*(?P<bbox>\[\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\])\s*<\|/det\|>\s*(?P<text>.*?)(?=(?:<\|ref\|>|<\|det\|>|<PAGE>|$))",
    re.DOTALL,
)


def parse_bbox(text: str) -> OcrBBox:
    match = re.fullmatch(BBOX_RE, text.strip())
    if match is None:
        raise ValueError(f"Invalid bbox syntax: {text}")
    x1, y1, x2, y2 = map(int, match.groups())
    if x2 < x1 or y2 < y1:
        raise ValueError(f"Invalid bbox coordinates: {text}")
    return OcrBBox(x1=x1, y1=y1, x2=x2, y2=y2)


def parse_page_content(page_text: str, page_number: int) -> OcrPage:
    elements: list[OcrElement] = []
    for pattern in (DET_PATTERN, REF_DET_PATTERN):
        for match in pattern.finditer(page_text):
            elements.append(
                OcrElement(
                    label=match.group("label").strip(),
                    bbox=parse_bbox(match.group("bbox")),
                    text=match.group("text").strip(),
                ))
    return OcrPage(page_number=page_number, elements=elements)


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
    if not pages:
        raise RuntimeError(
            "Model output is non-empty, but no OCR tokens were parsed")
    return pages


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


def color_for_label(label: str) -> tuple[int, int, int]:
    seed = sum(ord(c) for c in label)
    return (50 + (seed * 37) % 180, 50 + (seed * 67) % 180,
            50 + (seed * 97) % 180)


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
        text_bbox = draw.textbbox((x1, y1), element.label, font=font)
        draw.rectangle(text_bbox, fill=(255, 255, 255))
        draw.text((x1, y1), element.label, fill=color, font=font)

        if element.label.strip().casefold() == "image":
            crop = image.crop((x1, y1, x2, y2))
            crop.save(
                extracted_images_dir /
                f"page_{page.page_number:04d}_image_{image_crop_index:04d}.png"
            )
            image_crop_index += 1

    image.save(annotated_pages_dir / f"page_{page.page_number:04d}.png")


class UnlimitedOcrProcessor:

    def __init__(
        self,
        model_id: str = DEFAULT_MODEL_ID,
        image_size: int = 1024,
        max_length: int = 32768,
        dpi: int = 300,
        prompt_image: str = "<image>document parsing.",
        prompt_pdf: str = "<image>Multi page parsing.",
    ) -> None:
        logger.info("Creating unlimited OCR processor")

        if not torch.cuda.is_available():
            raise RuntimeError(
                f"CUDA is unavailable: torch={torch.__version__}, "
                f"torch_cuda={torch.version.cuda}")

        self.device = torch.device("cuda:0")
        torch.cuda.set_device(self.device)

        self.image_size = image_size
        self.max_length = max_length
        self.dpi = dpi
        self.prompt_image = prompt_image
        self.prompt_pdf = prompt_pdf

        self.tokenizer = AutoTokenizer.from_pretrained(
            model_id,
            trust_remote_code=True,
        )
        self.model = AutoModel.from_pretrained(
            model_id,
            trust_remote_code=True,
            dtype=torch.bfloat16,
        )
        self.model.to(device=self.device, dtype=torch.bfloat16)
        self.model.eval()

        parameter = next(self.model.parameters())
        if parameter.device.type != "cuda":
            raise RuntimeError(f"Model remained on {parameter.device}")

        logger.info(
            "Model loaded on {}, allocated={:.2f} GiB, reserved={:.2f} GiB",
            parameter.device,
            torch.cuda.memory_allocated(self.device) / 1024**3,
            torch.cuda.memory_reserved(self.device) / 1024**3,
        )

    def render_pages(self, source_file: Path,
                     rendered_dir: Path) -> list[Path]:
        ext = source_file.suffix.lower()
        if ext == ".pdf":
            return render_pdf_pages(source_file, rendered_dir, dpi=self.dpi)
        rendered_dir.mkdir(parents=True, exist_ok=True)
        page_copy = rendered_dir / f"page_0001{ext}"
        shutil.copy2(source_file, page_copy)
        return [page_copy]

    def _run_infer_multi(self, image_files: list[Path],
                         output_path: Path) -> str:
        result = self.model.infer_multi(
            self.tokenizer,
            prompt=self.prompt_pdf,
            image_files=[str(p) for p in image_files],
            output_path=str(output_path),
            image_size=self.image_size,
            save_results=False,
            max_length=self.max_length,
        )
        return unpack_infer_multi_result(result)

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
            raise RuntimeError(f"infer did not return str, got {type(result)}")
        return result

    def process_chunk(
        self,
        source_file: Path,
        chunk_index: int,
        chunk_page_files: list[Path],
        page_offset: int,
        chunk_dir: Path,
    ) -> OcrChunkResult:
        pages_dir = chunk_dir / "pages"
        annotated_pages_dir = chunk_dir / "annotated_pages"
        extracted_images_dir = chunk_dir / "images"
        model_tmp_dir = chunk_dir / "_model"
        for d in (pages_dir, annotated_pages_dir, extracted_images_dir,
                  model_tmp_dir):
            d.mkdir(parents=True, exist_ok=True)

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

        raw_text_file = chunk_dir / "raw_text.txt"
        raw_text_file.write_text(raw_text, encoding="utf-8")

        pages = parse_ocr_output(raw_text, page_offset=page_offset)
        if is_single_image and not pages:
            pages = [OcrPage(page_number=1, elements=[])]

        local_pages_by_number = {
            page_offset + idx + 1: path
            for idx, path in enumerate(local_page_paths)
        }
        known_pages = set(local_pages_by_number.keys())
        pages = [p for p in pages if p.page_number in known_pages]

        for page in pages:
            annotate_and_extract(
                source_page=local_pages_by_number[page.page_number],
                page=page,
                annotated_pages_dir=annotated_pages_dir,
                extracted_images_dir=extracted_images_dir,
            )

        structured_json_file = chunk_dir / "structured.json"
        structured_json_file.write_text(
            OcrChunkResult(
                chunk_index=chunk_index,
                page_start=page_offset + 1,
                page_end=page_offset + len(local_page_paths),
                raw_text_file=str(raw_text_file),
                structured_json_file=str(structured_json_file),
                pages_dir=str(pages_dir),
                annotated_pages_dir=str(annotated_pages_dir),
                extracted_images_dir=str(extracted_images_dir),
                pages=pages,
            ).model_dump_json(indent=2),
            encoding="utf-8",
        )

        return OcrChunkResult.model_validate_json(
            structured_json_file.read_text())


processor: UnlimitedOcrProcessor | None = None


@asynccontextmanager
async def lifespan(app: FastAPI):
    global processor
    processor = UnlimitedOcrProcessor()
    yield


app = FastAPI(title="Unlimited-OCR Service", lifespan=lifespan)


@app.get("/health")
def health() -> dict:
    return {"status": "ok", "model_loaded": processor is not None}


@app.post("/ocr", response_model=OcrDocumentResult)
def ocr(file: UploadFile = File(...)) -> OcrDocumentResult:
    logger.info("Received uploaded file")
    assert processor is not None
    suffix = Path(file.filename or "upload.pdf").suffix.lower()
    if suffix not in (".pdf", ".png", ".jpg", ".jpeg", ".webp", ".tif",
                      ".tiff", ".bmp"):
        raise HTTPException(status_code=400,
                            detail=f"Unsupported file type: {suffix}")

    workdir = Path(tempfile.mkdtemp(prefix=f"ocr_{uuid4().hex[:8]}_"))
    try:
        logger.info("Rendering pages")
        source_file = workdir / f"source{suffix}"
        source_file.write_bytes(file.file.read())

        rendered_dir = workdir / "rendered"
        page_files = processor.render_pages(source_file, rendered_dir)

        chunks: list[OcrChunkResult] = []
        for chunk_index, start in enumerate(
                range(0, len(page_files), CHUNK_PAGES)):
            chunk_pages = page_files[start:start + CHUNK_PAGES]
            logger.info(
                f"Processing chunk {start}+{CHUNK_PAGES}/{len(page_files)}")
            chunks.append(
                processor.process_chunk(
                    source_file=source_file,
                    chunk_index=chunk_index,
                    chunk_page_files=chunk_pages,
                    page_offset=start,
                    chunk_dir=workdir / f"chunk_{chunk_index:04d}",
                ))

        return OcrDocumentResult(
            source_file=str(source_file),
            relative_source=source_file.name,
            output_dir=str(workdir),
            chunks=chunks,
        )
    finally:
        logger.info("completed document processing")
        shutil.rmtree(workdir, ignore_errors=True)


if __name__ == "__main__":
    uvicorn.run("main:app", host="0.0.0.0", port=8080)
