#!/usr/bin/env python

import argparse
import logging
import os
import sys
import tempfile
from pathlib import Path

import torch
import pymupdf
from beartype import beartype
from beartype.typing import List
from transformers import AutoModel, AutoTokenizer

logging.basicConfig(
    level=logging.INFO,
    format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
)
logger = logging.getLogger("unlimited_ocr")

MODEL_NAME = "baidu/Unlimited-OCR"
DONE_MARKER = ".ocr_done"

IMAGE_EXTENSIONS = {".png", ".jpg", ".jpeg", ".webp", ".bmp", ".tif", ".tiff"}


@beartype
def pdf_to_images(pdf_path: Path, dpi: int) -> List[Path]:
    doc = pymupdf.open(pdf_path)
    tmp_dir = Path(tempfile.mkdtemp(prefix="pdf_ocr_"))
    mat = pymupdf.Matrix(dpi / 72, dpi / 72)
    paths: List[Path] = []
    for i, page in enumerate(doc):
        out = tmp_dir / f"page_{i + 1:04d}.png"
        page.get_pixmap(matrix=mat).save(str(out))
        paths.append(out)
    doc.close()
    return paths


@beartype
def get_output_dir(input_path: Path) -> Path:
    return input_path.with_name(f"{input_path.name}.d")


@beartype
def run_ocr(model, tokenizer, input_path: Path) -> None:
    output_dir = get_output_dir(input_path)
    marker = output_dir / DONE_MARKER

    if marker.exists():
        logger.info(
            f"Skipping {input_path}, already converted at {output_dir}")
        return

    output_dir.mkdir(parents=True, exist_ok=True)
    ext = input_path.suffix.lower()

    match ext:
        case ".pdf":
            logger.info(f"Converting PDF {input_path} to images")
            image_files = pdf_to_images(input_path, dpi=300)
            CHUNK_SIZE = 20
            logger.info(
                f"Running multi-page OCR on {len(image_files)} pages in chunks of 40"
            )
            for chunk_index, start in enumerate(
                    range(0, len(image_files), CHUNK_SIZE)):
                chunk = image_files[start:start + CHUNK_SIZE]
                chunk_dir = output_dir / f"chunk_{chunk_index:04d}"
                chunk_dir.mkdir(parents=True, exist_ok=True)
                logger.info(
                    f"Processing pages {start + 1}..{start + len(chunk)} into {chunk_dir}"
                )
                model.infer_multi(
                    tokenizer,
                    prompt="<image>Multi page parsing.",
                    image_files=[str(p) for p in chunk],
                    output_path=str(chunk_dir),
                    image_size=1024,
                    max_length=32768,
                    no_repeat_ngram_size=35,
                    ngram_window=1024,
                    save_results=True,
                )

        case str() if ext in IMAGE_EXTENSIONS:
            logger.info(f"Running single-image OCR on {input_path}")
            model.infer(
                tokenizer,
                prompt="<image>document parsing.",
                image_file=str(input_path),
                output_path=str(output_dir),
                base_size=1024,
                image_size=640,
                crop_mode=True,
                max_length=32768,
                no_repeat_ngram_size=35,
                ngram_window=128,
                save_results=True,
            )
        case _:
            raise ValueError(
                f"Unsupported input extension '{ext}' for file {input_path}, "
                f"expected one of: .pdf, {sorted(IMAGE_EXTENSIONS)}")

    marker.write_text("done\n")
    logger.info(f"Finished {input_path}, results in {output_dir}")


@beartype
def collect_inputs(inputs: List[Path]) -> List[Path]:
    files: List[Path] = []
    for input_path in inputs:
        if not input_path.exists():
            raise FileNotFoundError(f"Input path does not exist: {input_path}")
        if input_path.is_dir():
            for entry in sorted(input_path.iterdir()):
                if entry.is_file() and (entry.suffix.lower()
                                        in IMAGE_EXTENSIONS
                                        or entry.suffix.lower() == ".pdf"):
                    files.append(entry)
        else:
            files.append(input_path)
    return files


@beartype
def main() -> None:
    parser = argparse.ArgumentParser(
        description="Run baidu/Unlimited-OCR on PDFs or images.")
    parser.add_argument("inputs",
                        nargs="+",
                        type=Path,
                        help="PDF or image files to convert")
    parser.add_argument(
        "--dpi",
        type=int,
        default=300,
        help="DPI for PDF page rendering (unused placeholder for CLI symmetry)"
    )
    args = parser.parse_args()

    if not torch.cuda.is_available():
        raise RuntimeError(
            "torch.cuda.is_available() is False; on AMD this means ROCm PyTorch is not "
            "installed or the GPU is not visible. Install the ROCm build of torch."
        )

    logger.info(f"Loading model {MODEL_NAME}")
    tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME,
                                              trust_remote_code=True)
    model = AutoModel.from_pretrained(
        MODEL_NAME,
        trust_remote_code=True,
        use_safetensors=True,
        torch_dtype=torch.bfloat16,
    )
    model = model.eval().cuda()

    for input_path in collect_inputs(args.inputs):
        run_ocr(model, tokenizer, input_path)


if __name__ == "__main__":
    main()
