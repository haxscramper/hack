#!/usr/bin/env python
from __future__ import annotations

import asyncio
import os
import re
import shlex
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from urllib.parse import quote

import click
import paramiko
import pymupdf
from loguru import logger
from openai import AsyncOpenAI
from pydantic import BaseModel, Field

DEFAULT_MODEL_ID = "baidu/Unlimited-OCR"
DEFAULT_CHUNK_PAGES = 16
DEFAULT_CONCURRENCY = 4
DEFAULT_DPI = 150
DEFAULT_MAX_TOKENS = 32768
DEFAULT_RASTER_THREADS = os.cpu_count() or 1

BBOX_RE = re.compile(r"\[\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\]")

DET_PATTERN = re.compile(
    r"<\|det\|>\s*"
    r"(?P<label>[^\[]+?)\s*"
    r"(?P<bbox>\[\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\])\s*"
    r"<\|/det\|>\s*"
    r"(?P<text>.*?)"
    r"(?=(?:<\|det\|>|<\|ref\|>|<PAGE>|$))",
    re.DOTALL,
)

REF_DET_PATTERN = re.compile(
    r"<\|ref\|>\s*"
    r"(?P<label>.*?)\s*"
    r"<\|/ref\|>\s*"
    r"<\|det\|>\s*"
    r"(?P<bbox>\[\s*\d+\s*,\s*\d+\s*,\s*\d+\s*,\s*\d+\s*\])\s*"
    r"<\|/det\|>\s*"
    r"(?P<text>.*?)"
    r"(?=(?:<\|ref\|>|<\|det\|>|<PAGE>|$))",
    re.DOTALL,
)


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
    raw_text: str
    elements: list[OcrElement] = Field(default_factory=list)


class OcrChunkResult(BaseModel):
    chunk_index: int = Field(ge=0)
    page_start: int = Field(ge=1)
    page_end: int = Field(ge=1)
    raw_text: str
    pages: list[OcrPage]


class OcrDocumentResult(BaseModel):
    source_file: str
    model: str
    chunks: list[OcrChunkResult]


class SshConnectionArguments(BaseModel):
    host: str
    port: int
    user: str
    key_file: Path


def parse_bbox(text: str) -> OcrBBox:
    match = re.fullmatch(BBOX_RE, text.strip())

    if match is None:
        raise ValueError(f"Invalid bounding-box syntax: {text}")

    x1, y1, x2, y2 = map(int, match.groups())

    if x2 < x1 or y2 < y1:
        raise ValueError(f"Invalid bounding-box coordinates: {text}")

    return OcrBBox(
        x1=x1,
        y1=y1,
        x2=x2,
        y2=y2,
    )


def parse_page_content(raw_text: str, page_number: int) -> OcrPage:
    elements: list[OcrElement] = []
    occupied_ranges: list[tuple[int, int]] = []

    for match in REF_DET_PATTERN.finditer(raw_text):
        elements.append(
            OcrElement(
                label=match.group("label").strip(),
                bbox=parse_bbox(match.group("bbox")),
                text=match.group("text").strip(),
            ))
        occupied_ranges.append(match.span())

    for match in DET_PATTERN.finditer(raw_text):
        if any(start <= match.start() and match.end() <= end
               for start, end in occupied_ranges):
            continue

        elements.append(
            OcrElement(
                label=match.group("label").strip(),
                bbox=parse_bbox(match.group("bbox")),
                text=match.group("text").strip(),
            ))

    logger.debug(
        f"Parsed page {page_number}: found {len(elements)} OCR elements")

    return OcrPage(
        page_number=page_number,
        raw_text=raw_text,
        elements=elements,
    )


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


def render_pdf_page(
    input_pdf: Path,
    destination: Path,
    dpi: int,
    page_index: int,
) -> Path:
    matrix = pymupdf.Matrix(dpi / 72.0, dpi / 72.0)
    page_number = page_index + 1
    page_path = destination / f"page_{page_number:06d}.png"

    with pymupdf.open(input_pdf) as document:
        page = document.load_page(page_index)
        pixmap = page.get_pixmap(
            matrix=matrix,
            alpha=False,
        )
        pixmap.save(page_path.as_posix())

    logger.info(f"Rendered page {page_number}: "
                f"{pixmap.width}x{pixmap.height}, "
                f"{page_path.stat().st_size} bytes")

    return page_path


def render_pdf_pages(
    input_pdf: Path,
    destination: Path,
    dpi: int,
    raster_threads: int,
) -> list[Path]:
    destination.mkdir(parents=True, exist_ok=True)

    with pymupdf.open(input_pdf) as document:
        page_count = document.page_count

    if page_count == 0:
        raise RuntimeError(f"PDF contains no pages: {input_pdf}")

    logger.info(f"Rendering {page_count} PDF pages at {dpi} DPI "
                f"using {raster_threads} threads")

    with ThreadPoolExecutor(max_workers=raster_threads) as executor:
        page_paths = list(
            executor.map(
                lambda page_index: render_pdf_page(
                    input_pdf=input_pdf,
                    destination=destination,
                    dpi=dpi,
                    page_index=page_index,
                ),
                range(page_count),
            ))

    total_size = sum(path.stat().st_size for path in page_paths)

    logger.info(f"Rendered {len(page_paths)} pages, "
                f"total size {total_size} bytes")

    return page_paths


def run_ssh_command(
    ssh: paramiko.SSHClient,
    command: str,
) -> str:
    _, stdout, stderr = ssh.exec_command(command)
    status = stdout.channel.recv_exit_status()

    output = stdout.read().decode("utf-8")
    error = stderr.read().decode("utf-8")

    if status != 0:
        raise RuntimeError(f"Remote command failed with status {status}: "
                           f"{command}\n{error.strip()}")

    return output.strip()


def create_remote_job_directory(ssh: paramiko.SSHClient) -> str:
    remote_directory = run_ssh_command(
        ssh,
        "mkdir -p /tmp/unlimited-ocr && "
        "mktemp -d /tmp/unlimited-ocr/job.XXXXXXXXXX",
    )

    logger.info(f"Created remote job directory {remote_directory}")

    return remote_directory


def remove_remote_job_directory(
    ssh: paramiko.SSHClient,
    remote_directory: str,
) -> None:
    run_ssh_command(
        ssh,
        f"rm -rf -- {shlex.quote(remote_directory)}",
    )

    logger.info(f"Removed remote job directory {remote_directory}")


def remote_file_url(remote_path: str) -> str:
    return f"file://{quote(remote_path, safe='/')}"


def upload_pages(
    sftp: paramiko.SFTPClient,
    page_files: list[Path],
    remote_job_directory: str,
) -> list[str]:
    remote_paths: list[str] = []

    for page_path in page_files:
        remote_path = f"{remote_job_directory}/{page_path.name}"

        sftp.put(
            str(page_path),
            remote_path,
            confirm=True,
        )
        remote_paths.append(remote_path)

    logger.info(f"Uploaded {len(remote_paths)} rendered pages "
                f"to {remote_job_directory}")

    return remote_paths


async def recognize_chunk(
    client: AsyncOpenAI,
    model_id: str,
    remote_paths: list[str],
    chunk_index: int,
    page_start: int,
    max_tokens: int,
    semaphore: asyncio.Semaphore,
) -> OcrChunkResult:
    page_end = page_start + len(remote_paths) - 1
    chunk_number = chunk_index + 1

    content: list[dict[str, object]] = [{
        "type": "text",
        "text": "<image>document parsing.",
    }]

    content.extend({
        "type": "image_url",
        "image_url": {
            "url": remote_file_url(remote_path),
        },
    } for remote_path in remote_paths)

    async with semaphore:
        response = await client.chat.completions.create(
            model=model_id,
            messages=[{
                "role": "user",
                "content": content,
            }],
            max_tokens=max_tokens,
            temperature=0.0,
            extra_body={
                "skip_special_tokens": False,
                "vllm_xargs": {
                    "ngram_size": 35,
                    "window_size": 128,
                },
            },
        )

    if not response.choices:
        raise RuntimeError(
            f"Model returned no choices for chunk {chunk_number}")

    choice = response.choices[0]
    raw_text = choice.message.content

    if raw_text is None or not raw_text.strip():
        raise RuntimeError(
            f"Model returned empty output for chunk {chunk_number}")

    pages = parse_ocr_output(
        raw_text=raw_text,
        page_offset=page_start - 1,
    )

    if response.usage is not None:
        logger.debug(f"Chunk {chunk_number} token usage: "
                     f"{response.usage.prompt_tokens} prompt, "
                     f"{response.usage.completion_tokens} completion, "
                     f"{response.usage.total_tokens} total")

    logger.info(f"Completed OCR chunk {chunk_number}: "
                f"requested pages {page_start}-{page_end}, "
                f"parsed {len(pages)} pages, "
                f"received {len(raw_text)} characters")

    return OcrChunkResult(
        chunk_index=chunk_index,
        page_start=page_start,
        page_end=page_end,
        raw_text=raw_text,
        pages=pages,
    )


async def process_ocr_chunks(
    remote_paths: list[str],
    output_text: Path,
    vllm_url: str,
    model_id: str,
    chunk_pages: int,
    concurrency: int,
    max_tokens: int,
) -> list[OcrChunkResult]:
    chunk_specs = [(
        chunk_index,
        start + 1,
        remote_paths[start:start + chunk_pages],
    ) for chunk_index, start in enumerate(
        range(0, len(remote_paths), chunk_pages))]

    semaphore = asyncio.Semaphore(concurrency)
    tasks: list[asyncio.Task[OcrChunkResult]] = []

    output_text.write_text("", encoding="utf-8")

    async with AsyncOpenAI(
            api_key="EMPTY",
            base_url=vllm_url.rstrip("/"),
            timeout=3600,
    ) as client:
        for chunk_index, page_start, chunk_paths in chunk_specs:
            tasks.append(
                asyncio.create_task(
                    recognize_chunk(
                        client=client,
                        model_id=model_id,
                        remote_paths=chunk_paths,
                        chunk_index=chunk_index,
                        page_start=page_start,
                        max_tokens=max_tokens,
                        semaphore=semaphore,
                    )))

        chunks: list[OcrChunkResult] = []

        try:
            with output_text.open("a", encoding="utf-8") as text_file:
                for task in tasks:
                    chunk = await task
                    chunks.append(chunk)

                    if text_file.tell() > 0:
                        text_file.write("\n")

                    text_file.write(chunk.raw_text)

                    if not chunk.raw_text.endswith("\n"):
                        text_file.write("\n")

                    text_file.flush()

                    logger.info(f"Appended OCR chunk {chunk.chunk_index + 1} "
                                f"to {output_text}")
        finally:
            for task in tasks:
                if not task.done():
                    task.cancel()

            await asyncio.gather(
                *tasks,
                return_exceptions=True,
            )

    return chunks


async def process_pdf(
    input_pdf: Path,
    output_json: Path,
    vllm_url: str,
    ssh_arguments: SshConnectionArguments,
    model_id: str,
    dpi: int,
    raster_threads: int,
    chunk_pages: int,
    concurrency: int,
    max_tokens: int,
) -> None:
    output_text = output_json.with_suffix(".txt")

    output_json.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    logger.info(f"Processing {input_pdf} with model {model_id}; "
                f"JSON output: {output_json}; text output: {output_text}")

    ssh = paramiko.SSHClient()
    ssh.load_system_host_keys()
    ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())

    ssh.connect(
        hostname=ssh_arguments.host,
        port=ssh_arguments.port,
        username=ssh_arguments.user,
        key_filename=str(ssh_arguments.key_file),
        look_for_keys=False,
        allow_agent=False,
        timeout=30,
    )

    logger.info(f"Connected to {ssh_arguments.user}@"
                f"{ssh_arguments.host}:{ssh_arguments.port}")

    remote_job_directory: str | None = None

    try:
        remote_job_directory = create_remote_job_directory(ssh)

        with tempfile.TemporaryDirectory(
                prefix="unlimited-ocr-") as temporary_directory:
            rendered_directory = Path(temporary_directory) / "rendered"

            page_files = render_pdf_pages(
                input_pdf=input_pdf,
                destination=rendered_directory,
                dpi=dpi,
                raster_threads=raster_threads,
            )

            sftp = ssh.open_sftp()

            try:
                remote_paths = upload_pages(
                    sftp=sftp,
                    page_files=page_files,
                    remote_job_directory=remote_job_directory,
                )
            finally:
                sftp.close()

            chunks = await process_ocr_chunks(
                remote_paths=remote_paths,
                output_text=output_text,
                vllm_url=vllm_url,
                model_id=model_id,
                chunk_pages=chunk_pages,
                concurrency=concurrency,
                max_tokens=max_tokens,
            )

            result = OcrDocumentResult(
                source_file=str(input_pdf.resolve()),
                model=model_id,
                chunks=chunks,
            )

            output_json.write_text(
                result.model_dump_json(indent=2),
                encoding="utf-8",
            )

            logger.info(f"Wrote {len(chunks)} OCR chunks to {output_json}")
    finally:
        if remote_job_directory is not None:
            remove_remote_job_directory(
                ssh=ssh,
                remote_directory=remote_job_directory,
            )

        ssh.close()
        logger.info("Closed SSH connection")


@click.command(context_settings={
    "show_default": True,
})
@click.option(
    "--input",
    "input_pdf",
    required=True,
    type=click.Path(
        path_type=Path,
        exists=True,
        file_okay=True,
        dir_okay=False,
        readable=True,
    ),
    help="Input PDF path.",
)
@click.option(
    "--output",
    "output_json",
    required=True,
    type=click.Path(
        path_type=Path,
        file_okay=True,
        dir_okay=False,
        writable=True,
    ),
    help="Destination JSON path.",
)
@click.option(
    "--vllm-url",
    required=True,
    help="vLLM OpenAI base URL ending with /v1.",
)
@click.option(
    "--ssh-host",
    required=True,
    help="RunPod direct TCP SSH hostname or IP address.",
)
@click.option(
    "--ssh-port",
    required=True,
    type=click.IntRange(min=1),
    help="RunPod externally mapped SSH port.",
)
@click.option(
    "--ssh-user",
    required=True,
    help="SSH user.",
)
@click.option(
    "--ssh-key",
    required=True,
    type=click.Path(
        path_type=Path,
        exists=True,
        file_okay=True,
        dir_okay=False,
        readable=True,
    ),
    help="SSH private-key path.",
)
@click.option(
    "--model",
    "model_id",
    default=DEFAULT_MODEL_ID,
    help="Served model name.",
)
@click.option(
    "--dpi",
    default=DEFAULT_DPI,
    type=click.IntRange(min=1),
    help="PDF rendering DPI.",
)
@click.option(
    "--raster-threads",
    default=DEFAULT_RASTER_THREADS,
    type=click.IntRange(min=1),
    help="Number of concurrent PDF rasterization threads.",
)
@click.option(
    "--chunk-pages",
    default=DEFAULT_CHUNK_PAGES,
    type=click.IntRange(min=1),
    help="Maximum number of images included in each OCR request.",
)
@click.option(
    "--concurrency",
    default=DEFAULT_CONCURRENCY,
    type=click.IntRange(min=1),
    help="Maximum number of concurrent vLLM requests.",
)
@click.option(
    "--max-tokens",
    default=DEFAULT_MAX_TOKENS,
    type=click.IntRange(min=1),
    help="Maximum generated tokens per OCR request.",
)
def main(
    input_pdf: Path,
    output_json: Path,
    vllm_url: str,
    ssh_host: str,
    ssh_port: int,
    ssh_user: str,
    ssh_key: Path,
    model_id: str,
    dpi: int,
    raster_threads: int,
    chunk_pages: int,
    concurrency: int,
    max_tokens: int,
) -> None:
    if input_pdf.suffix.lower() != ".pdf":
        raise click.BadParameter(
            "input must be a PDF file",
            param_hint="--input",
        )

    if output_json.suffix.lower() != ".json":
        raise click.BadParameter(
            "output must use the .json extension",
            param_hint="--output",
        )

    if not vllm_url.rstrip("/").endswith("/v1"):
        raise click.BadParameter(
            "URL must end with /v1",
            param_hint="--vllm-url",
        )

    ssh_arguments = SshConnectionArguments(
        host=ssh_host,
        port=ssh_port,
        user=ssh_user,
        key_file=ssh_key,
    )

    try:
        asyncio.run(
            process_pdf(
                input_pdf=input_pdf,
                output_json=output_json,
                vllm_url=vllm_url,
                ssh_arguments=ssh_arguments,
                model_id=model_id,
                dpi=dpi,
                raster_threads=raster_threads,
                chunk_pages=chunk_pages,
                concurrency=concurrency,
                max_tokens=max_tokens,
            ))
    except Exception:
        logger.exception("OCR processing failed")
        raise

    logger.info("OCR processing finished successfully")


if __name__ == "__main__":
    main()
