#!/usr/bin/env python
from __future__ import annotations

import argparse
import asyncio
import re
import shlex
import sys
import tempfile
import time
from pathlib import Path
from urllib.parse import quote

import paramiko
import pymupdf
from loguru import logger
from openai import AsyncOpenAI
from pydantic import BaseModel, Field

DEFAULT_MODEL_ID = "baidu/Unlimited-OCR"
DEFAULT_CHUNK_PAGES = 16
DEFAULT_CONCURRENCY = 4
DEFAULT_DPI = 300
DEFAULT_MAX_TOKENS = 8192

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
    logger.debug(f"Parsing OCR response for page {page_number}, "
                 f"response length is {len(raw_text)} characters")

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

    logger.info(
        f"Parsed page {page_number}: found {len(elements)} OCR elements")

    return OcrPage(
        page_number=page_number,
        raw_text=raw_text,
        elements=elements,
    )


def render_pdf_pages(
    input_pdf: Path,
    destination: Path,
    dpi: int,
) -> list[Path]:
    started_at = time.monotonic()

    logger.info(f"Opening PDF file {input_pdf}")
    logger.info(f"Input PDF size is {input_pdf.stat().st_size}")

    destination.mkdir(parents=True, exist_ok=True)

    logger.debug(f"Rendered pages will be stored in {destination}")
    logger.info(f"Rendering PDF pages at {dpi} DPI")

    page_paths: list[Path] = []
    matrix = pymupdf.Matrix(dpi / 72.0, dpi / 72.0)

    with pymupdf.open(input_pdf) as document:
        if document.page_count == 0:
            raise RuntimeError(f"PDF contains no pages: {input_pdf}")

        logger.info(f"PDF contains {document.page_count} pages")

        for page_index, page in enumerate(document, start=1):

            logger.info(
                f"Rendering page {page_index} of {document.page_count}")

            pixmap = page.get_pixmap(
                matrix=matrix,
                alpha=False,
            )
            page_path = destination / f"page_{page_index:06d}.png"
            pixmap.save(page_path.as_posix())
            page_paths.append(page_path)

            file_size = page_path.stat().st_size

            logger.info(f"Rendered page {page_index} to {page_path.name}, "
                        f"dimensions are {pixmap.width} by {pixmap.height}, "
                        f"size is {file_size}")
    total_size = sum(path.stat().st_size for path in page_paths)

    logger.info(f"Finished rendering {len(page_paths)} pages, "
                f"total rendered size is {total_size}")

    return page_paths


def run_ssh_command(
    ssh: paramiko.SSHClient,
    command: str,
) -> str:
    logger.debug(f"Executing remote command: {command}")

    _, stdout, stderr = ssh.exec_command(command)
    status = stdout.channel.recv_exit_status()

    output = stdout.read().decode("utf-8")
    error = stderr.read().decode("utf-8")

    logger.debug(f"Remote command completed with status {status}")

    if status != 0:
        raise RuntimeError(f"Remote command failed with status {status}: "
                           f"{command}\n{error.strip()}")

    return output.strip()


def create_remote_job_directory(ssh: paramiko.SSHClient) -> str:
    logger.info("Creating remote OCR job directory")

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
    logger.info(f"Removing remote job directory {remote_directory}")

    run_ssh_command(
        ssh,
        f"rm -rf -- {shlex.quote(remote_directory)}",
    )

    logger.info(f"Removed remote job directory {remote_directory}")


def remote_file_url(remote_path: str) -> str:
    return f"file://{quote(remote_path, safe='/')}"


async def recognize_page(
    client: AsyncOpenAI,
    model_id: str,
    remote_path: str,
    page_number: int,
    max_tokens: int,
    semaphore: asyncio.Semaphore,
) -> OcrPage:
    queued_at = time.monotonic()

    logger.info(f"Page {page_number} is queued for OCR using {remote_path}")

    if semaphore.locked():
        logger.info(f"Page {page_number} is waiting for an available "
                    f"OCR request slot")

    async with semaphore:
        logger.info(f"Page {page_number} acquired an OCR request slot")
        logger.info(f"Sending page {page_number} to model {model_id} "
                    f"with a maximum of {max_tokens} generated tokens")
        logger.debug(f"Page {page_number} image URL is "
                     f"{remote_file_url(remote_path)}")
        logger.info(f"Waiting for the OCR response for page {page_number}")

        request_started_at = time.monotonic()

        response = await client.chat.completions.create(
            model=model_id,
            messages=[{
                "role":
                "user",
                "content": [
                    {
                        "type": "text",
                        "text": "<image>document parsing.",
                    },
                    {
                        "type": "image_url",
                        "image_url": {
                            "url": remote_file_url(remote_path),
                        },
                    },
                ],
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

    logger.info(f"Received OCR response for page {page_number}")
    logger.debug(f"Page {page_number} response identifier is {response.id}")

    choice = response.choices[0]
    raw_text = choice.message.content

    logger.debug(f"Page {page_number} completion finish reason is "
                 f"{choice.finish_reason}")

    if response.usage is not None:
        logger.info(f"Page {page_number} token usage: "
                    f"{response.usage.prompt_tokens} prompt tokens, "
                    f"{response.usage.completion_tokens} completion tokens, "
                    f"{response.usage.total_tokens} total tokens")

    if raw_text is None or not raw_text.strip():
        raise RuntimeError(
            f"Model returned empty output for page {page_number}")

    logger.info(
        f"Page {page_number} returned {len(raw_text)} characters of OCR text")

    page = parse_page_content(
        raw_text=raw_text,
        page_number=page_number,
    )

    logger.info(f"Finished OCR processing for page {page_number}")

    return page


async def process_pdf(
    input_pdf: Path,
    output_json: Path,
    vllm_url: str,
    ssh_arguments: SshConnectionArguments,
    model_id: str,
    dpi: int,
    chunk_pages: int,
    concurrency: int,
    max_tokens: int,
) -> None:
    process_started_at = time.monotonic()

    logger.info(f"Starting OCR processing for {input_pdf}")
    logger.info(f"Output JSON will be written to {output_json}")
    logger.info(f"Using vLLM endpoint {vllm_url.rstrip('/')}")
    logger.info(f"Using model {model_id}")
    logger.info(
        f"Configuration: DPI is {dpi}, chunk size is {chunk_pages} pages, "
        f"concurrency is {concurrency}, maximum tokens are {max_tokens}")
    logger.info(f"Connecting to SSH host {ssh_arguments.user}@"
                f"{ssh_arguments.host}:{ssh_arguments.port}")
    logger.debug(f"Using SSH key {ssh_arguments.key_file}")

    ssh = paramiko.SSHClient()
    ssh.load_system_host_keys()
    ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())

    ssh_started_at = time.monotonic()

    ssh.connect(
        hostname=ssh_arguments.host,
        port=ssh_arguments.port,
        username=ssh_arguments.user,
        key_filename=str(ssh_arguments.key_file),
        look_for_keys=False,
        allow_agent=False,
        timeout=30,
    )

    logger.info(f"SSH connection established")

    remote_job_directory: str | None = None

    try:
        logger.info("Opening SFTP session")
        sftp = ssh.open_sftp()
        logger.info("SFTP session opened")

        try:
            remote_job_directory = create_remote_job_directory(ssh)

            with tempfile.TemporaryDirectory(
                    prefix="unlimited-ocr-") as temporary_directory:
                logger.debug(
                    f"Created local temporary directory {temporary_directory}")

                rendered_directory = Path(temporary_directory) / "rendered"
                page_files = render_pdf_pages(
                    input_pdf=input_pdf,
                    destination=rendered_directory,
                    dpi=dpi,
                )

                total_chunks = (len(page_files) + chunk_pages -
                                1) // chunk_pages

                logger.info(f"Processing {len(page_files)} pages in "
                            f"{total_chunks} chunks")

                chunks: list[OcrChunkResult] = []
                semaphore = asyncio.Semaphore(concurrency)

                logger.info("Creating vLLM OpenAI client")

                async with AsyncOpenAI(
                        api_key="EMPTY",
                        base_url=vllm_url.rstrip("/"),
                        timeout=3600,
                ) as client:
                    logger.info("vLLM OpenAI client is ready")

                    for chunk_index, start in enumerate(
                            range(0, len(page_files), chunk_pages)):
                        chunk_started_at = time.monotonic()
                        chunk = page_files[start:start + chunk_pages]
                        remote_paths: list[str] = []
                        chunk_number = chunk_index + 1
                        page_start = start + 1
                        page_end = start + len(chunk)

                        logger.info(
                            f"Starting chunk {chunk_number} of {total_chunks}, "
                            f"containing pages {page_start} through {page_end}"
                        )

                        try:
                            for index, page_path in enumerate(chunk, start=1):
                                remote_path = (
                                    f"{remote_job_directory}/{page_path.name}")
                                file_size = page_path.stat().st_size

                                logger.info(f"Uploading page {start + index} "
                                            f"for chunk {chunk_number}: "
                                            f"{page_path.name}, "
                                            f"size is {file_size}")

                                sftp.put(
                                    str(page_path),
                                    remote_path,
                                    confirm=True,
                                )
                                remote_paths.append(remote_path)

                                logger.info(
                                    f"Uploaded page {start + index} to "
                                    f"{remote_path}")

                            logger.info(
                                f"All {len(remote_paths)} files for chunk "
                                f"{chunk_number} are uploaded")
                            logger.info(
                                f"Submitting {len(remote_paths)} OCR requests "
                                f"for chunk {chunk_number} with concurrency "
                                f"limited to {concurrency}")

                            pages = await asyncio.gather(*(recognize_page(
                                client=client,
                                model_id=model_id,
                                remote_path=remote_path,
                                page_number=start + index + 1,
                                max_tokens=max_tokens,
                                semaphore=semaphore,
                            ) for index, remote_path in enumerate(remote_paths)
                                                           ))

                            logger.info(f"Received all OCR results for chunk "
                                        f"{chunk_number}")
                        finally:
                            if remote_paths:
                                logger.info(
                                    f"Removing {len(remote_paths)} uploaded "
                                    f"files for chunk {chunk_number}")

                            for remote_path in remote_paths:
                                try:
                                    logger.debug(
                                        f"Removing remote file {remote_path}")
                                    sftp.remove(remote_path)
                                    logger.debug(
                                        f"Removed remote file {remote_path}")
                                except FileNotFoundError:
                                    logger.warning(
                                        f"Remote file was already absent: "
                                        f"{remote_path}")

                        chunks.append(
                            OcrChunkResult(
                                chunk_index=chunk_index,
                                page_start=page_start,
                                page_end=page_end,
                                raw_text="<PAGE>".join(page.raw_text
                                                       for page in pages),
                                pages=pages,
                            ))

                        logger.info(
                            f"Finished chunk {chunk_number} of {total_chunks}")

                logger.info(f"Building final result from {len(chunks)} chunks")

                result = OcrDocumentResult(
                    source_file=str(input_pdf.resolve()),
                    model=model_id,
                    chunks=chunks,
                )

                logger.info(f"Creating output directory {output_json.parent}")

                output_json.parent.mkdir(
                    parents=True,
                    exist_ok=True,
                )

                logger.info(f"Serializing OCR result to {output_json}")

                serialized_result = result.model_dump_json(indent=2)

                logger.info(
                    f"Writing {len(serialized_result.encode('utf-8'))} "
                    f"of JSON output")

                output_json.write_text(
                    serialized_result,
                    encoding="utf-8",
                )

                logger.info(f"Wrote OCR result to {output_json}, "
                            f"file size is "
                            f"{output_json.stat().st_size}")
        finally:
            logger.info("Closing SFTP session")
            sftp.close()
            logger.info("SFTP session closed")
    finally:
        if remote_job_directory is not None:
            remove_remote_job_directory(
                ssh,
                remote_job_directory,
            )

        logger.info("Closing SSH connection")
        ssh.close()
        logger.info("SSH connection closed")

    logger.info(f"Completed OCR processing for {input_pdf}")


def positive_integer(value: str) -> int:
    parsed = int(value)

    if parsed <= 0:
        raise argparse.ArgumentTypeError("value must be greater than zero")

    return parsed


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=("Render a PDF and process its pages using Unlimited-OCR "
                     "served by a remote vLLM instance."))

    parser.add_argument(
        "--input",
        required=True,
        type=Path,
        help="Input PDF path",
    )
    parser.add_argument(
        "--output",
        required=True,
        type=Path,
        help="Destination JSON path",
    )
    parser.add_argument(
        "--vllm-url",
        required=True,
        help=("vLLM OpenAI base URL, for example "
              "https://POD_ID-8000.proxy.runpod.net/v1"),
    )
    parser.add_argument(
        "--ssh-host",
        required=True,
        help="RunPod direct TCP SSH hostname or IP address",
    )
    parser.add_argument(
        "--ssh-port",
        required=True,
        type=positive_integer,
        help="RunPod externally mapped SSH port",
    )
    parser.add_argument(
        "--ssh-user",
        required=True,
        help="SSH user, normally root",
    )
    parser.add_argument(
        "--ssh-key",
        required=True,
        type=Path,
        help="SSH private-key path",
    )
    parser.add_argument(
        "--model",
        default=DEFAULT_MODEL_ID,
        help=f"Served model name; default: {DEFAULT_MODEL_ID}",
    )
    parser.add_argument(
        "--dpi",
        default=DEFAULT_DPI,
        type=positive_integer,
        help=f"PDF rendering DPI; default: {DEFAULT_DPI}",
    )
    parser.add_argument(
        "--chunk-pages",
        default=DEFAULT_CHUNK_PAGES,
        type=positive_integer,
        help=(f"Number of rendered pages uploaded per chunk; "
              f"default: {DEFAULT_CHUNK_PAGES}"),
    )
    parser.add_argument(
        "--concurrency",
        default=DEFAULT_CONCURRENCY,
        type=positive_integer,
        help=(f"Maximum concurrent vLLM requests; "
              f"default: {DEFAULT_CONCURRENCY}"),
    )
    parser.add_argument(
        "--max-tokens",
        default=DEFAULT_MAX_TOKENS,
        type=positive_integer,
        help=(f"Maximum generated tokens per page; "
              f"default: {DEFAULT_MAX_TOKENS}"),
    )

    arguments = parser.parse_args()

    if not arguments.input.is_file():
        parser.error(f"Input PDF does not exist: {arguments.input}")

    if arguments.input.suffix.lower() != ".pdf":
        parser.error("--input must refer to a PDF file")

    if not arguments.ssh_key.is_file():
        parser.error(f"SSH private key does not exist: {arguments.ssh_key}")

    if not arguments.vllm_url.rstrip("/").endswith("/v1"):
        parser.error("--vllm-url must end with /v1")

    return arguments


def main() -> None:
    logger.info("Unlimited-OCR PDF processor started")

    arguments = parse_arguments()

    logger.debug(f"Input argument is {arguments.input}")
    logger.debug(f"Output argument is {arguments.output}")
    logger.debug(f"vLLM URL argument is {arguments.vllm_url}")
    logger.debug(f"SSH destination is {arguments.ssh_user}@"
                 f"{arguments.ssh_host}:{arguments.ssh_port}")
    logger.debug(f"Model argument is {arguments.model}")
    logger.debug(f"DPI argument is {arguments.dpi}")
    logger.debug(f"Chunk pages argument is {arguments.chunk_pages}")
    logger.debug(f"Concurrency argument is {arguments.concurrency}")
    logger.debug(f"Maximum tokens argument is {arguments.max_tokens}")

    ssh_arguments = SshConnectionArguments(
        host=arguments.ssh_host,
        port=arguments.ssh_port,
        user=arguments.ssh_user,
        key_file=arguments.ssh_key,
    )

    try:
        asyncio.run(
            process_pdf(
                input_pdf=arguments.input,
                output_json=arguments.output,
                vllm_url=arguments.vllm_url,
                ssh_arguments=ssh_arguments,
                model_id=arguments.model,
                dpi=arguments.dpi,
                chunk_pages=arguments.chunk_pages,
                concurrency=arguments.concurrency,
                max_tokens=arguments.max_tokens,
            ))
    except Exception:
        logger.exception("OCR processing failed")
        raise

    logger.info("Unlimited-OCR PDF processor finished successfully")


if __name__ == "__main__":
    main()
