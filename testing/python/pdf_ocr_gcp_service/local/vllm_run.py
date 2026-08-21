#!/usr/bin/env python
from __future__ import annotations

import argparse
import asyncio
import re
import shlex
import tempfile
from pathlib import Path
from urllib.parse import quote
from loguru import logger

import pymupdf
import paramiko
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
    destination.mkdir(parents=True, exist_ok=True)

    page_paths: list[Path] = []
    matrix = pymupdf.Matrix(dpi / 72.0, dpi / 72.0)

    with pymupdf.open(input_pdf) as document:
        if document.page_count == 0:
            raise RuntimeError(f"PDF contains no pages: {input_pdf}")

        for page_index, page in enumerate(document, start=1):
            pixmap = page.get_pixmap(
                matrix=matrix,
                alpha=False,
            )
            page_path = destination / f"page_{page_index:06d}.png"
            pixmap.save(page_path.as_posix())
            page_paths.append(page_path)

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


def create_remote_job_directory(ssh: paramiko.SSHClient, ) -> str:
    return run_ssh_command(
        ssh,
        "mkdir -p /tmp/unlimited-ocr && "
        "mktemp -d /tmp/unlimited-ocr/job.XXXXXXXXXX",
    )


def remove_remote_job_directory(
    ssh: paramiko.SSHClient,
    remote_directory: str,
) -> None:
    run_ssh_command(
        ssh,
        f"rm -rf -- {shlex.quote(remote_directory)}",
    )


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
    async with semaphore:
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

    raw_text = response.choices[0].message.content

    if raw_text is None or not raw_text.strip():
        raise RuntimeError(
            f"Model returned empty output for page {page_number}")

    return parse_page_content(
        raw_text=raw_text,
        page_number=page_number,
    )


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

    remote_job_directory: str | None = None

    try:
        sftp = ssh.open_sftp()

        try:
            remote_job_directory = create_remote_job_directory(ssh)

            with tempfile.TemporaryDirectory(
                    prefix="unlimited-ocr-") as temporary_directory:
                rendered_directory = (Path(temporary_directory) / "rendered")
                page_files = render_pdf_pages(
                    input_pdf=input_pdf,
                    destination=rendered_directory,
                    dpi=dpi,
                )

                chunks: list[OcrChunkResult] = []
                semaphore = asyncio.Semaphore(concurrency)

                async with AsyncOpenAI(
                        api_key="EMPTY",
                        base_url=vllm_url.rstrip("/"),
                        timeout=3600,
                ) as client:
                    for chunk_index, start in enumerate(
                            range(0, len(page_files), chunk_pages)):
                        chunk = page_files[start:start + chunk_pages]
                        remote_paths: list[str] = []

                        logger.info(
                            f"Processing pages {start + 1}-"
                            f"{start + len(chunk)} of {len(page_files)}")

                        try:
                            for page_path in chunk:
                                remote_path = (f"{remote_job_directory}/"
                                               f"{page_path.name}")
                                sftp.put(
                                    str(page_path),
                                    remote_path,
                                    confirm=True,
                                )
                                remote_paths.append(remote_path)

                            pages = await asyncio.gather(*(recognize_page(
                                client=client,
                                model_id=model_id,
                                remote_path=remote_path,
                                page_number=start + index + 1,
                                max_tokens=max_tokens,
                                semaphore=semaphore,
                            ) for index, remote_path in enumerate(remote_paths)
                                                           ))
                        finally:
                            for remote_path in remote_paths:
                                try:
                                    sftp.remove(remote_path)
                                except FileNotFoundError:
                                    pass

                        chunks.append(
                            OcrChunkResult(
                                chunk_index=chunk_index,
                                page_start=start + 1,
                                page_end=start + len(chunk),
                                raw_text="<PAGE>".join(page.raw_text
                                                       for page in pages),
                                pages=pages,
                            ))

                result = OcrDocumentResult(
                    source_file=str(input_pdf.resolve()),
                    model=model_id,
                    chunks=chunks,
                )

                output_json.parent.mkdir(
                    parents=True,
                    exist_ok=True,
                )
                output_json.write_text(
                    result.model_dump_json(indent=2),
                    encoding="utf-8",
                )
        finally:
            sftp.close()
    finally:
        if remote_job_directory is not None:
            remove_remote_job_directory(
                ssh,
                remote_job_directory,
            )

        ssh.close()


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
        help=("Number of rendered pages uploaded per chunk; "
              f"default: {DEFAULT_CHUNK_PAGES}"),
    )
    parser.add_argument(
        "--concurrency",
        default=DEFAULT_CONCURRENCY,
        type=positive_integer,
        help=("Maximum concurrent vLLM requests; "
              f"default: {DEFAULT_CONCURRENCY}"),
    )
    parser.add_argument(
        "--max-tokens",
        default=DEFAULT_MAX_TOKENS,
        type=positive_integer,
        help=("Maximum generated tokens per page; "
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
    arguments = parse_arguments()

    ssh_arguments = SshConnectionArguments(
        host=arguments.ssh_host,
        port=arguments.ssh_port,
        user=arguments.ssh_user,
        key_file=arguments.ssh_key,
    )

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


if __name__ == "__main__":
    main()
