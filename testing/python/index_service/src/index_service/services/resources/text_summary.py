# index_service/services/resources/text_summary.py
import logging

import tiktoken
from langchain_text_splitters import TokenTextSplitter
from pydantic import BaseModel, Field

log = logging.getLogger(__name__)

from index_service.services.core.job_types import (
    BaseResource,
    RunContext,
)
from index_service.services.resources.flm_server import (
    FlmMessage,
    FlmRequest,
    FlmServerResource,
)

_SUMMARIZE_SYSTEM_PROMPT = "You summarize text files in 5-10 concise sentences."
_COMBINE_SYSTEM_PROMPT = (
    "You are given summaries of consecutive sections of a single text file. "
    "Produce one coherent summary of the whole file in 5-10 concise sentences.")


class SummarizeRequest(BaseModel, extra="forbid"):
    text: str


class ChunkSummary(BaseModel, extra="forbid"):
    index: int
    token_count: int
    summary: str


class TextSummaryResult(BaseModel, extra="forbid"):
    summary: str
    chunked: bool = False
    chunk_count: int = 1
    chunk_token_size: int = 0
    chunk_overlap: int = 0
    total_token_count: int = 1
    chunk_summaries: list[ChunkSummary] = Field(default_factory=list)


class TextSummaryResource(BaseResource):
    resource_key = "text_summary"
    required_resources = ("flm_server",)

    def __init__(
        self,
        model: str = "gemma4-it:e4b",
        context_window_tokens: int = 16_000,
        output_reserve_tokens: int = 4_000,
        chunk_overlap_tokens: int = 200,
        encoding_name: str = "o200k_base",
    ) -> None:
        self._model = model
        self._context_window_tokens = context_window_tokens
        self._output_reserve_tokens = output_reserve_tokens
        self._chunk_overlap_tokens = chunk_overlap_tokens
        self._encoding_name = encoding_name
        # Leave headroom for the system prompt and the generated output so a
        # single chunk plus prompt always fits inside the model context.
        self._chunk_token_size = context_window_tokens - output_reserve_tokens
        self._encoder = tiktoken.get_encoding(encoding_name)

        self._splitter = TokenTextSplitter(
            encoding_name=encoding_name,
            chunk_size=self._chunk_token_size,
            chunk_overlap=chunk_overlap_tokens,
        )

    def _resolve_flm(self, resources: dict[str, BaseResource]) -> FlmServerResource:
        flm = resources.get("flm_server")
        if flm is None:
            raise KeyError("Missing required resource: flm_server")
        if not isinstance(flm, FlmServerResource):
            raise TypeError(
                "Resource `flm_server` must be an instance of FlmServerResource")
        return flm

    def _summarize(
        self,
        ctx: RunContext,
        flm: FlmServerResource,
        resources: dict[str, BaseResource],
        text: str,
        system_prompt: str,
    ) -> str:
        flm_response = flm.handle(
            ctx=ctx,
            request=FlmRequest(
                model=self._model,
                messages=[
                    FlmMessage(role="system", content=system_prompt),
                    FlmMessage(role="user", content=text),
                ],
            ),
            resources=resources,
        )
        return flm_response.content.strip()

    def handle(
        self,
        ctx: RunContext,
        request: SummarizeRequest,
        resources: dict[str, BaseResource],
    ) -> TextSummaryResult:
        flm = self._resolve_flm(resources)

        total_tokens = len(self._encoder.encode(request.text))

        if total_tokens <= self._chunk_token_size:
            summary = self._summarize(
                ctx,
                flm,
                resources,
                request.text,
                _SUMMARIZE_SYSTEM_PROMPT,
            )
            return TextSummaryResult(
                summary=summary,
                chunked=False,
                chunk_count=1,
                chunk_token_size=self._chunk_token_size,
                chunk_overlap=self._chunk_overlap_tokens,
                total_token_count=total_tokens,
                chunk_summaries=[
                    ChunkSummary(
                        index=0,
                        token_count=total_tokens,
                        summary=summary,
                    )
                ],
            )

        chunks = self._splitter.split_text(request.text)
        chunk_summaries: list[ChunkSummary] = []
        for index, chunk in enumerate(chunks):
            chunk_summary = self._summarize(
                ctx,
                flm,
                resources,
                chunk,
                _SUMMARIZE_SYSTEM_PROMPT,
            )
            chunk_summaries.append(
                ChunkSummary(
                    index=index,
                    token_count=len(self._encoder.encode(chunk)),
                    summary=chunk_summary,
                ))

        combined = "\n\n".join(
            f"Section {c.index + 1}: {c.summary}" for c in chunk_summaries)
        final_summary = self._summarize(
            ctx,
            flm,
            resources,
            combined,
            _COMBINE_SYSTEM_PROMPT,
        )

        return TextSummaryResult(
            summary=final_summary,
            chunked=True,
            chunk_count=len(chunk_summaries),
            chunk_token_size=self._chunk_token_size,
            chunk_overlap=self._chunk_overlap_tokens,
            total_token_count=total_tokens,
            chunk_summaries=chunk_summaries,
        )
