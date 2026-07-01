# index_service/services/resources/text_summary.py
import os

from pydantic import BaseModel

from index_service.services.core.job_types import BaseResource, RunContext
from index_service.services.resources.flm_server import (
    FlmMessage,
    FlmRequest,
    FlmServerResource,
)


class SummarizeRequest(BaseModel, extra="forbid"):
    text: str


class TextSummaryResult(BaseModel, extra="forbid"):
    summary: str


class TextSummaryResource(BaseResource):
    resource_key = "text_summary"
    required_resources = ("flm_server", )

    def __init__(self, model: str | None = None) -> None:
        self._model = model or os.environ.get("FLM_MODEL", "gemma4-it:e4b")

    def handle(
        self,
        ctx: RunContext,
        request: SummarizeRequest,
        resources: dict[str, BaseResource],
    ) -> TextSummaryResult:
        flm = resources.get("flm_server")
        if flm is None:
            raise KeyError("Missing required resource: flm_server")
        if not isinstance(flm, FlmServerResource):
            raise TypeError(
                "Resource `flm_server` must be an instance of FlmServerResource"
            )

        flm_response = flm.handle(
            ctx=ctx,
            request=FlmRequest(
                model=self._model,
                messages=[
                    FlmMessage(
                        role="system",
                        content=
                        "You summarize text files in 2-4 concise sentences.",
                    ),
                    FlmMessage(role="user", content=request.text),
                ],
            ),
            resources=resources,
        )

        return TextSummaryResult(summary=flm_response.content.strip())
