import os

from index_service.harness import BaseResourceActor
from openai import OpenAI
from pydantic import BaseModel


class SummarizeRequest(BaseModel):
    text: str


class FlmSummaryResult(BaseModel):
    summary: str


class FlmGemmaResourceActor(BaseResourceActor):
    actor_id = "flm-gemma"

    def __init__(self,
                 client: OpenAI | None = None,
                 model: str | None = None) -> None:
        super().__init__()
        base_url = os.environ.get("FLM_BASE_URL", "http://127.0.0.1:52625/v1")
        self._model = model or os.environ.get("FLM_MODEL", "gemma4-it:e4b")
        self._client = client or OpenAI(base_url=base_url, api_key="flm")

    def handle(self, request: SummarizeRequest) -> FlmSummaryResult:
        response = self._client.chat.completions.create(
            model=self._model,
            messages=[
                {
                    "role":
                    "system",
                    "content":
                    "You summarize text files in 2-4 concise sentences.",
                },
                {
                    "role": "user",
                    "content": request.text,
                },
            ],
        )
        return FlmSummaryResult(
            summary=response.choices[0].message.content.strip())
