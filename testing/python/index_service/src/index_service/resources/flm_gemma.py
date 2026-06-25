import os
from dataclasses import dataclass

from beartype import beartype
from beartype.typing import Dict
from index_service.harness import ResourceHarness
from openai import OpenAI


@beartype
@dataclass
class FlmSummaryResult:
    summary: str


@beartype
def _make_handler(client: OpenAI, model: str):

    @beartype
    def handle(payload: Dict[str, object]) -> FlmSummaryResult:
        text = payload["text"]
        response = client.chat.completions.create(
            model=model,
            messages=[
                {
                    "role":
                    "system",
                    "content":
                    "You summarize text files in 2-4 concise sentences.",
                },
                {
                    "role": "user",
                    "content": text,
                },
            ],
        )
        summary = response.choices[0].message.content.strip()
        return FlmSummaryResult(summary=summary)

    return handle


if __name__ == "__main__":
    base_url = os.environ.get("FLM_BASE_URL", "http://127.0.0.1:52625/v1")
    model = os.environ.get("FLM_MODEL", "gemma4-it:e4b")
    client = OpenAI(base_url=base_url, api_key="flm")
    ResourceHarness("flm-gemma", _make_handler(client, model)).run()
