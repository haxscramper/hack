#!/usr/bin/env python
# /// script
# dependencies = [
#   "pydantic",
#   "requests",
# ]
# ///

import base64
import json
import logging
import mimetypes
import os
from pathlib import Path
from typing import Any, Literal

import requests
from pydantic import BaseModel, ValidationError

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
)

OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions"
MODEL_NAME = "google/gemini-3.1-pro-preview"
MAX_ATTEMPTS = 3


class ChequeItem(BaseModel):
    original_name: str
    english_name: str
    overall_price: float
    price_type: Literal["weight"] | Literal["pieces"]
    quantity: str
    item_id: str | None = None


class Cheque(BaseModel):
    date: str
    shop: str
    items: list[ChequeItem]
    original_plaintext: str
    translated_plaintext: str


def image_to_data_url(image_path: Path) -> str:
    mime_type, _ = mimetypes.guess_type(image_path.name)
    if mime_type is None:
        raise ValueError(f"Could not determine MIME type for {image_path}")
    encoded = base64.b64encode(image_path.read_bytes()).decode("ascii")
    return f"data:{mime_type};base64,{encoded}"


def extract_json_from_text(text: str) -> Any:
    text = text.strip()
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        pass

    start = text.find("{")
    end = text.rfind("}")
    if start == -1 or end == -1 or end <= start:
        raise ValueError("No JSON object found in model response")

    return json.loads(text[start:end + 1])


def build_initial_prompt(schema_json: dict[str, Any]) -> str:
    return (
        "You are extracting structured data from a shop cheque image.\n"
        "Return only a JSON object that exactly matches this Pydantic-exported JSON schema.\n"
        "Do not include markdown, explanations, or any extra text.\n"
        "If a field is not visible, infer only when clearly justified by the image; otherwise use null only where schema allows it.\n"
        "Put the full original untranslated text of the cheque in unstructured form on original_plaintext field.\n"
        "Put the full translated text of the cheque in unstructured form on the translated_plaintext field.\n"
        "Keep item names exactly as written for original_name.\n"
        "Translate item names to English for english_name.\n"
        "Put the full line price into overall_price.\n"
        "Choose price_type based on the entry type.\n"
        "Put the quantity exactly as shown, including units like kg/g/l/pcs when present.\n"
        "Use item_id if a numeric code present on the individual item on the cheque.\n\n"
        "Schema:\n"
        f"{json.dumps(schema_json, ensure_ascii=False, indent=2)}")


def build_retry_prompt(
    schema_json: dict[str, Any],
    last_response: str,
    validation_error: str,
) -> str:
    return ("Here is an image.\n"
            "Here is your last response:\n"
            f"{last_response}\n\n"
            "Here is why the response is wrong:\n"
            f"{validation_error}\n\n"
            "Return a corrected response as JSON only.\n"
            "It must exactly match this Pydantic-exported JSON schema.\n"
            "Do not include markdown, explanations, or any extra text.\n\n"
            "Schema:\n"
            f"{json.dumps(schema_json, ensure_ascii=False, indent=2)}")


def call_openrouter(
    api_key: str,
    image_path: Path,
    prompt: str,
) -> str:
    data_url = image_to_data_url(image_path)
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json",
    }
    payload = {
        "model":
        MODEL_NAME,
        "messages": [{
            "role":
            "user",
            "content": [
                {
                    "type": "text",
                    "text": prompt,
                },
                {
                    "type": "image_url",
                    "image_url": {
                        "url": data_url,
                    },
                },
            ],
        }],
    }

    logging.debug("Sending request to OpenRouter for %s", image_path)
    response = requests.post(OPENROUTER_API_URL,
                             headers=headers,
                             json=payload,
                             timeout=180)
    response.raise_for_status()
    response_json = response.json()
    logging.debug("Received response for %s", image_path)

    return response_json["choices"][0]["message"]["content"]


def process_image(api_key: str, image_path: Path) -> None:
    output_path = image_path.with_suffix(".json")
    if output_path.exists():
        logging.info("Skipping %s because %s already exists", image_path,
                     output_path)
        return

    schema_json = Cheque.model_json_schema()
    prompt = build_initial_prompt(schema_json)
    last_response_text = ""

    for attempt in range(1, MAX_ATTEMPTS + 1):
        logging.info("Processing %s attempt %d/%d", image_path, attempt,
                     MAX_ATTEMPTS)
        response_text = call_openrouter(api_key, image_path, prompt)
        last_response_text = response_text

        try:
            response_json = extract_json_from_text(response_text)
            cheque = Cheque.model_validate(response_json)
            output_path.write_text(
                json.dumps(cheque.model_dump(), ensure_ascii=False, indent=2),
                encoding="utf-8",
            )
            logging.info("Saved parsed cheque JSON to %s", output_path)
            return
        except (ValidationError, ValueError, json.JSONDecodeError) as exc:
            logging.warning("Validation failed for %s on attempt %d: %s",
                            image_path, attempt, exc)
            if attempt == MAX_ATTEMPTS:
                raise
            prompt = build_retry_prompt(
                schema_json=schema_json,
                last_response=last_response_text,
                validation_error=str(exc),
            )

    raise RuntimeError(f"Failed to process {image_path}")


def iter_image_files(input_dir: Path) -> list[Path]:
    exts = {".jpg", ".jpeg", ".png", ".webp", ".bmp", ".tiff", ".tif"}
    return sorted(path for path in input_dir.iterdir()
                  if path.is_file() and path.suffix.lower() in exts)


def main() -> None:
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir", type=Path)
    args = parser.parse_args()

    api_key = os.environ["OPENROUTER_API_KEY"]
    input_dir = args.input_dir

    if not input_dir.is_dir():
        raise NotADirectoryError(f"Not a directory: {input_dir}")

    image_files = iter_image_files(input_dir)
    logging.info("Found %d image files in %s", len(image_files), input_dir)

    for image_path in image_files:
        process_image(api_key, image_path)


if __name__ == "__main__":
    main()
