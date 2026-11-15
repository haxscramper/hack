import argparse
import base64
import mimetypes
import os
import sys
import traceback
from pathlib import Path

import pandas as pd
from beartype import beartype
from openai import OpenAI
from tqdm import tqdm

MODEL = "deepseek/deepseek-v4.1-flash-1017"
PROVIDER = "deepinfra/fp8"


@beartype
def image_data_url(image: Path) -> str:
    media_type, _ = mimetypes.guess_file_type(image)
    if media_type is None or not media_type.startswith("image/"):
        raise ValueError(
            f"Cannot caption {image}: its extension does not identify an image "
            f"MIME type; detected {media_type!r}"
        )

    encoded = base64.b64encode(image.read_bytes()).decode("ascii")
    return f"data:{media_type};base64,{encoded}"


@beartype
def generate_caption(client: OpenAI, input_path: Path, prompt: str) -> str:
    response = client.chat.completions.create(
        model=MODEL,
        extra_body={
            "provider": {
                "order": [PROVIDER],
                "allow_fallbacks": False,
            },
        },
        messages=[
            {"role": "system", "content": prompt},
            {"role": "user", "content": input_path.read_text()},
        ],
    )

    if not response.choices:
        raise ValueError(
            f"Cannot rewrite {input_path}: model {MODEL!r} returned no choices"
        )

    choice = response.choices[0]
    if choice.finish_reason != "stop":
        raise ValueError(
            f"Cannot save rewrite for {input_path}: model {MODEL!r} did not finish "
            f"normally; finish reason was {choice.finish_reason!r}"
        )

    content = choice.message.content
    if content is None or not content.strip():
        raise ValueError(
            f"Cannot rewrite {input_path}: model {MODEL!r} returned empty content"
        )

    return content.strip()


@beartype
def main() -> None:
    parser = argparse.ArgumentParser(description="Caption images through OpenRouter.")
    parser.add_argument("prompt", type=Path, help="Markdown prompt file")
    parser.add_argument("csv", type=Path, help="CSV containing image file paths")
    parser.add_argument(
        "--column", default="path", help="Column containing image paths"
    )
    args = parser.parse_args()

    prompt = args.prompt.read_text()
    frame = pd.read_csv(args.csv, dtype={args.column: "string"})

    if args.column not in frame.columns:
        raise ValueError(
            f"Cannot read image paths from {args.csv}: "
            f"column {args.column!r} is missing; "
            f"available columns: {frame.columns.tolist()!r}"
        )

    paths = frame[args.column]
    missing = paths.isna() | paths.str.strip().eq("")
    if missing.any():
        rows = (frame.index[missing] + 1).tolist()
        raise ValueError(
            f"Cannot read image paths from {args.csv}: "
            f"column {args.column!r} contains empty paths "
            f"at data rows {rows!r} (numbered from 1, excluding the header)"
        )

    with OpenAI(
        base_url="https://openrouter.ai/api/v1",
        api_key=os.environ["OPENROUTER_API_KEY"],
    ) as client:
        for path in tqdm(
            paths,
            total=len(paths),
            desc="Rewriting captions",
            unit="file",
        ):
            input_path = Path(path)

            try:
                output_path = input_path.with_name(input_path.name + ".updated")
                if output_path.exists():
                    continue

                content = generate_caption(client, input_path, prompt)
                output_path.write_text(content)

            except Exception:
                with tqdm.external_write_mode(file=sys.stderr):
                    print(f"Failed to rewrite {input_path}:", file=sys.stderr)
                    traceback.print_exc()


if __name__ == "__main__":
    main()
