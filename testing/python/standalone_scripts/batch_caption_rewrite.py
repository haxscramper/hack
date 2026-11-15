import argparse
import json
import sys
from pathlib import Path

import httpx
import pandas as pd
from beartype import beartype
from loguru import logger
from tqdm import tqdm


@beartype
def main() -> None:
    parser = argparse.ArgumentParser(
        description="Process text files through a local llama-server."
    )
    parser.add_argument("prompt", type=Path, help="Markdown prompt file")
    parser.add_argument("csv", type=Path, help="CSV containing input file paths")
    parser.add_argument("--column", default="path", help="Column containing file paths")
    parser.add_argument("--server", default="http://127.0.0.1:9931")
    args = parser.parse_args()

    prompt = args.prompt.read_text()
    frame = pd.read_csv(args.csv, dtype={args.column: "string"})

    if args.column not in frame.columns:
        raise ValueError(
            f"Cannot read input paths from {args.csv}: "
            f"column {args.column!r} is missing; "
            f"available columns: {frame.columns.tolist()!r}"
        )

    paths = frame[args.column]
    missing = paths.isna() | paths.str.strip().eq("")
    if missing.any():
        rows = (frame.index[missing] + 1).tolist()
        raise ValueError(
            f"Cannot read input paths from {args.csv}: "
            f"column {args.column!r} contains empty paths "
            f"at data rows {rows!r} (numbered from 1, excluding the header)"
        )

    with httpx.Client(
        base_url=args.server.rstrip("/") + "/",
        timeout=None,
    ) as client:
        for path in tqdm(
            paths, total=len(paths), desc="Rewriting captions", unit="file"
        ):
            input_path = Path(path)
            fragments: list[str] = []

            with client.stream(
                "POST",
                "v1/chat/completions",
                json={
                    "messages": [
                        {"role": "system", "content": prompt},
                        {"role": "user", "content": input_path.read_text()},
                    ],
                    "chat_template_kwargs": {"enable_thinking": True},
                    # Copied from the llama.cpp web UI request
                    "thinking_budget_tokens": 512,
                    "reasoning_control": True,
                    "backend_sampling": False,
                    "timings_per_token": True,
                    "stream": True,
                    "cache_prompt": True,
                },
            ) as response:
                response.raise_for_status()

                for line in response.iter_lines():
                    if not line.startswith("data:"):
                        continue

                    data = line.removeprefix("data:").strip()
                    if data == "[DONE]":
                        break

                    event = json.loads(data)
                    if "error" in event:
                        raise RuntimeError(
                            f"Cannot complete rewrite for {input_path}: "
                            f"server returned {event['error']!r}"
                        )

                    for choice in event["choices"]:
                        fragment = choice["delta"].get("content") or choice[
                            "delta"
                        ].get("reasoning_content")
                        if fragment is None:
                            continue

                        if not isinstance(fragment, str):
                            raise TypeError(
                                f"Cannot stream response for {input_path}: "
                                f"expected textual content, received {fragment!r}"
                            )

                        fragments.append(fragment)
                        sys.stdout.write(fragment)
                        sys.stdout.flush()

            sys.stdout.write("\n")
            sys.stdout.flush()
            content = "".join(fragments)

            output_path = input_path.with_name(input_path.name + ".updated")
            output_path.write_text(content)
            logger.info(f"Saved {output_path}")


if __name__ == "__main__":
    main()
