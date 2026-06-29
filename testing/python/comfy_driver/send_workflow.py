#!/usr/bin/env python

import argparse
import json
import urllib.request


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("saved_api_path")
    parser.add_argument("input_json_path")
    parser.add_argument("--comfy-url", default="http://127.0.0.1:8188")
    args = parser.parse_args()

    with open(args.saved_api_path, "r", encoding="utf-8") as f:
        workflow = json.load(f)

    with open(args.input_json_path, "r", encoding="utf-8") as f:
        input_payload = json.load(f)

    target_node = None
    for node in workflow.values():
        meta = node.get("_meta", {})
        if meta.get("title") == "input_json_parameter":
            target_node = node
            break

    if target_node is None:
        raise RuntimeError("Could not find node titled input_json_parameter")

    target_node["inputs"]["value"] = json.dumps(input_payload, indent=2)

    request_body = {
        "prompt": workflow,
    }

    req = urllib.request.Request(
        f"{args.comfy_url}/prompt",
        data=json.dumps(request_body).encode("utf-8"),
        headers={"Content-Type": "application/json"},
        method="POST",
    )

    with urllib.request.urlopen(req) as resp:
        print(resp.read().decode("utf-8"))


if __name__ == "__main__":
    main()
