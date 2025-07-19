#!/usr/bin/env python

import json
import msgpack
from pathlib import Path
from typing import Any


def convert_msgpack_to_json() -> None:
    input_path = Path("/tmp/msgpack_dump.bin")
    output_path = Path("/tmp/msgpack_dump.json")

    with input_path.open("rb") as f:
        data: Any = msgpack.unpack(f, raw=False,  strict_map_key=False)

    with output_path.open("w") as f:
        json.dump(data, f, ensure_ascii=False, indent=2)


if __name__ == "__main__":
    convert_msgpack_to_json()
