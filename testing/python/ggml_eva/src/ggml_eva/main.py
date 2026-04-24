#!/usr/bin/env python

from pathlib import Path
from safetensors import safe_open


def main() -> None:
    model_path = Path("model.safetensors")

    with safe_open(model_path, framework="pt", device="cpu") as f:
        keys = sorted(f.keys())
        for key in keys:
            tensor = f.get_tensor(key)
            print(f"{key:60} {tuple(tensor.shape)} {tensor.dtype}")


if __name__ == "__main__":
    main()
