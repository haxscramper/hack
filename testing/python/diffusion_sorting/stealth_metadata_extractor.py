#!/usr/bin/env python

import argparse
import logging
from pathlib import Path
from typing import Optional
import gzip

from PIL import Image
import numpy as np


def has_alpha(image_array: np.ndarray) -> bool:
    return image_array.shape[2] == 4


def read_stealth_metadata(image_path: Path) -> Optional[str]:
    image = Image.open(image_path).convert("RGBA")
    width, height = image.size
    data = np.array(image)

    has_alpha_channel = has_alpha(data)
    mode = None
    compressed = False
    binary_data = ""
    buffer_a = ""
    buffer_rgb = ""
    index_a = 0
    index_rgb = 0
    sig_confirmed = False
    confirming_signature = True
    reading_param_len = False
    reading_param = False
    read_end = False
    param_len = 0

    for x in range(width):
        for y in range(height):
            pixel = data[y, x]

            if has_alpha_channel:
                r, g, b, a = pixel
                buffer_a += str(a & 1)
                index_a += 1
            else:
                r, g, b = pixel[:3]

            buffer_rgb += str(r & 1)
            buffer_rgb += str(g & 1)
            buffer_rgb += str(b & 1)
            index_rgb += 3

            if confirming_signature:
                if index_a == len("stealth_pnginfo") * 8:
                    decoded_sig = bytes([
                        int(buffer_a[i:i + 8], 2)
                        for i in range(0, len(buffer_a), 8)
                    ]).decode("utf-8", errors="ignore")
                    if decoded_sig in ["stealth_pnginfo", "stealth_pngcomp"]:
                        confirming_signature = False
                        sig_confirmed = True
                        reading_param_len = True
                        mode = "alpha"
                        compressed = decoded_sig == "stealth_pngcomp"
                        buffer_a = ""
                        index_a = 0
                    else:
                        read_end = True
                        break
                elif index_rgb == len("stealth_pnginfo") * 8:
                    decoded_sig = bytes([
                        int(buffer_rgb[i:i + 8], 2)
                        for i in range(0, len(buffer_rgb), 8)
                    ]).decode("utf-8", errors="ignore")
                    if decoded_sig in ["stealth_rgbinfo", "stealth_rgbcomp"]:
                        confirming_signature = False
                        sig_confirmed = True
                        reading_param_len = True
                        mode = "rgb"
                        compressed = decoded_sig == "stealth_rgbcomp"
                        buffer_rgb = ""
                        index_rgb = 0
            elif reading_param_len:
                if mode == "alpha" and index_a == 32:
                    param_len = int(buffer_a, 2)
                    reading_param_len = False
                    reading_param = True
                    buffer_a = ""
                    index_a = 0
                elif mode != "alpha" and index_rgb == 33:
                    param_len = int(buffer_rgb[:-1], 2)
                    reading_param_len = False
                    reading_param = True
                    buffer_rgb = buffer_rgb[-1:]
                    index_rgb = 1
            elif reading_param:
                if mode == "alpha" and index_a == param_len:
                    binary_data = buffer_a
                    read_end = True
                    break
                elif mode != "alpha" and index_rgb >= param_len:
                    diff = param_len - index_rgb
                    if diff < 0:
                        buffer_rgb = buffer_rgb[:diff]
                    binary_data = buffer_rgb
                    read_end = True
                    break
            else:
                read_end = True
                break

        if read_end:
            break

    if sig_confirmed and binary_data:
        byte_data = bytes([
            int(binary_data[i:i + 8], 2)
            for i in range(0, len(binary_data), 8)
        ])
        if compressed:
            decoded_data = gzip.decompress(byte_data).decode("utf-8")
        else:
            decoded_data = byte_data.decode("utf-8")
        return decoded_data

    return None


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Extract stealth metadata from images")
    parser.add_argument("image_path", type=Path, help="Path to the image file")
    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="Enable verbose logging")

    args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO)

    if not args.image_path.exists():
        logging.error(f"Image file not found: {args.image_path}")
        return

    metadata = read_stealth_metadata(args.image_path)
    
    if metadata:
        output_path = args.image_path.with_suffix(".txt")
        output_path.write_text(metadata, encoding="utf-8")
        print(metadata)
        logging.info(f"Metadata written to: {output_path}")
    else:
        Path(args.image_path).unlink()
        logging.info("No stealth metadata found")


if __name__ == "__main__":
    main()
