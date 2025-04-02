#!/usr/bin/env python

import shutil
import tempfile
import logging
from pathlib import Path
from typing import List, Tuple
from PIL import Image
import math
import os
import sys

logging.basicConfig()
logging.root.setLevel(logging.NOTSET)
logging.basicConfig(level=logging.NOTSET)

logger = logging.getLogger(__name__)

def get_resized_dimensions(width: int, height: int, max_width: int = 1920, max_height: int = 1080) -> Tuple[int, int]:
    if width > height:
        if width > max_width:
            new_width = max_width
            new_height = int(height * (max_width / width))
        else:
            return width, height
    else:
        if height > max_height:
            new_height = max_height
            new_width = int(width * (max_height / height))
        else:
            return width, height
    
    return new_width, new_height

def compress_image(image_path: Path, output_path: Path, max_size_bytes: int = 1572864) -> None:
    try:
        # Always ensure output has PNG extension
        output_path = output_path.with_suffix(".png")
        
        img = Image.open(image_path)
        width, height = img.size
        
        # Resize if needed
        new_width, new_height = get_resized_dimensions(width, height, 1920, 1080)
        if new_width != width or new_height != height:
            img = img.resize((new_width, new_height), Image.LANCZOS)
        
        # Create temporary file
        with tempfile.NamedTemporaryFile(delete=False) as temp_file:
            temp_path = Path(temp_file.name)
        
        # Save as PNG initially
        img.save(temp_path, format="PNG")
        
        # If the PNG is too large, try optimizing it
        if temp_path.stat().st_size > max_size_bytes:
            # Try optimizing PNG
            img.save(temp_path, format="PNG", optimize=True)
            
            # If still too large, try reducing colors
            if temp_path.stat().st_size > max_size_bytes and img.mode != "P":
                # Convert to palette mode with dithering
                img = img.convert("P", palette=Image.ADAPTIVE, colors=256)
                img.save(temp_path, format="PNG", optimize=True)
            
            # If still too large, try JPEG with decreasing quality and then convert back to PNG
            if temp_path.stat().st_size > max_size_bytes:
                quality = 95
                # Convert to RGB if needed (for JPEG)
                if img.mode in ["RGBA", "P"]:
                    rgb_img = img.convert("RGB")
                else:
                    rgb_img = img
                    
                # Keep reducing quality until size is acceptable
                while temp_path.stat().st_size > max_size_bytes and quality > 10:
                    rgb_img.save(temp_path, format="JPEG", quality=quality)
                    quality -= 5
                    
                    # Load the JPEG and save as PNG for final output
                    if temp_path.stat().st_size <= max_size_bytes:
                        jpeg_img = Image.open(temp_path)
                        jpeg_img.save(temp_path, format="PNG")
        
        # Copy temp file to destination
        shutil.copy2(temp_path, output_path)
        
        # Cleanup
        os.unlink(temp_path)
            
    except Exception as e:
        logger.error(f"Error processing {image_path}: {str(e)}")
        # For failed conversions, try a direct PIL conversion to PNG
        try:
            img = Image.open(image_path)
            img.save(output_path, format="PNG")
        except Exception as e2:
            logger.error(f"Second attempt to convert {image_path} failed: {str(e2)}")
            # If conversion completely fails, copy original but rename to PNG
            shutil.copy2(image_path, output_path.with_suffix(image_path.suffix))
            logger.warning(f"Copied original file format for {image_path}")

def is_image_file(file_path: Path) -> bool:
    image_extensions = [".jpg", ".jpeg", ".png", ".gif", ".bmp", ".webp", ".tiff", ".tif"]
    return file_path.suffix.lower() in image_extensions

def process_directory(source_dir: Path, target_dir: Path) -> None:
    target_dir.mkdir(parents=True, exist_ok=True)
    
    for item in source_dir.iterdir():
        target_path = target_dir / item.name
        
        if item.is_dir():
            process_directory(item, target_path)
        elif item.is_file():
            if is_image_file(item):
                # Convert all images to PNG
                png_target_path = target_path.with_suffix(".png")
                compress_image(item, png_target_path)
            else:
                shutil.copy2(item, target_path)

def main() -> None:
    if len(sys.argv) < 2:
        logger.error("Usage: script.py <source_directory>")
        sys.exit(1)
    
    source_dir = Path(sys.argv[1])
    target_dir = Path("/tmp") / source_dir.name
    
    if not source_dir.exists() or not source_dir.is_dir():
        logger.error(f"Error: {source_dir} does not exist or is not a directory")
        sys.exit(1)
    
    process_directory(source_dir, target_dir)
    logger.info(f"Directory copied and processed to {target_dir}")

if __name__ == "__main__":
    main()
