#!/usr/bin/env python

import logging
from pathlib import Path
from typing import Optional, Dict, Any

import click
from PIL import Image
from PIL.ExifTags import TAGS
import piexif

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(name)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s")

logging.getLogger("asyncio").setLevel(logging.WARNING)
logging.getLogger("PIL").setLevel(logging.WARNING)
logging.getLogger("PIL.PngImagePlugin").setLevel(logging.WARNING)
logging.getLogger("PIL.TiffImagePlugin").setLevel(logging.WARNING)

def extract_png_metadata(png_path: Path) -> Optional[Dict[str, Any]]:
    try:
        with Image.open(png_path) as img:
            metadata = {}
            
            if hasattr(img, "text") and img.text:
                metadata.update(img.text)
            
            if hasattr(img, "info") and img.info:
                for key, value in img.info.items():
                    if isinstance(value, (str, int, float)):
                        metadata[str(key)] = value
            
            return metadata if metadata else None
    except Exception as e:
        logging.error(f"Failed to extract metadata from {png_path}: {e}")
        return None

def create_exif_from_metadata(metadata: Dict[str, Any]) -> Optional[bytes]:
    exif_dict = {"0th": {}, "Exif": {}, "GPS": {}, "1st": {}, "thumbnail": None}
    
    logging.info(f'{[(v, k) for k, v in piexif.TAGS["0th"].items()]}')
    tag_name_to_id = {v: k for k, v in piexif.TAGS["0th"].items()}
    exif_tag_name_to_id = {v: k for k, v in piexif.TAGS["Exif"].items()}
    gps_tag_name_to_id = {v: k for k, v in piexif.TAGS["GPS"].items()}
    
    for key, value in metadata.items():
        try:
            if key.startswith("exif:"):
                exif_key = key.replace("exif:", "")
                if exif_key.isdigit():
                    tag_id = int(exif_key)
                    if tag_id <= 65535:
                        exif_dict["0th"][tag_id] = str(value)
                elif exif_key in tag_name_to_id:
                    exif_dict["0th"][tag_name_to_id[exif_key]] = str(value)
                elif exif_key in exif_tag_name_to_id:
                    exif_dict["Exif"][exif_tag_name_to_id[exif_key]] = str(value)
                elif exif_key in gps_tag_name_to_id:
                    exif_dict["GPS"][gps_tag_name_to_id[exif_key]] = str(value)
            elif key in tag_name_to_id:
                exif_dict["0th"][tag_name_to_id[key]] = str(value)
            elif key in exif_tag_name_to_id:
                exif_dict["Exif"][exif_tag_name_to_id[key]] = str(value)
            elif key in gps_tag_name_to_id:
                exif_dict["GPS"][gps_tag_name_to_id[key]] = str(value)
            else:
                try:
                    tag_id = int(key)
                    if tag_id <= 65535:
                        exif_dict["0th"][tag_id] = str(value)
                except ValueError:
                    exif_dict["0th"][piexif.ImageIFD.ImageDescription] = f"{key}:{value}"
        except Exception as e:
            logging.warning(f"Failed to process metadata key {key}: {e}")
            continue
    
    if any(exif_dict[ifd] for ifd in ["0th", "Exif", "GPS"] if exif_dict[ifd]):
        return piexif.dump(exif_dict)

    else:
        return None


@click.command()
@click.argument("source_dir", type=click.Path(exists=True, file_okay=False, dir_okay=True, path_type=Path))
@click.argument("target_dir", type=click.Path(file_okay=False, dir_okay=True, path_type=Path))
def convert_png_to_jpeg(source_dir: Path, target_dir: Path) -> None:
    target_dir.mkdir(parents=True, exist_ok=True)
    
    png_files = list(source_dir.glob("*.png"))
    
    for png_file in png_files:
        logging.debug(f"Processing {png_file}")
        
        metadata = extract_png_metadata(png_file)
        
        with Image.open(png_file) as img:
            rgb_img = img.convert("RGB")
            output_file = target_dir / f"{png_file.stem}.jpeg"
            
            if metadata:
                exif_bytes = create_exif_from_metadata(metadata)
                if exif_bytes:
                    rgb_img.save(output_file, "JPEG", exif=exif_bytes)
                    logging.debug(f"Converted {png_file} to {output_file} with metadata")
                else:
                    rgb_img.save(output_file, "JPEG")
                    logging.debug(f"Converted {png_file} to {output_file} without metadata")
            else:
                rgb_img.save(output_file, "JPEG")
                logging.debug(f"Converted {png_file} to {output_file} without metadata")
            


if __name__ == "__main__":
    convert_png_to_jpeg()
