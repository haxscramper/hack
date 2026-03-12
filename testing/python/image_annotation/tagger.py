import sys
import urllib.request
import numpy as np
import onnxruntime as ort
import cv2
import pandas as pd
import json
import logging
from pathlib import Path
from beartype import beartype

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
)


@beartype
def download_progress(block_num: int, block_size: int,
                      total_size: int) -> None:
    downloaded = block_num * block_size
    if 0 < total_size:
        percent = min(downloaded * 100 / total_size, 100.0)
        logging.debug(f"Downloading: {percent:.1f}%")


@beartype
def ensure_file_exists(filename: Path, url: str) -> None:
    if not filename.exists():
        logging.info(
            f"File {filename} not found. Downloading from Hugging Face...")
        urllib.request.urlretrieve(url,
                                   str(filename),
                                   reporthook=download_progress)
        logging.info(f"Successfully downloaded {filename}")
    else:
        logging.info(
            f"File {filename} already exists locally. Skipping download.")


@beartype
def preprocess_image(image_path: Path, target_size: int = 448) -> np.ndarray:
    img = cv2.imread(str(image_path))
    if img is None:
        raise FileNotFoundError(f"Could not load image at: {image_path}")

    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    img = cv2.resize(img, (target_size, target_size),
                     interpolation=cv2.INTER_AREA)
    img_array = img.astype(np.float32)
    img_array = np.expand_dims(img_array, axis=0)

    return img_array


repo_url = "https://huggingface.co/SmilingWolf/wd-vit-tagger-v3/resolve/main/"
model_file = Path("model.onnx")
tags_file = Path("selected_tags.csv")

ensure_file_exists(model_file, repo_url + "model.onnx")
ensure_file_exists(tags_file, repo_url + "selected_tags.csv")

target_ep = "MIGraphXExecutionProvider"
available_providers = ort.get_available_providers()

if target_ep not in available_providers:
    raise RuntimeError(
        f"{target_ep} is not available in your ONNX Runtime build!\n"
        f"Available EPs: {available_providers}")

sess_options = ort.SessionOptions()
sess_options.add_session_config_entry("session.disable_cpu_ep_fallback", "1")

logging.info(f"Loading model strictly to {target_ep}...")
session = ort.InferenceSession(str(model_file),
                               sess_options=sess_options,
                               providers=[target_ep])

input_dir = Path(
    "/home/haxscramper/defaultdirs/input/grabber/banana_generations")
valid_extensions = {".jpg", ".jpeg", ".png", ".bmp", ".webp"}

input_name = session.get_inputs()[0].name
output_name = session.get_outputs()[0].name

tags_df = pd.read_csv(str(tags_file))
confidence_threshold = 0.35

for image_path in input_dir.rglob("*"):
    if image_path.is_file() and image_path.suffix.lower() in valid_extensions:
        logging.info(f"Running inference on image: {image_path.name}")

        input_data = preprocess_image(image_path)
        raw_predictions = session.run([output_name],
                                      {input_name: input_data})[0]
        probabilities = raw_predictions[0]

        predicted_tags = {}
        for i, prob in enumerate(probabilities):
            if confidence_threshold < prob:
                tag_name = tags_df["name"].iloc[i]
                predicted_tags[tag_name] = float(prob)

        predicted_tags = dict(
            sorted(predicted_tags.items(),
                   key=lambda item: item[1],
                   reverse=True))

        output_file = image_path.with_name(
            f"{image_path.stem}_infer_tags.json")
        with open(output_file, "w") as f:
            json.dump(predicted_tags, f, indent=4)

        logging.info(f"Saved predicted tags to: {output_file}")
