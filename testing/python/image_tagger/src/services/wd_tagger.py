from __future__ import annotations

import logging
from pathlib import Path
import cv2
import numpy as np
import onnxruntime as ort
import pandas as pd


CATEGORY_MAP = {
    0: "general",
    1: "artist",
    3: "copyright",
    4: "character",
    5: "meta",
    9: "rating",
}


class WdTagger:
    def __init__(self, model_path: Path, tags_csv_path: Path, threshold: float = 0.01):
        self.model_path = model_path
        self.tags_csv_path = tags_csv_path
        self.threshold = threshold
        self.tags_df = pd.read_csv(str(tags_csv_path))

        target_ep = "MIGraphXExecutionProvider"
        available = ort.get_available_providers()
        if target_ep not in available:
            target_ep = "CPUExecutionProvider"
            logging.warning("MIGraphXExecutionProvider not found, falling back to CPUExecutionProvider")

        sess_options = ort.SessionOptions()
        self.session = ort.InferenceSession(
            str(model_path),
            sess_options=sess_options,
            providers=[target_ep],
        )
        self.input_name = self.session.get_inputs()[0].name
        self.output_name = self.session.get_outputs()[0].name

    def preprocess_image(self, image_path: Path, target_size: int = 448) -> np.ndarray:
        img = cv2.imread(str(image_path))
        if img is None:
            raise FileNotFoundError(f"Could not load image: {image_path}")
        img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
        img = cv2.resize(img, (target_size, target_size), interpolation=cv2.INTER_AREA)
        img = img.astype(np.float32)
        img = np.expand_dims(img, axis=0)
        return img

    def tag_image(self, image_path: Path) -> list[tuple[str, str, float]]:
        input_data = self.preprocess_image(image_path)
        raw = self.session.run([self.output_name], {self.input_name: input_data})[0]
        probs = raw[0]
        result = []
        for i, prob in enumerate(probs):
            if prob >= self.threshold:
                row = self.tags_df.iloc[i]
                category = CATEGORY_MAP.get(int(row["category"]), f"cat_{int(row['category'])}")
                result.append((category, str(row["name"]), float(prob)))
        result.sort(key=lambda x: x[2], reverse=True)
        return result
