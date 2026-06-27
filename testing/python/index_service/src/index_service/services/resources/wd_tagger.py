from __future__ import annotations

import logging
from pathlib import Path

import cv2
import numpy as np
import onnxruntime as ort
import pandas as pd
from huggingface_hub import hf_hub_download
from pydantic import BaseModel

from index_service.services.harness import BaseResource
from index_service.services.utils import get_xdg_cache_dir

log = logging.getLogger(__name__)


class WdTag(BaseModel, extra="forbid"):
    category: str
    name: str
    probability: float


class WdTaggerRequest(BaseModel, extra="forbid"):
    path: str


class WdTaggerResult(BaseModel, extra="forbid"):
    tags: list[WdTag]


CATEGORY_MAP = {
    0: "general",
    1: "artist",
    3: "copyright",
    4: "character",
    5: "meta",
    9: "rating",
}


class WdTagger(BaseResource):
    resource_key = "wd_tagger"

    @staticmethod
    def from_huggingface(
        threshold: float = 0.01,
        cache_dir: Path | None = get_xdg_cache_dir(["wd_tagger"]),
    ) -> "WdTagger":
        repo_id = "SmilingWolf/wd-vit-tagger-v3"

        model_path = Path(
            hf_hub_download(
                repo_id=repo_id,
                filename="model.onnx",
                repo_type="model",
                cache_dir=cache_dir,
            ))

        tags_csv_path = Path(
            hf_hub_download(
                repo_id=repo_id,
                filename="selected_tags.csv",
                repo_type="model",
                cache_dir=cache_dir,
            ))

        return WdTagger(
            model_path=model_path,
            tags_csv_path=tags_csv_path,
            threshold=threshold,
        )

    def __init__(self,
                 model_path: Path,
                 tags_csv_path: Path,
                 threshold: float = 0.01):
        self.model_path = model_path
        self.tags_csv_path = tags_csv_path
        self.threshold = threshold
        self.tags_df = pd.read_csv(str(tags_csv_path))

        target_ep = "MIGraphXExecutionProvider"
        available = ort.get_available_providers()
        if target_ep not in available:
            target_ep = "CPUExecutionProvider"
            logging.warning(
                "MIGraphXExecutionProvider not found, falling back to CPUExecutionProvider"
            )

        sess_options = ort.SessionOptions()
        self.session = ort.InferenceSession(
            str(model_path),
            sess_options=sess_options,
            providers=[target_ep],
        )
        self.input_name = self.session.get_inputs()[0].name
        self.output_name = self.session.get_outputs()[0].name

    def preprocess_image(self,
                         image_path: Path,
                         target_size: int = 448) -> np.ndarray:
        img = cv2.imread(str(image_path))
        if img is None:
            raise FileNotFoundError(f"Could not load image: {image_path}")

        img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
        img = cv2.resize(img, (target_size, target_size),
                         interpolation=cv2.INTER_AREA)
        img = img.astype(np.float32)
        img = np.expand_dims(img, axis=0)
        return img

    def tag_image(self, image_path: Path) -> list[WdTag]:
        log.info(f"WD tagging image {image_path}")
        input_data = self.preprocess_image(image_path)
        raw = self.session.run([self.output_name],
                               {self.input_name: input_data})[0]
        probs = raw[0]
        result: list[WdTag] = list()
        for i, prob in enumerate(probs):
            if prob >= self.threshold:
                row = self.tags_df.iloc[i]
                category = CATEGORY_MAP.get(int(row["category"]),
                                            f"cat_{int(row['category'])}")
                result.append(
                    WdTag(
                        category=category,
                        name=str(row["name"]),
                        probability=float(prob),
                    ))
        result.sort(key=lambda x: x.probability, reverse=True)
        return result

    def handle(self, request: WdTaggerRequest) -> WdTaggerResult:
        return WdTaggerResult(tags=self.tag_image(Path(request.path)))
