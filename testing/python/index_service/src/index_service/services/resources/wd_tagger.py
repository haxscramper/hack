from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Callable

from beartype.typing import ClassVar
import flax
import jax
import numpy as np
import pandas as pd
from huggingface_hub import hf_hub_download
from PIL import Image
from pydantic import BaseModel, Field

from index_service.services.core.job_types import BaseResource, BaseResourceConfig, RunContext
from index_service.services.resources.wdv3_jax import Models
from index_service.services.utils import get_xdg_cache_dir

jax.config.update(
    "jax_compilation_cache_dir",
    str(get_xdg_cache_dir(["resource", "wd_tagger", "jax_cache"])),
)
jax.config.update("jax_persistent_cache_min_entry_size_bytes", -1)
jax.config.update("jax_persistent_cache_min_compile_time_secs", 0)


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

MODEL_REPO_MAP = {
    "eva02_large": "SmilingWolf/wd-eva02-large-tagger-v3",
    "vit": "SmilingWolf/wd-vit-tagger-v3",
    "vit_large": "SmilingWolf/wd-vit-large-tagger-v3",
    "swinv2_v2": "SmilingWolf/wd-v1-4-swinv2-tagger-v2",
    "swinv2_v3": "SmilingWolf/wd-swinv2-tagger-v3",
    "convnext": "SmilingWolf/wd-convnext-tagger-v3",
}


@flax.struct.dataclass
class PredModel:
    apply_fun: Callable = flax.struct.field(pytree_node=False)
    params: Any = flax.struct.field(pytree_node=True)

    def jit_predict(self, x):
        x = x / 127.5 - 1
        x = self.apply_fun(self.params, x, train=False)
        x = flax.linen.sigmoid(x)
        x = jax.numpy.float32(x)
        return x

    def predict(self, x):
        preds = self.jit_predict(x)
        preds = jax.device_get(preds)
        return preds[0]


def pil_ensure_rgb(image: Image.Image) -> Image.Image:
    if image.mode not in ["RGB", "RGBA"]:
        image = (image.convert("RGBA")
                 if "transparency" in image.info else image.convert("RGB"))

    if image.mode == "RGBA":
        canvas = Image.new("RGBA", image.size, (255, 255, 255))
        canvas.alpha_composite(image)
        image = canvas.convert("RGB")

    return image


def pil_pad_square(image: Image.Image) -> Image.Image:
    w, h = image.size
    px = max(image.size)
    canvas = Image.new("RGB", (px, px), (255, 255, 255))
    canvas.paste(image, ((px - w) // 2, (px - h) // 2))
    return canvas


def pil_resize(image: Image.Image, target_size: int) -> Image.Image:
    if max(image.size) != target_size:
        image = image.resize((target_size, target_size), Image.BICUBIC)
    return image


class WdTaggerConfig(BaseResourceConfig):
    model: str = "swinv2_v3"
    threshold: float = 0.01
    cache_dir: Path | None = Field(
        default_factory=lambda: get_xdg_cache_dir(["resource", "wd_tagger"]))


class WdTagger(BaseResource):
    resource_key = "wd_tagger"
    config_model: ClassVar[type] = WdTaggerConfig

    config: WdTaggerConfig

    def __init__(self, config: WdTaggerConfig):
        super().__init__(config=config)

        repo_id = MODEL_REPO_MAP[self.config.model]

        weights_path = Path(
            hf_hub_download(
                repo_id=repo_id,
                filename="model.msgpack",
                repo_type="model",
                cache_dir=self.config.cache_dir,
            ))

        config_path = Path(
            hf_hub_download(
                repo_id=repo_id,
                filename="sw_jax_cv_config.json",
                repo_type="model",
                cache_dir=self.config.cache_dir,
            ))

        tags_csv_path = Path(
            hf_hub_download(
                repo_id=repo_id,
                filename="selected_tags.csv",
                repo_type="model",
                cache_dir=self.config.cache_dir,
            ))

        self.model_name = self.config.model
        self.model_path = weights_path
        self.config_path = config_path
        self.tags_csv_path = tags_csv_path
        self.threshold = self.config.threshold
        self.tags_df = pd.read_csv(str(tags_csv_path))

        with open(weights_path, "rb") as f:
            data = f.read()

        restored = flax.serialization.msgpack_restore(data)["model"]
        variables = {"params": restored["params"], **restored["constants"]}

        with open(config_path) as f:
            model_config = json.load(f)

        model_builder = Models.model_registry[model_config["model_name"]]()
        model = model_builder.build(
            config=model_builder,
            **model_config["model_args"],
        )

        self.model = PredModel(model.apply, params=variables)
        self.target_size = int(model_config["image_size"])

    def preprocess_image(self, image_path: Path) -> np.ndarray:
        img = Image.open(image_path)
        img = pil_ensure_rgb(img)
        img = pil_pad_square(img)
        img = pil_resize(img, self.target_size)
        inputs = np.array(img)
        inputs = np.expand_dims(inputs, axis=0)
        inputs = inputs[..., ::-1]
        return inputs

    def tag_image(self, ctx: RunContext, image_path: Path) -> list[WdTag]:
        input_data = self.preprocess_image(image_path)
        with ctx.trace_scope("predict image tags", path=image_path):
            probs = self.model.predict(input_data)

        result: list[WdTag] = []
        for i, prob in enumerate(probs):
            if prob >= self.threshold:
                row = self.tags_df.iloc[i]
                category = CATEGORY_MAP.get(
                    int(row["category"]),
                    f"cat_{int(row['category'])}",
                )
                result.append(
                    WdTag(
                        category=category,
                        name=str(row["name"]),
                        probability=float(prob),
                    ))

        result.sort(key=lambda x: x.probability, reverse=True)
        return result

    def handle(self, ctx: RunContext, request: WdTaggerRequest) -> WdTaggerResult:
        return WdTaggerResult(tags=self.tag_image(ctx, Path(request.path)))
