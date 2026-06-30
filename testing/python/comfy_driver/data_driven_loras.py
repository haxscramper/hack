#!/usr/bin/env python

import json
import logging
from pathlib import Path

import comfy.sd
import comfy.utils
import folder_paths

logger = logging.getLogger("ApplyLorasFromJson")


class ApplyLorasFromJson:

    @classmethod
    def INPUT_TYPES(cls):
        return {
            "required": {
                "model": ("MODEL", ),
                "json_text": ("STRING", {
                    "multiline": True
                }),
                "lora_name_field": ("STRING", {
                    "default": "name"
                }),
                "model_weight_field": ("STRING", {
                    "default": "weight"
                }),
                "clip_weight_field": ("STRING", {
                    "default": "clip_weight"
                }),
            },
            "optional": {
                "clip": ("CLIP", ),
            },
        }

    RETURN_TYPES = ("MODEL", "CLIP")
    FUNCTION = "apply"
    CATEGORY = "loaders"

    def _resolve_lora_path(self, lora_name):
        direct_path = folder_paths.get_full_path("loras", lora_name)
        if direct_path is not None:
            return direct_path

        candidates = [lora_name]
        if not lora_name.lower().endswith(".safetensors"):
            candidates.append(f"{lora_name}.safetensors")

        for loras_root in folder_paths.get_folder_paths("loras"):
            loras_dir = Path(loras_root)
            for candidate in candidates:
                candidate_name = Path(candidate).name
                for path in loras_dir.rglob(candidate_name):
                    if path.is_file():
                        return str(path)

        return None

    def apply(
        self,
        model,
        json_text,
        lora_name_field,
        model_weight_field,
        clip_weight_field,
        clip=None,
    ):
        current_model = model
        current_clip = clip

        if isinstance(json_text, list):
            loras = json_text
        else:
            loras = json.loads(json_text)

        if not isinstance(loras, list):
            raise TypeError(
                f"Expected a list of loras, got {type(loras).__name__}")

        for index, item in enumerate(loras):
            lora_name = item[lora_name_field]
            model_weight = float(item[model_weight_field])
            clip_weight = float(
                item[clip_weight_field]) if clip is not None else 0.0

            lora_path = self._resolve_lora_path(lora_name)
            if lora_path is None:
                raise FileNotFoundError(
                    f"Lora '{lora_name}' (entry #{index}) not found in loras folder"
                )

            lora = comfy.utils.load_torch_file(lora_path, safe_load=True)
            current_model, current_clip = comfy.sd.load_lora_for_models(
                current_model,
                current_clip,
                lora,
                model_weight,
                clip_weight,
            )
            logger.info(
                f"Applied lora '{lora_name}' from '{lora_path}' "
                f"(model_weight={model_weight}, clip_weight={clip_weight})")

        return (current_model, current_clip)


NODE_CLASS_MAPPINGS = {
    "ApplyLorasFromJson": ApplyLorasFromJson,
}

NODE_DISPLAY_NAME_MAPPINGS = {
    "ApplyLorasFromJson": "Apply Loras From Json",
}
