#!/usr/bin/env python

import json
from pathlib import Path

import comfy.sd
import comfy.utils
import folder_paths


class LoadCheckpointPartsFromJson:

    @classmethod
    def INPUT_TYPES(cls):
        return {
            "required": {
                "json_text": ("STRING", {
                    "multiline": True
                }),
                "checkpoint_name_field": ("STRING", {
                    "default": "checkpoint"
                }),
                "clip_name_field": ("STRING", {
                    "default": "clip"
                }),
                "vae_name_field": ("STRING", {
                    "default": "vae"
                }),
            }
        }

    RETURN_TYPES = ("MODEL", "CLIP", "VAE")
    FUNCTION = "load"
    CATEGORY = "loaders"

    def _resolve_path(self, category, name):
        direct_path = folder_paths.get_full_path(category, name)
        if direct_path is not None:
            return direct_path

        candidates = [name]
        if Path(name).suffix == "":
            candidates.append(f"{name}.safetensors")
            candidates.append(f"{name}.ckpt")
            candidates.append(f"{name}.pt")

        base_dir = Path(folder_paths.get_folder_paths(category)[0])

        for candidate in candidates:
            candidate_name = Path(candidate).name
            for path in base_dir.rglob(candidate_name):
                if path.is_file():
                    return str(path)

        return None

    def load(self, json_text, checkpoint_name_field, clip_name_field,
             vae_name_field):
        if isinstance(json_text, dict):
            data = json_text
        else:
            data = json.loads(json_text)

        if not isinstance(data, dict):
            raise TypeError(f"Expected json object, got {type(data).__name__}")

        checkpoint_name = data.get(checkpoint_name_field)
        clip_name = data.get(clip_name_field)
        vae_name = data.get(vae_name_field)

        if checkpoint_name is None and clip_name is None and vae_name is None:
            raise ValueError(
                "At least one of checkpoint, clip, or vae must be specified")

        model = None
        clip = None
        vae = None

        checkpoint_path = None
        checkpoint_clip = None
        checkpoint_vae = None

        if checkpoint_name is not None:
            checkpoint_path = self._resolve_path("checkpoints",
                                                 checkpoint_name)
            if checkpoint_path is None:
                raise FileNotFoundError(
                    f"Checkpoint '{checkpoint_name}' not found")

            model, checkpoint_clip, checkpoint_vae, _ = comfy.sd.load_checkpoint_guess_config(
                checkpoint_path,
                output_vae=True,
                output_clip=True,
                embedding_directory=folder_paths.get_folder_paths(
                    "embeddings"),
            )

            clip = checkpoint_clip
            vae = checkpoint_vae

        if clip_name is not None:
            if checkpoint_name is not None and clip_name == checkpoint_name:
                if checkpoint_clip is None:
                    raise ValueError(
                        f"CLIP '{clip_name}' requested from checkpoint, but checkpoint was not loaded"
                    )
                clip = checkpoint_clip
            else:
                clip_path = self._resolve_path("clip", clip_name)
                if clip_path is None:
                    raise FileNotFoundError(f"CLIP '{clip_name}' not found")

                clip = comfy.sd.load_clip(
                    ckpt_paths=[clip_path],
                    embedding_directory=folder_paths.get_folder_paths(
                        "embeddings"),
                )

        if vae_name is not None:
            if checkpoint_name is not None and vae_name == checkpoint_name:
                if checkpoint_vae is None:
                    raise ValueError(
                        f"VAE '{vae_name}' requested from checkpoint, but checkpoint was not loaded"
                    )
                vae = checkpoint_vae
            else:
                vae_path = self._resolve_path("vae", vae_name)
                if vae_path is None:
                    raise FileNotFoundError(f"VAE '{vae_name}' not found")

                vae = comfy.sd.VAE(
                    sd=comfy.utils.load_torch_file(vae_path, safe_load=True))

        return (model, clip, vae)


NODE_CLASS_MAPPINGS = {
    "LoadCheckpointPartsFromJson": LoadCheckpointPartsFromJson,
}

NODE_DISPLAY_NAME_MAPPINGS = {
    "LoadCheckpointPartsFromJson": "Load Checkpoint Parts From Json",
}
