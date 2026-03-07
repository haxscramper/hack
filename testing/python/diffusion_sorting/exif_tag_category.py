import json
import logging
import os
import re
import requests
import streamlit as st
from pathlib import Path
from typing import Any, Dict, List, Optional, Set

import yaml
from pydantic import BaseModel
from llama_cpp import Llama


# --- Configurations ---
def _get_llm_cache_dir() -> Path:
    xdg_cache_home = os.environ.get("XDG_CACHE_HOME",
                                    os.path.expanduser("~/.cache"))
    cache_dir = Path(xdg_cache_home) / "exif_explorer_app" / "models"
    cache_dir.mkdir(parents=True, exist_ok=True)
    return cache_dir


LLM_MODEL_URL = "https://huggingface.co/QuantFactory/Meta-Llama-3-8B-Instruct-GGUF/resolve/main/Meta-Llama-3-8B-Instruct.Q4_K_M.gguf"
LLM_MODEL_PATH = _get_llm_cache_dir() / "Meta-Llama-3-8B-Instruct.Q4_K_M.gguf"

USER_CUSTOM_YAML_PATH = Path("user_custom_tags.yaml")
TAG_CACHE_PATH = Path("tag_llm_cache.json")
DEFAULT_YAML_PATH = Path("~/tmp/default_tags.yaml").expanduser(
)  # Place the downloaded SD yaml here


# --- Pydantic Schemas for YAML ---
class GroupSchema(BaseModel):
    color: Optional[str] = None
    # YAML parses empty keys as None, so tags is a Dict[str, Any]
    tags: Optional[Dict[str, Any]] = None
    name: str = "group-default"


class CategorySchema(BaseModel):
    groups: List[GroupSchema]
    name: str = "category-default"


class TagCategorizer:

    def __init__(self):
        self.tag_to_category: Dict[str, str] = {}
        self.categories: Set[str] = set()
        self.llm_cache: Dict[str, str] = {}
        self.llm: Optional[Llama] = None

        self._load_cache()

        # Load YAMLs
        if DEFAULT_YAML_PATH.exists():
            self._load_yaml_file(DEFAULT_YAML_PATH)
        else:
            logging.warning(f"Default YAML not found at {DEFAULT_YAML_PATH}.")

        if USER_CUSTOM_YAML_PATH.exists():
            self._load_yaml_file(USER_CUSTOM_YAML_PATH)

        # Ensure 'Uncategorized' exists as a fallback
        self.categories.add("Uncategorized")

    def normalize(self, tag: str) -> str:
        """Normalizes tags to lowercase with spaces (e.g. 'top-down_view' -> 'top down view')"""
        # Replace underscores and hyphens with spaces, strip whitespace, lower
        return re.sub(r'[_\-]+', ' ', tag).lower().strip()

    def _load_yaml_file(self, path: Path):
        """Loads and validates a YAML file using Pydantic."""
        try:
            with open(path, "r", encoding="utf-8") as f:
                data = yaml.safe_load(f)

            if not data:
                return

            # Parse through Pydantic
            categories = [CategorySchema(**cat) for cat in data]

            for category in categories:
                cat_name = category.name
                self.categories.add(cat_name)

                for group in category.groups:
                    if group.tags:
                        for tag_name in group.tags.keys():
                            norm_tag = self.normalize(tag_name)
                            self.tag_to_category[norm_tag] = cat_name

            logging.info(
                f"Loaded {len(categories)} categories from {path.name}")
        except Exception as e:
            logging.error(f"Failed to load YAML {path}: {e}")

    def _load_cache(self):
        """Loads the LLM tag cache."""
        if TAG_CACHE_PATH.exists():
            try:
                with open(TAG_CACHE_PATH, "r", encoding="utf-8") as f:
                    self.llm_cache = json.load(f)
            except Exception as e:
                logging.error(f"Failed to load tag cache: {e}")

    def _save_cache(self):
        """Saves the LLM tag cache."""
        try:
            with open(TAG_CACHE_PATH, "w", encoding="utf-8") as f:
                json.dump(self.llm_cache, f, indent=2)
        except Exception as e:
            logging.error(f"Failed to save tag cache: {e}")

    def _download_model(self):
        """Downloads the LLM model if it doesn't exist, showing a progress bar in Streamlit."""
        if LLM_MODEL_PATH.exists():
            return

        logging.info(f"Downloading LLM model to {LLM_MODEL_PATH}...")
        try:
            response = requests.get(LLM_MODEL_URL, stream=True)
            response.raise_for_status()
            total_size = int(response.headers.get('content-length', 0))

            progress_text = "Downloading Llama-3 LLM (One-time setup ~4.7GB)..."
            progress_bar = st.progress(0, text=progress_text)

            downloaded = 0
            with open(LLM_MODEL_PATH, "wb") as f:
                # Download in 8MB chunks
                for chunk in response.iter_content(chunk_size=8192 * 1024):
                    if chunk:
                        f.write(chunk)
                        downloaded += len(chunk)
                        if total_size > 0:
                            progress_bar.progress(min(1.0,
                                                      downloaded / total_size),
                                                  text=progress_text)

            progress_bar.empty()
            logging.info("Model download complete!")
        except Exception as e:
            logging.error(f"Failed to download model: {e}")
            if LLM_MODEL_PATH.exists():
                LLM_MODEL_PATH.unlink()  # Cleanup corrupted partial download

    def _init_llm(self):
        """Lazy load the LLM only when an unknown tag is encountered."""
        if self.llm is None:
            self._download_model()

            if LLM_MODEL_PATH.exists():
                logging.info("Initializing Llama CPP...")
                self.llm = Llama(
                    model_path=str(LLM_MODEL_PATH),
                    n_gpu_layers=-1,  # Offload all layers to AMD GPU
                    n_ctx=512,
                    verbose=False)
            else:
                logging.warning(
                    "LLM model download failed. Cannot categorize unknown tags."
                )

    def _ask_llm_category(self, tag: str) -> str:
        """Prompts the local LLM to categorize a single tag."""
        self._init_llm()
        if self.llm is None:
            return "Uncategorized"

        valid_cats = list(self.categories - {"Uncategorized"})
        cats_str = ", ".join(valid_cats)

        prompt = (
            f"You are a Stable Diffusion tag classifier. "
            f"Classify the tag '{tag}' into EXACTLY ONE of the following categories: [{cats_str}]. "
            f"Reply with ONLY the exact category name. Do not explain.")

        try:
            response = self.llm(prompt,
                                max_tokens=10,
                                temperature=0.1,
                                stop=["\n", ".", ","])
            answer = response["choices"][0]["text"].strip()

            # Verify the LLM actually picked a valid category
            for cat in valid_cats:
                if cat.lower() in answer.lower():
                    return cat

        except Exception as e:
            logging.error(f"LLM Error on tag '{tag}': {e}")

        return "Uncategorized"

    def get_category(self, tag: str) -> str:
        """Returns the category for a tag, utilizing YAML -> Cache -> LLM."""
        norm_tag = self.normalize(tag)

        # 1. Check YAML dictionaries
        if norm_tag in self.tag_to_category:
            return self.tag_to_category[norm_tag]

        # 2. Check Local LLM Cache
        if norm_tag in self.llm_cache:
            return self.llm_cache[norm_tag]

        # 3. Ask LLM (and save to cache)
        logging.info(f"Unknown tag: '{norm_tag}'. Asking LLM...")
        category = self._ask_llm_category(norm_tag)
        self.llm_cache[norm_tag] = category
        self._save_cache()

        return category
