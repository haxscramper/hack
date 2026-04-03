from __future__ import annotations

import base64
import logging
import subprocess
import time
from pathlib import Path

import requests


class Joytagger:
    def __init__(
        self, backend: str = "ollama", model_name: str = "user-v4/joycaption-beta"
    ):
        self.backend = backend
        self.model_name = model_name
        self._server_process = None

        if self.backend == "ollama":
            self._ensure_ollama_running()
            self._ensure_model_loaded()
        elif self.backend == "llama-cpp":
            self._ensure_llama_server_running()
        else:
            raise ValueError(f"Unknown backend: {self.backend}")

    def _ensure_llama_server_running(self):
        try:
            requests.get("http://localhost:8080/health", timeout=2)
            logging.info("llama-server is already running.")
        except requests.exceptions.RequestException:
            logging.info("Starting llama-server...")
            llama_dir = Path("/home/haxscramper/software/llama.cpp")
            self._server_process = subprocess.Popen(
                [
                    "./build/bin/llama-server",
                    "--hf-repo",
                    "concedo/llama-joycaption-beta-one-hf-llava-mmproj-gguf",
                    "--hf-file",
                    "Llama-Joycaption-Beta-One-Hf-Llava-Q4_K.gguf",
                    "--mmproj-url",
                    "https://huggingface.co/concedo/llama-joycaption-beta-one-hf-llava-mmproj-gguf/resolve/main/llama-joycaption-beta-one-llava-mmproj-model-f16.gguf",
                ],
                cwd=llama_dir,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            # Wait for it to start
            for _ in range(60):
                try:
                    requests.get("http://localhost:8080/health", timeout=2)
                    logging.info("llama-server started successfully.")
                    break
                except requests.exceptions.RequestException:
                    time.sleep(1)
            else:
                raise RuntimeError("Failed to start llama-server.")

    def _ensure_ollama_running(self):
        try:
            requests.get("http://127.0.0.1:11434/", timeout=2)
            logging.info("Ollama server is already running.")
        except requests.exceptions.RequestException:
            logging.info("Starting Ollama server...")
            subprocess.Popen(
                ["ollama", "serve"],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            # Wait for it to start
            for _ in range(15):
                try:
                    requests.get("http://127.0.0.1:11434/", timeout=2)
                    logging.info("Ollama server started successfully.")
                    break
                except requests.exceptions.RequestException:
                    time.sleep(1)
            else:
                raise RuntimeError("Failed to start Ollama server.")

    def _ensure_model_loaded(self):
        logging.info(f"Loading/pulling model {self.model_name}...")
        # Run it to pull if necessary, capturing output to avoid breaking the console
        subprocess.run(
            ["ollama", "run", self.model_name],
            input=b"",
            capture_output=True,
            timeout=1800,  # Allow time for potential download
        )

    def __del__(self):
        if hasattr(self, "_server_process") and self._server_process is not None:
            self._server_process.terminate()
            self._server_process.wait(timeout=5)

    def _generate(self, image_path: Path, prompt: str) -> str:
        from PIL import Image
        import io

        with Image.open(image_path) as img:
            img = img.convert("RGB")
            buf = io.BytesIO()
            img.save(buf, format="PNG")
            b64_img = base64.b64encode(buf.getvalue()).decode("utf-8")

        if self.backend == "ollama":
            payload = {
                "model": self.model_name,
                "prompt": prompt,
                "stream": False,
                "images": [b64_img],
            }

            response = requests.post(
                "http://127.0.0.1:11434/api/generate", json=payload, timeout=300
            )
            try:
                response.raise_for_status()

            except Exception as e:
                e.add_note(f"prompt: {prompt}")
                e.add_note(f"image_path: {image_path}")
                raise e from None

            return response.json().get("response", "")

        elif self.backend == "llama-cpp":
            payload = {
                "messages": [
                    {
                        "role": "user",
                        "content": [
                            {
                                "type": "image_url",
                                "image_url": {
                                    "url": f"data:image/png;base64,{b64_img}",
                                },
                            },
                            {
                                "type": "text",
                                "text": prompt,
                            },
                        ],
                    }
                ],
                "temperature": 0,
                "top_p": 1,
            }

            response = requests.post(
                "http://localhost:8080/v1/chat/completions", json=payload, timeout=300
            )
            try:
                response.raise_for_status()

            except Exception as e:
                e.add_note(f"prompt: {prompt}")
                e.add_note(f"image_path: {image_path}")
                raise e from None

            return response.json()["choices"][0]["message"]["content"]

        return ""

    def regular_tags(self, image_path: Path) -> list[tuple[str, str]]:
        prompt = "Write a long comma-separated list of rule34 tags in alphabetical order for this image. Start with the artist, copyright, character, and meta tags (if any), prefixed by 'artist:', 'copyright:', 'character:', and 'meta:'. Then all the general tags."
        response_text = self._generate(image_path, prompt)

        # Parse tags
        tags = []
        raw_tags = [t.strip() for t in response_text.split(",") if t.strip()]
        for tag in raw_tags:
            parts = tag.split(":", 1)
            if len(parts) == 2:
                category, name = parts[0].strip().lower(), parts[1].strip()
                if category in ["artist", "copyright", "character", "meta"]:
                    tags.append((category, name))
                else:
                    # If prefix is something else, fallback to general with the full tag or just name
                    tags.append(("general", tag))
            else:
                tags.append(("general", tag))
        return tags

    def describe(self, image_path: Path, word_count: int = 100) -> str:
        prompt = f"Write a detailed description for this image in {word_count} words or less."
        return self._generate(image_path, prompt)
