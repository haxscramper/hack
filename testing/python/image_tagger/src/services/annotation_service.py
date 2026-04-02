import logging
import time
from pathlib import Path

from db.repository import Repository
from .chroma_store import ChromaDescriptionStore
from .wd_tagger import WdTagger
from .ollama_tagger import OllamaTagger


class AnnotationService:
    def __init__(
        self,
        repository: Repository,
        chroma_store: ChromaDescriptionStore,
        wd_tagger: WdTagger,
        ollama_tagger: OllamaTagger,
    ):
        self.repository = repository
        self.chroma_store = chroma_store
        self.wd_tagger = wd_tagger
        self.ollama_tagger = ollama_tagger
        self.processed_count = 0
        self.total_images = 0
        self.start_time: float | None = None

    def annotate_image(self, root_dir: Path, image_path: Path):
        self.processed_count += 1
        entry = self.repository.upsert_image(root_dir, image_path)
        image_id = int(entry.id)  # type: ignore

        existing_prob_tags = self.repository.list_probabilistic_tags(image_id)
        existing_reg_tags = self.repository.list_regular_tags(image_id)
        existing_desc = self.repository.get_description(image_id)

        if existing_prob_tags and existing_reg_tags and existing_desc:
            logging.debug(f"Skipping {image_path}, all annotations exist.")
            return

        if self.start_time and self.processed_count > 0 and self.total_images > 0:
            elapsed = time.time() - self.start_time
            if elapsed > 0:
                images_per_sec = self.processed_count / elapsed
                remaining = self.total_images - self.processed_count
                eta = remaining / images_per_sec if images_per_sec > 0 else 0
                h, m, s = int(eta // 3600), int((eta % 3600) // 60), int(eta % 60)
                progress_str = f"[{self.processed_count}/{self.total_images}] {images_per_sec:.2f} images/sec ETA:{h:02}:{m:02}:{s:02} "
            else:
                progress_str = ""
        else:
            progress_str = ""

        logging.info(f"{progress_str}Annotating image: {image_path}")

        if not existing_prob_tags:
            logging.debug(f"Running WD tagger for {image_path}")
            prob_tags = self.wd_tagger.tag_image(image_path)
            self.repository.replace_probabilistic_annotations(image_id, prob_tags)
        else:
            logging.debug(f"Skipping WD tagger for {image_path}, tags already exist.")

        if not existing_reg_tags:
            logging.debug(f"Running Ollama tagger for {image_path}")
            reg_tags = self.ollama_tagger.regular_tags(image_path)
            self.repository.replace_regular_annotations(image_id, reg_tags)
        else:
            logging.debug(
                f"Skipping Ollama regular tags for {image_path}, tags already exist."
            )

        if not existing_desc:
            logging.debug(f"Running Ollama description tagger for {image_path}")
            description = self.ollama_tagger.describe(image_path)
            if description:
                self.repository.set_description(
                    image_id, description, self.ollama_tagger.model_name
                )
                self.chroma_store.upsert_description(
                    relative_path=entry.relative_path,
                    description=description,
                    metadata={"relative_path": entry.relative_path},
                )
        else:
            logging.debug(
                f"Skipping Ollama description for {image_path}, description already exists."
            )
