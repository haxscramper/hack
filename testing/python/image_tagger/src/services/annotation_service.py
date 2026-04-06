import logging
import time
from collections import deque
from pathlib import Path

from db.repository import Repository
from .chroma_store import ChromaDescriptionStore
from .wd_tagger import WdTagger
from .joy_tagger import Joytagger


class AnnotationService:
    def __init__(
        self,
        repository: Repository,
        chroma_store: ChromaDescriptionStore,
        wd_tagger: WdTagger,
        joy_tagger: Joytagger,
    ):
        self.repository = repository
        self.chroma_store = chroma_store
        self.wd_tagger = wd_tagger
        self.joy_tagger = joy_tagger
        self.processed_count = 0
        self.total_images = 0
        self.processing_times = deque(maxlen=20)

    def annotate_image(self, root_dir: Path, image_path: Path):
        start_time = time.time()
        entry = self.repository.upsert_image(root_dir, image_path)
        image_id = int(entry.id)  # type: ignore

        existing_prob_tags = self.repository.list_probabilistic_tags(image_id)
        existing_reg_tags = self.repository.list_regular_tags(image_id)
        existing_desc = self.repository.get_description(image_id)

        if existing_prob_tags and existing_reg_tags and existing_desc:
            logging.debug(f"Skipping {image_path}, all annotations exist.")
            return

        self.processed_count += 1

        if self.processing_times:
            avg_time = sum(self.processing_times) / len(self.processing_times)
            if avg_time > 0:
                images_per_sec = 1 / avg_time
                remaining = self.total_images - self.processed_count
                eta_seconds = remaining * avg_time
                h, m, s = (
                    int(eta_seconds // 3600),
                    int((eta_seconds % 3600) // 60),
                    int(eta_seconds % 60),
                )
                progress_str = f"[{self.processed_count}/{self.total_images}] {images_per_sec:.2f} images/sec ETA:{h:02}:{m:02}:{s:02} "
            else:
                progress_str = ""
        else:
            progress_str = ""

        need_wd_tagger = not existing_prob_tags and False
        need_joycaption_tagger =not existing_reg_tags and False
        need_joycaption_description = not existing_desc and True
        need_any_processing = need_wd_tagger or need_joycaption_tagger or need_joycaption_description

        if need_any_processing:
            logging.info(f"{progress_str}Annotating image: {image_path}")

        if need_wd_tagger:
            if not existing_prob_tags:
                logging.debug(f"Running WD tagger for {image_path}")
                prob_tags = self.wd_tagger.tag_image(image_path)
                self.repository.replace_probabilistic_annotations(image_id, prob_tags)
            else:
                logging.debug(f"Skipping WD tagger for {image_path}, tags already exist.")

        if need_joycaption_tagger: 
            if not existing_reg_tags:
                logging.debug(f"Running Ollama tagger for {image_path}")
                reg_tags = self.joy_tagger.regular_tags(image_path)
                self.repository.replace_regular_annotations(image_id, reg_tags)
            else:
                logging.debug(
                    f"Skipping Ollama regular tags for {image_path}, tags already exist."
                )

        if need_joycaption_description:
            if not existing_desc:
                logging.debug(f"Running Ollama description tagger for {image_path}")
                description = self.joy_tagger.describe(image_path)
                if description:
                    self.repository.set_description(
                        image_id, description, self.joy_tagger.model_name
                    )
                    self.chroma_store.upsert_description(
                        relative_path=str(entry.relative_path),
                        description=description,
                        metadata={"relative_path": str(entry.relative_path)},
                    )

                    logging.info(f"Description: '{description}'")
            else:
                logging.debug(
                    f"Skipping Ollama description for {image_path}, description already exists."
                )

        if need_any_processing:
            self.processing_times.append(time.time() - start_time)
