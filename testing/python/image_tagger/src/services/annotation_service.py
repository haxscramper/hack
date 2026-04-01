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
        wd_tagger: WdTagger | None = None,
        ollama_tagger: OllamaTagger | None = None,
    ):
        self.repository = repository
        self.chroma_store = chroma_store
        self.wd_tagger = wd_tagger
        self.ollama_tagger = ollama_tagger

    def annotate_image(self, root_dir: Path, image_path: Path):
        entry = self.repository.upsert_image(root_dir, image_path)

        if self.wd_tagger:
            prob_tags = self.wd_tagger.tag_image(image_path)
            self.repository.replace_probabilistic_annotations(entry.id, prob_tags)

        if self.ollama_tagger:
            reg_tags = self.ollama_tagger.regular_tags(image_path)
            self.repository.replace_regular_annotations(entry.id, reg_tags)

            description = self.ollama_tagger.describe(image_path)
            if description:
                self.repository.set_description(entry.id, description, self.ollama_tagger.model_name)
                self.chroma_store.upsert_description(
                    image_path=str(image_path.resolve()),
                    description=description,
                    metadata={"relative_path": entry.relative_path},
                )
