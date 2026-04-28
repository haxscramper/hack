from beartype import beartype
from pathlib import Path
import chromadb


class ChromaDescriptionStore:
    def __init__(self, base_dir: Path):
        self.client = chromadb.PersistentClient(path=str(base_dir))
        self.collection = self.client.get_or_create_collection("image_descriptions")

    def upsert_description(
        self, relative_path: str, description: str, metadata: dict | None = None
    ):
        self.collection.upsert(
            ids=[relative_path],
            documents=[description],
            metadatas=[metadata or {}],
        )

    def query_similar(self, text: str, n_results: int = 10):
        return self.collection.query(
            query_texts=[text],
            n_results=n_results,
        )
