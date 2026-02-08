"""RAG (Retrieval-Augmented Generation) features with file indexing."""

import os
import logging
from beartype.typing import Optional, List
import hashlib
from pathlib import Path
from beartype import beartype

import chromadb
from chromadb.config import Settings
from chromadb.api.types import EmbeddingFunction, Documents, Embeddings
from openrouter import OpenRouter

from .config import RAG_DIR, API_KEY, DEFAULT_EMBEDDING_MODEL

logger = logging.getLogger(__name__)



class OpenRouterEmbeddingFunction(EmbeddingFunction):
    """Custom embedding function that uses OpenRouter's embedding API."""

    def __init__(self,
                 api_key: str = API_KEY,
                 model: str = DEFAULT_EMBEDDING_MODEL):
        self.api_key = api_key
        self.model = model
        logger.debug(
            f"Initialized OpenRouterEmbeddingFunction with model: {model}")

    def __call__(self, input: Documents) -> Embeddings:
        """Generate embeddings for the given documents."""
        if not input:
            logger.debug("Empty input, returning empty embeddings")
            return []

        logger.debug(
            f"Generating embeddings for {len(input)} documents using model: {self.model}"
        )

        try:
            with OpenRouter(api_key=self.api_key) as client:
                # Process in batches to avoid rate limits
                all_embeddings: list[list[float]] = []
                batch_size = 100

                for i in range(0, len(input), batch_size):
                    batch = input[i:i + batch_size]
                    batch_num = i // batch_size + 1
                    total_batches = (len(input) + batch_size - 1) // batch_size

                    logger.debug(
                        f"Processing embedding batch {batch_num}/{total_batches} ({len(batch)} documents)"
                    )

                    # Log first document preview
                    if batch:
                        preview = (batch[0][:100] +
                                   "..." if len(batch[0]) > 100 else batch[0])
                        logger.debug(f"First document preview: {preview}")

                    # Use the embeddings.generate method from the SDK
                    response = client.embeddings.generate(
                        input=batch if len(batch) > 1 else batch[0],
                        model=self.model)

                    # Extract embeddings from response
                    if hasattr(response, "data") and response.data:
                        embedding_count = 0
                        for item in response.data:
                            if hasattr(item, "embedding"):
                                all_embeddings.append(item.embedding)
                                embedding_count += 1
                        logger.debug(
                            f"Received {embedding_count} embeddings, dim={len(all_embeddings[-1]) if all_embeddings else 0}"
                        )
                    else:
                        logger.warning(
                            f"Unexpected response format: {response}")
                        # Return empty embeddings for this batch
                        all_embeddings.extend([[0.0] * 1536] * len(batch))

                logger.debug(
                    f"Total embeddings generated: {len(all_embeddings)}")
                return all_embeddings
                
        except Exception as e:
            logger.error(f"Error generating embeddings: {e}", exc_info=True)
            # Return zero embeddings as fallback
            return [[0.0] * 1536] * len(input)


@beartype
class RAGManager:
    """Manages RAG features with file indexing using ChromaDB and OpenRouter embeddings."""

    def __init__(
        self,
        persist_dir: str = RAG_DIR,
        api_key: str = API_KEY,
        embedding_model: str = DEFAULT_EMBEDDING_MODEL,
    ):
        self.persist_dir = persist_dir
        os.makedirs(persist_dir, exist_ok=True)

        logger.debug(
            f"Initializing RAGManager with persist_dir: {persist_dir}")

        # Initialize ChromaDB with persistence
        self.client = chromadb.PersistentClient(
            path=persist_dir, settings=Settings(anonymized_telemetry=False))

        # Create embedding function using OpenRouter
        self.embedding_function = OpenRouterEmbeddingFunction(
            api_key=api_key, model=embedding_model)

        # Get or create collection with custom embedding function
        self.collection = self.client.get_or_create_collection(
            name="documents",
            metadata={"hnsw:space": "cosine"},
            embedding_function=self.embedding_function,
        )

        logger.info(
            f"Initialized RAG manager with {self.collection.count()} documents"
        )
        logger.info(f"Using embedding model: {embedding_model}")

    def _compute_file_hash(self, filepath: Path) -> str:
        """Compute MD5 hash of a file for change detection."""
        with open(filepath, "rb") as f:
            return hashlib.md5(f.read()).hexdigest()

    def _chunk_text(self,
                    text: str,
                    chunk_size: int = 1000,
                    overlap: int = 200) -> list[str]:
        """Split text into overlapping chunks."""
        chunks: List[str] = []
        start = 0
        while start < len(text):
            end = start + chunk_size
            chunk = text[start:end]
            if chunk.strip():
                chunks.append(chunk)
            start = end - overlap
        logger.debug(
            f"Chunked text into {len(chunks)} chunks (size={chunk_size}, overlap={overlap})"
        )
        return chunks

    def index_file(self, filepath: Path, force: bool = False) -> int:
        """Index a file and return number of chunks added."""
        if not os.path.exists(filepath):
            logger.warning(f"File not found: {filepath}")
            return 0

        file_hash = self._compute_file_hash(filepath)
        logger.debug(f"Indexing file: {filepath} (hash={file_hash[:8]}...)")

        # Check if already indexed with same hash
        existing = self.collection.get(
            where={"source": str(filepath)},
            include=["metadatas"],
        )

        if existing["ids"] and not force:
            # Check if hash matches
            if (existing["metadatas"]
                    and existing["metadatas"][0].get("hash") == file_hash):
                logger.info(f"File already indexed (unchanged): {filepath}")
                return 0
            else:
                # Remove old chunks
                self.collection.delete(ids=existing["ids"])
                logger.info(
                    f"Removed {len(existing['ids'])} old chunks for: {filepath}"
                )

        # Read and chunk file
        try:
            with open(filepath, "r", encoding="utf-8") as f:
                content = f.read()
            logger.debug(f"Read file: {len(content)} bytes")
        except UnicodeDecodeError:
            logger.warning(f"Cannot read file (not UTF-8): {filepath}")
            return 0
        except Exception as e:
            logger.error(f"Error reading file {filepath}: {e}")
            return 0

        chunks = self._chunk_text(content)
        if not chunks:
            return 0

        # Add chunks to collection (embeddings are generated automatically)
        ids = [f"{filepath}:{i}" for i in range(len(chunks))]
        metadatas = [{
            "source": str(filepath),
            "chunk_index": i,
            "hash": file_hash
        } for i in range(len(chunks))]

        logger.info(f"Generating embeddings for {len(chunks)} chunks...")
        self.collection.add(ids=ids, documents=chunks, metadatas=metadatas)

        logger.info(f"Indexed {len(chunks)} chunks from: {filepath}")
        return len(chunks)

    def index_path(self,
                   dirpath: str,
                   extensions: Optional[list[str]] = None) -> int:
        """Index all files in a directory."""
        if extensions is None:
            extensions = [
                ".py", ".txt", ".md", ".json", ".yaml", ".yml", ".toml"
            ]

        logger.debug(
            f"Indexing directory: {dirpath} (extensions={extensions})")

        total_chunks = 0
        file_count = 0
        path = Path(dirpath)
        if path.is_file():
            total_chunks += self.index_file(path)

        else:
            for file in Path(dirpath).rglob("*"):
                if file.suffix in extensions:
                    file_count += 1
                    total_chunks += self.index_file(file)

        logger.info(
            f"Indexed {total_chunks} total chunks from {file_count} files in: {dirpath}"
        )
        return total_chunks

    def query(self, query_text: str, n_results: int = 5) -> list[dict]:
        """Query the RAG index and return relevant chunks."""
        logger.debug(
            f"RAG query: '{query_text[:50]}...' (n_results={n_results})")

        if self.collection.count() == 0:
            logger.debug("Collection is empty, returning no results")
            return []

        results = self.collection.query(query_texts=[query_text],
                                        n_results=min(n_results,
                                                      self.collection.count()))

        documents = []
        if results["documents"] and results["metadatas"]:
            distances = (results["distances"][0] if results["distances"] else
                         [0] * len(results["documents"][0]))
            for doc, metadata, distance in zip(results["documents"][0],
                                               results["metadatas"][0],
                                               distances):
                documents.append({
                    "content": doc,
                    "source": metadata.get("source", "unknown"),
                    "chunk_index": metadata.get("chunk_index", 0),
                    "distance": distance,
                })

        logger.debug(f"Query returned {len(documents)} results")
        for i, doc in enumerate(documents):
            logger.debug(
                f"  Result {i + 1}: source={doc['source']}, distance={doc['distance']:.4f}"
            )

        return documents

    def augment_query(self, query: str, n_results: int = 3) -> str:
        """Augment a query with relevant context from the index."""
        logger.debug(
            f"Augmenting query with RAG context (n_results={n_results})")
        relevant_docs = self.query(query, n_results)

        if not relevant_docs:
            logger.debug(
                "No relevant documents found, returning original query")
            return query

        context_parts = []
        for doc in relevant_docs:
            source = doc["source"]
            content = doc["content"]
            context_parts.append(f"[From {source}]:\n{content}")

        context = "\n\n---\n\n".join(context_parts)
        augmented = f"""Context from indexed documents:

{context}

---

User query: {query}"""

        logger.debug(
            f"Augmented query with {len(relevant_docs)} context documents")

        return augmented

    def get_stats(self) -> dict:
        """Get statistics about the RAG index."""
        count = self.collection.count()

        # Get unique sources
        sources = set()
        if count > 0:
            all_docs = self.collection.get(include=["metadatas"])
            if all_docs["metadatas"]:
                for metadata in all_docs["metadatas"]:
                    if metadata and "source" in metadata:
                        sources.add(metadata["source"])

        return {
            "total_chunks": count,
            "indexed_files": len(sources),
            "sources": list(sources),
        }

    def clear(self) -> None:
        """Clear all indexed documents."""
        logger.debug("Clearing RAG index")
        # Delete and recreate collection
        self.client.delete_collection("documents")
        self.collection = self.client.get_or_create_collection(
            name="documents",
            metadata={"hnsw:space": "cosine"},
            embedding_function=self.embedding_function,
        )
        logger.info("Cleared RAG index")
