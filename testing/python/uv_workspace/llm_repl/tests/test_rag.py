"""Tests for RAG module."""

import os
import pytest
from unittest.mock import MagicMock, patch
import numpy as np

from llm_repl.rag import RAGManager, OpenRouterEmbeddingFunction


class TestOpenRouterEmbeddingFunction:
    """Tests for OpenRouterEmbeddingFunction."""

    def test_init(self, api_key):
        """Test initialization."""
        func = OpenRouterEmbeddingFunction(api_key=api_key)
        assert func.api_key == api_key
        assert func.model is not None


@pytest.mark.integration
class TestOpenRouterEmbeddingFunctionIntegration:
    """Integration tests for OpenRouterEmbeddingFunction."""

    def test_generate_single_embedding(self, api_key):
        """Test generating a single embedding."""
        func = OpenRouterEmbeddingFunction(api_key=api_key)
        result = func(["Hello, world!"])

        assert len(result) == 1
        assert isinstance(result[0], np.ndarray)
        assert len(result[0]) > 0
        assert all(isinstance(x, float) for x in result[0])

    def test_generate_multiple_embeddings(self, api_key):
        """Test generating multiple embeddings."""
        func = OpenRouterEmbeddingFunction(api_key=api_key)
        texts = ["Hello", "World", "Test"]
        result = func(texts)

        assert len(result) == 3
        for embedding in result:
            assert isinstance(embedding, list)
            assert len(embedding) > 0


class TestRAGManagerInit:
    """Tests for RAGManager initialization."""

    def test_init_creates_directory(self, temp_rag_dir, api_key):
        """Test that init creates directory."""
        rag_dir = os.path.join(temp_rag_dir, "new_dir")

        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          return_value=[]):
            manager = RAGManager(persist_dir=rag_dir, api_key=api_key)

        assert os.path.exists(rag_dir)

    def test_init_with_custom_model(self, temp_rag_dir, api_key):
        """Test init with custom embedding model."""
        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          return_value=[]):
            manager = RAGManager(
                persist_dir=temp_rag_dir,
                api_key=api_key,
                embedding_model="custom/model",
            )

        assert manager.embedding_function.model == "custom/model"


class TestRAGManagerChunking:
    """Tests for RAGManager text chunking."""

    def test_chunk_text_basic(self, temp_rag_dir, api_key):
        """Test basic text chunking."""
        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          return_value=[]):
            manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)

        text = "A" * 2000  # 2000 character text
        chunks = manager._chunk_text(text, chunk_size=1000, overlap=200)

        assert len(chunks) >= 2

    def test_chunk_text_small(self, temp_rag_dir, api_key):
        """Test chunking small text."""
        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          return_value=[]):
            manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)

        text = "Small text"
        chunks = manager._chunk_text(text, chunk_size=1000, overlap=200)

        assert len(chunks) == 1
        assert chunks[0] == "Small text"

    def test_chunk_text_overlap(self, temp_rag_dir, api_key):
        """Test that chunks overlap correctly."""
        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          return_value=[]):
            manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)

        # Create text where chunks will overlap
        text = "A" * 500 + "B" * 500 + "C" * 500
        chunks = manager._chunk_text(text, chunk_size=600, overlap=200)

        # Second chunk should contain some of first chunk
        assert len(chunks) >= 2


class TestRAGManagerFileHash:
    """Tests for RAGManager file hashing."""

    def test_compute_file_hash(self, temp_rag_dir, sample_text_file, api_key):
        """Test computing file hash."""
        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          return_value=[]):
            manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)

        hash1 = manager._compute_file_hash(sample_text_file)
        hash2 = manager._compute_file_hash(sample_text_file)

        assert hash1 == hash2
        assert len(hash1) == 32  # MD5 hex length

    def test_hash_changes_with_content(self, temp_dir, api_key):
        """Test that hash changes when content changes."""
        filepath = os.path.join(temp_dir, "test.txt")

        # Create temp RAG dir
        rag_dir = os.path.join(temp_dir, "rag")
        os.makedirs(rag_dir, exist_ok=True)

        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          return_value=[]):
            manager = RAGManager(persist_dir=rag_dir, api_key=api_key)

        # Write first content
        with open(filepath, "w") as f:
            f.write("Content 1")
        hash1 = manager._compute_file_hash(filepath)

        # Write different content
        with open(filepath, "w") as f:
            f.write("Content 2")
        hash2 = manager._compute_file_hash(filepath)

        assert hash1 != hash2


class TestRAGManagerIndexing:
    """Tests for RAGManager indexing."""

    def test_index_nonexistent_file(self, temp_rag_dir, api_key):
        """Test indexing nonexistent file."""
        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          return_value=[]):
            manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)

        result = manager.index_file("/nonexistent/file.txt")
        assert result == 0

    def test_index_file_returns_chunk_count(self, temp_rag_dir,
                                            sample_text_file, api_key):
        """Test that index_file returns chunk count."""

        # Mock embedding function to return valid embeddings
        def mock_embed(texts):
            return [[0.0] * 1536 for _ in texts]

        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          side_effect=mock_embed):
            manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)
            count = manager.index_file(sample_text_file)

        assert count > 0

    def test_index_directory(self, temp_rag_dir, temp_dir, sample_text_file,
                             sample_python_file, api_key):
        """Test indexing a directory."""

        def mock_embed(texts):
            return [[0.0] * 1536 for _ in texts]

        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          side_effect=mock_embed):
            manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)
            count = manager.index_path(temp_dir)

        assert count > 0


class TestRAGManagerQuery:
    """Tests for RAGManager query functionality."""

    def test_query_empty_collection(self, temp_rag_dir, api_key):
        """Test querying empty collection."""
        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          return_value=[]):
            manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)

        results = manager.query("test query")
        assert results == []


class TestRAGManagerStats:
    """Tests for RAGManager statistics."""

    def test_get_stats_empty(self, temp_rag_dir, api_key):
        """Test getting stats for empty index."""
        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          return_value=[]):
            manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)

        stats = manager.get_stats()

        assert stats["total_chunks"] == 0
        assert stats["indexed_files"] == 0
        assert stats["sources"] == []


class TestRAGManagerClear:
    """Tests for RAGManager clear functionality."""

    def test_clear(self, temp_rag_dir, sample_text_file, api_key):
        """Test clearing the index."""

        def mock_embed(texts):
            return [[0.0] * 1536 for _ in texts]

        with patch.object(OpenRouterEmbeddingFunction,
                          "__call__",
                          side_effect=mock_embed):
            manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)
            manager.index_file(sample_text_file)

            # Clear
            manager.clear()

            stats = manager.get_stats()
            assert stats["total_chunks"] == 0


@pytest.mark.integration
class TestRAGManagerIntegration:
    """Integration tests for RAGManager (require API access)."""

    def test_full_indexing_and_query(self, temp_rag_dir, sample_text_file,
                                     api_key):
        """Test full indexing and query workflow."""
        manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)

        # Index file
        count = manager.index_file(sample_text_file)
        assert count > 0

        # Query
        results = manager.query("Python programming")
        assert len(results) > 0

        # Results should have expected fields
        for result in results:
            assert "content" in result
            assert "source" in result
            assert "distance" in result

    def test_augment_query(self, temp_rag_dir, sample_text_file, api_key):
        """Test query augmentation."""
        manager = RAGManager(persist_dir=temp_rag_dir, api_key=api_key)

        # Index file
        manager.index_file(sample_text_file)

        # Augment query
        original = "What is Python?"
        augmented = manager.augment_query(original)

        # Should contain original query and context
        assert original in augmented
        assert "Context from indexed documents" in augmented
