"""Pytest configuration and fixtures."""

import os
import tempfile
import pytest

from llm_repl.config import API_KEY


# Use a faster/cheaper model for testing
TEST_MODEL = "openai/gpt-4o-mini"


def pytest_configure(config):
    """Configure pytest with custom markers."""
    config.addinivalue_line(
        "markers", "integration: marks tests as integration tests (require API access)"
    )


@pytest.fixture
def api_key():
    """Get the API key from environment."""
    if not API_KEY:
        pytest.fail("HAXSCRAMPER_LLM_REPL_KEY not set")
    return API_KEY


@pytest.fixture
def test_model():
    """Return the test model name."""
    return TEST_MODEL


@pytest.fixture
def temp_dir():
    """Create a temporary directory for tests."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield tmpdir


@pytest.fixture
def temp_sessions_dir(temp_dir):
    """Create a temporary sessions directory."""
    sessions_dir = os.path.join(temp_dir, "sessions")
    os.makedirs(sessions_dir, exist_ok=True)
    return sessions_dir


@pytest.fixture
def temp_rag_dir(temp_dir):
    """Create a temporary RAG directory."""
    rag_dir = os.path.join(temp_dir, "rag")
    os.makedirs(rag_dir, exist_ok=True)
    return rag_dir


@pytest.fixture
def sample_text_file(temp_dir):
    """Create a sample text file for indexing."""
    filepath = os.path.join(temp_dir, "sample.txt")
    content = """This is a sample document for testing RAG functionality.
    
It contains multiple paragraphs with different topics.

The first topic is about Python programming. Python is a versatile
programming language used for web development, data science, and automation.

The second topic is about machine learning. Machine learning is a subset
of artificial intelligence that enables systems to learn from data.

The third topic is about natural language processing. NLP helps computers
understand and process human language.
"""
    with open(filepath, "w") as f:
        f.write(content)
    return filepath


@pytest.fixture
def sample_python_file(temp_dir):
    """Create a sample Python file for indexing."""
    filepath = os.path.join(temp_dir, "example.py")
    content = '''"""Example Python module for testing."""

def hello_world():
    """Print a greeting."""
    print("Hello, World!")

def add_numbers(a: int, b: int) -> int:
    """Add two numbers together."""
    return a + b

class Calculator:
    """A simple calculator class."""
    
    def __init__(self):
        self.result = 0
    
    def add(self, value: int) -> int:
        """Add a value to the result."""
        self.result += value
        return self.result
'''
    with open(filepath, "w") as f:
        f.write(content)
    return filepath
