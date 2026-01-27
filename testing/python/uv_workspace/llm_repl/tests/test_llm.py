"""Tests for LLM client module."""

import pytest
from unittest.mock import MagicMock, patch

from llm_repl.llm import LLMClient


class TestLLMClientInit:
    """Tests for LLMClient initialization."""

    def test_init_with_api_key(self, api_key, test_model):
        """Test initializing with API key."""
        client = LLMClient(api_key=api_key, model=test_model)
        assert client.api_key == api_key
        assert client.model == test_model

    def test_init_without_api_key_raises(self):
        """Test that init raises without API key."""
        with patch("llm_repl.llm.API_KEY", ""):
            with pytest.raises(ValueError, match="API key not provided"):
                LLMClient(api_key="")

    def test_default_model(self, api_key):
        """Test that default model is set."""
        client = LLMClient(api_key=api_key)
        assert client.model is not None


class TestLLMClientChat:
    """Tests for LLMClient.chat method."""

    def test_chat_prepares_messages(self, api_key, test_model):
        """Test that chat prepares messages correctly."""
        client = LLMClient(api_key=api_key, model=test_model)

        messages = [{"role": "user", "content": "Hello"}]

        with patch.object(client, "_stream_response") as mock_stream:
            mock_stream.return_value = iter([{"type": "finish", "content": "Hi"}])
            list(client.chat(messages, stream=True))

            # Verify messages were passed
            call_args = mock_stream.call_args[0]
            assert len(call_args[0]) == 1

    def test_chat_adds_system_prompt(self, api_key, test_model):
        """Test that chat adds system prompt."""
        client = LLMClient(api_key=api_key, model=test_model)

        messages = [{"role": "user", "content": "Hello"}]
        system_prompt = "You are helpful"

        with patch.object(client, "_stream_response") as mock_stream:
            mock_stream.return_value = iter([{"type": "finish", "content": "Hi"}])
            list(client.chat(messages, system_prompt=system_prompt, stream=True))

            call_args = mock_stream.call_args[0]
            assert len(call_args[0]) == 2
            assert call_args[0][0]["role"] == "system"


@pytest.mark.integration
class TestLLMClientIntegration:
    """Integration tests for LLMClient (require API access)."""

    def test_simple_chat_stream(self, api_key, test_model):
        """Test a simple streaming chat request."""
        client = LLMClient(api_key=api_key, model=test_model)

        messages = [{"role": "user", "content": "Say hello in exactly one word."}]

        response_parts = []
        for chunk in client.chat(messages, stream=True):
            response_parts.append(chunk)

        # Should have content chunks and a finish
        assert len(response_parts) > 0

        # Last chunk should be finish
        finish_chunks = [c for c in response_parts if c.get("type") == "finish"]
        assert len(finish_chunks) == 1
        assert finish_chunks[0]["content"]

    def test_simple_chat_non_stream(self, api_key, test_model):
        """Test a simple non-streaming chat request."""
        client = LLMClient(api_key=api_key, model=test_model)

        messages = [
            {"role": "user", "content": "What is 2+2? Answer with just the number."}
        ]

        response = client.chat(messages, stream=False)

        assert response["type"] == "finish"
        assert "4" in response["content"]

    def test_chat_with_system_prompt(self, api_key, test_model):
        """Test chat with system prompt."""
        client = LLMClient(api_key=api_key, model=test_model)

        messages = [{"role": "user", "content": "What are you?"}]
        system_prompt = "You are a helpful pirate. Always respond with 'Arrr!'."

        response = client.chat(messages, system_prompt=system_prompt, stream=False)

        assert response["type"] == "finish"
        assert response["content"]  # Should have some response

    def test_multi_turn_conversation(self, api_key, test_model):
        """Test multi-turn conversation."""
        client = LLMClient(api_key=api_key, model=test_model)

        messages = [
            {"role": "user", "content": "My name is Alice."},
            {"role": "assistant", "content": "Nice to meet you, Alice!"},
            {"role": "user", "content": "What is my name?"},
        ]

        response = client.chat(messages, stream=False)

        assert response["type"] == "finish"
        assert "Alice" in response["content"]

    def test_chat_with_tools(self, api_key, test_model):
        """Test chat with tool definitions."""
        client = LLMClient(api_key=api_key, model=test_model)

        tools = [
            {
                "type": "function",
                "function": {
                    "name": "get_weather",
                    "description": "Get the weather for a location",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "location": {"type": "string", "description": "City name"}
                        },
                        "required": ["location"],
                    },
                },
            }
        ]

        messages = [{"role": "user", "content": "What's the weather in Paris?"}]

        response = client.chat(messages, tools=tools, stream=False)

        assert response["type"] == "finish"
        # Model should either respond with text or use the tool
        assert response["content"] or response["tool_calls"]

    def test_error_handling_invalid_model(self, api_key):
        """Test error handling for invalid model."""
        client = LLMClient(api_key=api_key, model="invalid/nonexistent-model")

        messages = [{"role": "user", "content": "Hello"}]
        response = client.chat(messages, stream=False)

        # Should return error instead of crashing
        assert response["type"] == "error"
        assert "error" in response
