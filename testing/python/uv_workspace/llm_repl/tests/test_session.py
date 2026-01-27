"""Tests for session module."""

import os
import json
import pytest
from datetime import datetime

from llm_repl.session import Message, Session, SessionManager


class TestMessage:
    """Tests for Message class."""

    def test_create_message(self):
        """Test creating a message."""
        msg = Message(role="user", content="Hello")
        assert msg.role == "user"
        assert msg.content == "Hello"
        assert isinstance(msg.timestamp, datetime)

    def test_message_with_tool_calls(self):
        """Test creating a message with tool calls."""
        tool_calls = [{"id": "1", "function": {"name": "test"}}]
        msg = Message(role="assistant", content="", tool_calls=tool_calls)
        assert msg.tool_calls == tool_calls

    def test_message_with_tool_call_id(self):
        """Test creating a tool result message."""
        msg = Message(
            role="tool", content="result", tool_call_id="123", name="test_func"
        )
        assert msg.tool_call_id == "123"
        assert msg.name == "test_func"


class TestSession:
    """Tests for Session class."""

    def test_create_session(self):
        """Test creating a session."""
        session = Session(name="test")
        assert session.name == "test"
        assert session.role == "assistant"
        assert session.agent is None
        assert len(session.messages) == 0

    def test_create_session_with_role(self):
        """Test creating a session with a specific role."""
        session = Session(name="test", role="coder")
        assert session.role == "coder"

    def test_add_message(self):
        """Test adding a message to session."""
        session = Session(name="test")
        msg = session.add_message("user", "Hello")

        assert len(session.messages) == 1
        assert msg.role == "user"
        assert msg.content == "Hello"

    def test_add_message_with_tool_calls(self):
        """Test adding a message with tool calls."""
        session = Session(name="test")
        tool_calls = [{"id": "1", "function": {"name": "test"}}]
        msg = session.add_message("assistant", "content", tool_calls=tool_calls)

        assert msg.tool_calls == tool_calls

    def test_get_messages_for_api(self):
        """Test getting messages in API format."""
        session = Session(name="test")
        session.add_message("system", "You are helpful")
        session.add_message("user", "Hello")
        session.add_message("assistant", "Hi there!")

        api_messages = session.get_messages_for_api()

        assert len(api_messages) == 3
        assert api_messages[0]["role"] == "system"
        assert api_messages[1]["role"] == "user"
        assert api_messages[2]["role"] == "assistant"

    def test_get_messages_for_api_with_tool_calls(self):
        """Test getting messages with tool calls in API format."""
        session = Session(name="test")
        tool_calls = [{"id": "tc_1", "function": {"name": "test", "arguments": "{}"}}]
        session.add_message("assistant", "", tool_calls=tool_calls)
        session.add_message("tool", "result", tool_call_id="tc_1", name="test")

        api_messages = session.get_messages_for_api()

        assert api_messages[0]["tool_calls"] == tool_calls
        assert api_messages[1]["tool_call_id"] == "tc_1"
        assert api_messages[1]["name"] == "test"

    def test_clear_history(self):
        """Test clearing message history."""
        session = Session(name="test")
        session.add_message("system", "System prompt")
        session.add_message("user", "Hello")
        session.add_message("assistant", "Hi")

        session.clear_history()

        assert len(session.messages) == 1
        assert session.messages[0].role == "system"

    def test_updated_at_changes(self):
        """Test that updated_at changes when messages are added."""
        session = Session(name="test")
        original_updated = session.updated_at

        import time

        time.sleep(0.01)

        session.add_message("user", "Hello")
        assert session.updated_at > original_updated


class TestSessionManager:
    """Tests for SessionManager class."""

    def test_create_manager(self, temp_sessions_dir):
        """Test creating a session manager."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)
        assert manager.sessions_dir == temp_sessions_dir

    def test_create_session(self, temp_sessions_dir):
        """Test creating a new session."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)
        session = manager.create("test_session")

        assert session.name == "test_session"
        assert session.role == "assistant"

    def test_create_session_with_role_and_agent(self, temp_sessions_dir):
        """Test creating a session with role and agent."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)
        session = manager.create("test", role="coder", agent="default")

        assert session.role == "coder"
        assert session.agent == "default"

    def test_save_and_load_session(self, temp_sessions_dir):
        """Test saving and loading a session."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)

        # Create and save
        session = manager.create("test_session")
        session.add_message("user", "Hello")
        session.add_message("assistant", "Hi there!")
        manager.save(session)

        # Load
        loaded = manager.load("test_session")

        assert loaded is not None
        assert loaded.name == "test_session"
        assert len(loaded.messages) == 2

    def test_load_nonexistent_session(self, temp_sessions_dir):
        """Test loading a nonexistent session."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)
        session = manager.load("nonexistent")
        assert session is None

    def test_load_or_create_new(self, temp_sessions_dir):
        """Test load_or_create creates new session."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)
        session = manager.load_or_create("new_session")

        assert session.name == "new_session"

    def test_load_or_create_existing(self, temp_sessions_dir):
        """Test load_or_create loads existing session."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)

        # Create and save
        original = manager.create("test")
        original.add_message("user", "Hello")
        manager.save(original)

        # Load or create
        loaded = manager.load_or_create("test")

        assert len(loaded.messages) == 1

    def test_load_or_create_updates_role(self, temp_sessions_dir):
        """Test load_or_create updates role for existing session."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)

        # Create with default role
        original = manager.create("test")
        manager.save(original)

        # Load with different role
        loaded = manager.load_or_create("test", role="coder")

        assert loaded.role == "coder"

    def test_list_sessions(self, temp_sessions_dir):
        """Test listing sessions."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)

        # Create some sessions
        manager.save(manager.create("alpha"))
        manager.save(manager.create("beta"))
        manager.save(manager.create("gamma"))

        sessions = manager.list_sessions()

        assert len(sessions) == 3
        assert "alpha" in sessions
        assert "beta" in sessions
        assert "gamma" in sessions

    def test_delete_session(self, temp_sessions_dir):
        """Test deleting a session."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)

        # Create and save
        session = manager.create("to_delete")
        manager.save(session)

        # Verify exists
        assert manager.load("to_delete") is not None

        # Delete
        result = manager.delete("to_delete")

        assert result is True
        assert manager.load("to_delete") is None

    def test_delete_nonexistent_session(self, temp_sessions_dir):
        """Test deleting a nonexistent session."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)
        result = manager.delete("nonexistent")
        assert result is False

    def test_session_persistence_format(self, temp_sessions_dir):
        """Test that sessions are saved as valid JSON."""
        manager = SessionManager(sessions_dir=temp_sessions_dir)

        session = manager.create("json_test")
        session.add_message("user", "Hello")
        manager.save(session)

        # Read raw JSON
        filepath = os.path.join(temp_sessions_dir, "json_test.json")
        with open(filepath, "r") as f:
            data = json.load(f)

        assert data["name"] == "json_test"
        assert len(data["messages"]) == 1
