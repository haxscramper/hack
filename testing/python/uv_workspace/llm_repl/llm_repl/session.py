"""Session management for LLM REPL."""

import os
import json
import logging
from datetime import datetime
from beartype.typing import Optional
from pydantic import BaseModel, Field

from .config import SESSIONS_DIR

logger = logging.getLogger(__name__)


class Message(BaseModel):
    """A single message in the conversation."""

    role: str  # "user", "assistant", "system", "tool"
    content: str
    timestamp: datetime = Field(default_factory=datetime.now)
    tool_calls: Optional[list[dict]] = None
    tool_call_id: Optional[str] = None
    name: Optional[str] = None  # For tool messages


class Session(BaseModel):
    """A chat session with conversation history."""

    name: str
    created_at: datetime = Field(default_factory=datetime.now)
    updated_at: datetime = Field(default_factory=datetime.now)
    role: str = "assistant"
    agent: Optional[str] = None
    messages: list[Message] = Field(default_factory=list)
    metadata: dict = Field(default_factory=dict)

    def add_message(
        self,
        role: str,
        content: str,
        tool_calls: Optional[list[dict]] = None,
        tool_call_id: Optional[str] = None,
        name: Optional[str] = None,
    ) -> Message:
        """Add a message to the session."""
        msg = Message(
            role=role,
            content=content,
            tool_calls=tool_calls,
            tool_call_id=tool_call_id,
            name=name,
        )
        self.messages.append(msg)
        self.updated_at = datetime.now()

        content_preview = content[:50] + "..." if len(content) > 50 else content
        logger.debug(
            f"Added message: role={role}, content={content_preview}, tool_calls={len(tool_calls) if tool_calls else 0}"
        )
        return msg

    def get_messages_for_api(self) -> list[dict]:
        """Get messages in the format expected by the OpenAI API."""
        api_messages = []
        for msg in self.messages:
            message_dict: dict = {"role": msg.role, "content": msg.content}
            if msg.tool_calls:
                message_dict["tool_calls"] = msg.tool_calls
            if msg.tool_call_id:
                message_dict["tool_call_id"] = msg.tool_call_id
            if msg.name:
                message_dict["name"] = msg.name
            api_messages.append(message_dict)

        logger.debug(f"Prepared {len(api_messages)} messages for API")
        return api_messages

    def clear_history(self) -> None:
        """Clear the message history except for system messages."""
        old_count = len(self.messages)
        self.messages = [m for m in self.messages if m.role == "system"]
        self.updated_at = datetime.now()
        logger.debug(f"Cleared history: {old_count} -> {len(self.messages)} messages")


class SessionManager:
    """Manages session persistence."""

    def __init__(self, sessions_dir: str = SESSIONS_DIR):
        self.sessions_dir = sessions_dir
        os.makedirs(sessions_dir, exist_ok=True)
        logger.debug(f"Initialized SessionManager with dir: {sessions_dir}")

    def _get_session_path(self, name: str) -> str:
        """Get the file path for a session."""
        return os.path.join(self.sessions_dir, f"{name}.json")

    def save(self, session: Session) -> None:
        """Save a session to disk."""
        path = self._get_session_path(session.name)
        logger.debug(f"Saving session to: {path}")
        with open(path, "w") as f:
            json.dump(session.model_dump(mode="json"), f, indent=2, default=str)
        logger.info(f"Saved session: {session.name} ({len(session.messages)} messages)")

    def load(self, name: str) -> Optional[Session]:
        """Load a session from disk."""
        path = self._get_session_path(name)
        logger.debug(f"Loading session from: {path}")

        if not os.path.exists(path):
            logger.debug(f"Session file not found: {name}")
            return None

        try:
            with open(path, "r") as f:
                data = json.load(f)
            session = Session.model_validate(data)
            logger.info(f"Loaded session: {name} with {len(session.messages)} messages")
            logger.debug(f"  Role: {session.role}, Agent: {session.agent}")
            return session
        except Exception as e:
            logger.error(f"Failed to load session {name}: {e}", exc_info=True)
            return None

    def create(
        self, name: str, role: str = "assistant", agent: Optional[str] = None
    ) -> Session:
        """Create a new session."""
        session = Session(name=name, role=role, agent=agent)
        logger.info(f"Created new session: {name}")
        logger.debug(f"  Role: {role}, Agent: {agent}")
        return session

    def load_or_create(
        self, name: str, role: str = "assistant", agent: Optional[str] = None
    ) -> Session:
        """Load an existing session or create a new one."""
        logger.debug(f"load_or_create: name={name}, role={role}, agent={agent}")
        session = self.load(name)
        if session:
            # Update role/agent if specified
            if role != "assistant":
                session.role = role
                logger.debug(f"Updated session role to: {role}")
            if agent:
                session.agent = agent
                logger.debug(f"Updated session agent to: {agent}")
            return session
        return self.create(name, role, agent)

    def list_sessions(self) -> list[str]:
        """List all available sessions."""
        sessions = []
        for filename in os.listdir(self.sessions_dir):
            if filename.endswith(".json"):
                sessions.append(filename[:-5])  # Remove .json extension
        logger.debug(f"Found {len(sessions)} sessions")
        return sorted(sessions)

    def delete(self, name: str) -> bool:
        """Delete a session."""
        path = self._get_session_path(name)
        if os.path.exists(path):
            os.remove(path)
            logger.info(f"Deleted session: {name}")
            return True
        logger.debug(f"Session not found for deletion: {name}")
        return False
