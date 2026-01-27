"""Tests for agent module."""

import json
import pytest
from unittest.mock import MagicMock, patch
from io import StringIO

from rich.console import Console

from llm_repl.agent import Agent, AgentAction, AgentState
from llm_repl.session import Session
from llm_repl.llm import LLMClient


class TestAgentAction:
    """Tests for AgentAction class."""

    def test_create_action(self):
        """Test creating an action."""
        action = AgentAction(
            tool_name="test_func", tool_args={"arg1": "value1"}, tool_call_id="tc_123"
        )

        assert action.tool_name == "test_func"
        assert action.tool_args == {"arg1": "value1"}
        assert action.tool_call_id == "tc_123"
        assert action.result is None
        assert action.overridden is False

    def test_action_override(self):
        """Test action override."""
        action = AgentAction(tool_name="test", tool_args={}, tool_call_id="tc_1")
        action.overridden = True
        action.override_result = "custom result"

        assert action.overridden is True
        assert action.override_result == "custom result"


class TestAgentState:
    """Tests for AgentState enum."""

    def test_states_exist(self):
        """Test that all states exist."""
        assert AgentState.IDLE.value == "idle"
        assert AgentState.RUNNING.value == "running"
        assert AgentState.PAUSED.value == "paused"
        assert AgentState.COMPLETED.value == "completed"
        assert AgentState.ERROR.value == "error"


class TestAgentInit:
    """Tests for Agent initialization."""

    def test_init_with_valid_agent(self, api_key, test_model):
        """Test initializing with valid agent name."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        agent = Agent(
            agent_name="default",
            llm_client=llm_client,
            session=session,
            console=console,
        )

        assert agent.config.name == "default"
        assert agent.state == AgentState.IDLE

    def test_init_with_invalid_agent(self, api_key, test_model):
        """Test initializing with invalid agent name."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        with pytest.raises(ValueError, match="Unknown agent"):
            Agent(
                agent_name="nonexistent",
                llm_client=llm_client,
                session=session,
                console=console,
            )

    def test_init_adds_system_prompt(self, api_key, test_model):
        """Test that init adds system prompt."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        agent = Agent(
            agent_name="default",
            llm_client=llm_client,
            session=session,
            console=console,
        )

        # Should have added system prompt
        assert len(session.messages) == 1
        assert session.messages[0].role == "system"


class TestAgentExecuteAction:
    """Tests for Agent._execute_action method."""

    def test_execute_known_function(self, api_key, test_model):
        """Test executing a known function."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        agent = Agent(
            agent_name="default",
            llm_client=llm_client,
            session=session,
            console=console,
        )

        action = AgentAction(
            tool_name="execute_python", tool_args={"code": "2 + 2"}, tool_call_id="tc_1"
        )

        result = agent._execute_action(action)

        assert result["success"] is True
        assert result["result"] == "4"

    def test_execute_overridden_action(self, api_key, test_model):
        """Test executing an overridden action."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        agent = Agent(
            agent_name="default",
            llm_client=llm_client,
            session=session,
            console=console,
        )

        action = AgentAction(
            tool_name="execute_python",
            tool_args={"code": "something"},
            tool_call_id="tc_1",
        )
        action.overridden = True
        action.override_result = "custom result"

        result = agent._execute_action(action)

        assert result["result"] == "custom result"
        assert result["overridden"] is True

    def test_execute_unknown_function(self, api_key, test_model):
        """Test executing an unknown function."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        agent = Agent(
            agent_name="default",
            llm_client=llm_client,
            session=session,
            console=console,
        )

        action = AgentAction(
            tool_name="unknown_function", tool_args={}, tool_call_id="tc_1"
        )

        result = agent._execute_action(action)

        assert "error" in result


class TestAgentRun:
    """Tests for Agent.run method."""

    def test_run_simple_no_tools(self, api_key, test_model):
        """Test running agent with simple response (no tools)."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        agent = Agent(
            agent_name="default",
            llm_client=llm_client,
            session=session,
            console=console,
        )

        # Mock LLM to return simple response
        with patch.object(llm_client, "chat") as mock_chat:
            mock_chat.return_value = {
                "type": "finish",
                "content": "Hello!",
                "tool_calls": None,
                "finish_reason": "stop",
            }

            result = agent.run("Say hello", interactive=False)

        assert result == "Hello!"
        assert agent.state == AgentState.COMPLETED

    def test_run_with_tool_call(self, api_key, test_model):
        """Test running agent with tool call."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        agent = Agent(
            agent_name="default",
            llm_client=llm_client,
            session=session,
            console=console,
        )

        # First call returns tool call, second call returns final response
        call_count = 0

        def mock_chat(*args, **kwargs):
            nonlocal call_count
            call_count += 1
            if call_count == 1:
                return {
                    "type": "finish",
                    "content": "Let me calculate that.",
                    "tool_calls": [
                        {
                            "id": "tc_1",
                            "function": {
                                "name": "execute_python",
                                "arguments": '{"code": "2 + 2"}',
                            },
                        }
                    ],
                    "finish_reason": "tool_calls",
                }
            else:
                return {
                    "type": "finish",
                    "content": "The result is 4.",
                    "tool_calls": None,
                    "finish_reason": "stop",
                }

        with patch.object(llm_client, "chat", side_effect=mock_chat):
            result = agent.run("What is 2+2?", interactive=False)

        assert "4" in result
        assert agent.state == AgentState.COMPLETED

    def test_run_error_handling(self, api_key, test_model):
        """Test running agent with error response."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        agent = Agent(
            agent_name="default",
            llm_client=llm_client,
            session=session,
            console=console,
        )

        with patch.object(llm_client, "chat") as mock_chat:
            mock_chat.return_value = {"type": "error", "error": "API error"}

            result = agent.run("Test", interactive=False)

        assert "Error" in result
        assert agent.state == AgentState.ERROR

    def test_run_max_iterations(self, api_key, test_model):
        """Test that agent stops at max iterations."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        agent = Agent(
            agent_name="default",
            llm_client=llm_client,
            session=session,
            console=console,
        )

        # Always return tool call (infinite loop)
        def mock_chat(*args, **kwargs):
            return {
                "type": "finish",
                "content": "",
                "tool_calls": [
                    {
                        "id": "tc_1",
                        "function": {
                            "name": "execute_python",
                            "arguments": '{"code": "1"}',
                        },
                    }
                ],
                "finish_reason": "tool_calls",
            }

        with patch.object(llm_client, "chat", side_effect=mock_chat):
            result = agent.run("Loop forever", interactive=False)

        assert "maximum iterations" in result
        assert agent.current_iteration == agent.config.max_iterations


@pytest.mark.integration
class TestAgentIntegration:
    """Integration tests for Agent (require API access)."""

    def test_simple_calculation(self, api_key, test_model):
        """Test agent performing a simple calculation."""
        llm_client = LLMClient(api_key=api_key, model=test_model)
        session = Session(name="test")
        console = Console(file=StringIO())

        agent = Agent(
            agent_name="coder", llm_client=llm_client, session=session, console=console
        )

        result = agent.run(
            "Use execute_python to calculate 10 * 5 and tell me the result.",
            interactive=False,
        )

        # Agent should have used the tool and gotten 50
        assert agent.state == AgentState.COMPLETED
        # Session should have tool messages
        tool_messages = [m for m in session.messages if m.role == "tool"]
        assert len(tool_messages) > 0
