"""Tests for CLI module."""

import os
import pytest
from click.testing import CliRunner
from unittest.mock import patch, MagicMock

from llm_repl.cli import main, setup_logging, MODULE_LOGGERS


class TestSetupLogging:
    """Tests for setup_logging function."""

    def test_setup_logging_no_debug(self):
        """Test logging setup without debug."""
        import logging

        setup_logging(debug=None, verbose=False)

        # Root logger should be set up
        root = logging.getLogger()
        assert len(root.handlers) >= 1

    def test_setup_logging_verbose(self):
        """Test logging setup with verbose."""
        import logging

        setup_logging(debug=None, verbose=True)

        # Check console handler level
        root = logging.getLogger()
        console_handlers = [
            h
            for h in root.handlers
            if isinstance(h, logging.StreamHandler)
            and not isinstance(h, logging.FileHandler)
        ]
        assert any(h.level == logging.INFO for h in console_handlers)

    def test_setup_logging_debug_all(self):
        """Test logging setup with debug all."""
        import logging

        setup_logging(debug="", verbose=False)

        # All module loggers should be at DEBUG
        for logger_name in MODULE_LOGGERS:
            logger = logging.getLogger(logger_name)
            assert logger.level == logging.DEBUG

    def test_setup_logging_debug_specific(self):
        """Test logging setup with specific loggers."""
        import logging

        setup_logging(debug="llm,rag", verbose=False)

        # Only specified loggers should be at DEBUG
        llm_logger = logging.getLogger("llm_repl.llm")
        rag_logger = logging.getLogger("llm_repl.rag")
        agent_logger = logging.getLogger("llm_repl.agent")

        assert llm_logger.level == logging.DEBUG
        assert rag_logger.level == logging.DEBUG
        assert agent_logger.level == logging.WARNING


class TestCLIHelp:
    """Tests for CLI help and options."""

    def test_help(self):
        """Test --help option."""
        runner = CliRunner()
        result = runner.invoke(main, ["--help"])

        assert result.exit_code == 0
        assert "LLM REPL" in result.output
        assert "--session" in result.output
        assert "--role" in result.output
        assert "--agent" in result.output
        assert "--debug" in result.output

    def test_list_sessions(self, temp_sessions_dir):
        """Test --list-sessions option."""
        runner = CliRunner()

        with patch("llm_repl.cli.SessionManager") as MockManager:
            mock_instance = MagicMock()
            mock_instance.list_sessions.return_value = ["session1", "session2"]
            MockManager.return_value = mock_instance

            result = runner.invoke(main, ["--list-sessions"])

        assert result.exit_code == 0
        assert "Available Sessions" in result.output

    def test_list_roles(self):
        """Test --list-roles option."""
        runner = CliRunner()
        result = runner.invoke(main, ["--list-roles"])

        assert result.exit_code == 0
        assert "Available Roles" in result.output
        assert "assistant" in result.output
        assert "coder" in result.output

    def test_list_agents(self):
        """Test --list-agents option."""
        runner = CliRunner()
        result = runner.invoke(main, ["--list-agents"])

        assert result.exit_code == 0
        assert "Available Agents" in result.output
        assert "default" in result.output


class TestCLIValidation:
    """Tests for CLI validation."""

    def test_missing_api_key(self):
        """Test error when API key is missing."""
        runner = CliRunner()

        with patch("llm_repl.cli.API_KEY", ""):
            result = runner.invoke(main, [])

        assert result.exit_code == 1
        assert "HAXSCRAMPER_LLM_REPL_KEY" in result.output

    def test_invalid_role(self):
        """Test error with invalid role."""
        runner = CliRunner()
        result = runner.invoke(main, ["--role", "invalid"])

        assert result.exit_code != 0

    def test_invalid_agent(self):
        """Test error with invalid agent."""
        runner = CliRunner()
        result = runner.invoke(main, ["--agent", "invalid"])

        assert result.exit_code != 0


class TestCLIOptions:
    """Tests for CLI option parsing."""

    def test_session_option(self, api_key):
        """Test --session option parsing."""
        runner = CliRunner()

        with patch("llm_repl.cli.API_KEY", api_key):
            with patch("llm_repl.cli.REPL") as MockREPL:
                mock_repl = MagicMock()
                MockREPL.return_value = mock_repl
                mock_repl.run.side_effect = KeyboardInterrupt()

                with patch("llm_repl.cli.LLMClient"):
                    with patch("llm_repl.cli.SessionManager") as MockSession:
                        mock_session = MagicMock()
                        mock_session.load_or_create.return_value = MagicMock(agent=None)
                        MockSession.return_value = mock_session

                        result = runner.invoke(main, ["--session", "custom_session"])

        # Should have tried to load/create the session
        mock_session.load_or_create.assert_called()

    def test_debug_all_option(self):
        """Test --debug-all flag."""
        runner = CliRunner()

        # Test that --debug-all is accepted as a flag (no API key so will fail early)
        with patch("llm_repl.cli.API_KEY", ""):
            result = runner.invoke(main, ["--debug-all"])

        # Should fail due to missing API key, not due to invalid option
        assert "HAXSCRAMPER_LLM_REPL_KEY" in result.output

    def test_debug_option_with_value(self):
        """Test --debug with specific loggers."""
        runner = CliRunner()

        with patch("llm_repl.cli.API_KEY", "test-key"):
            with patch("llm_repl.cli.setup_logging") as mock_setup:
                with patch("llm_repl.cli.REPL") as MockREPL:
                    mock_repl = MagicMock()
                    MockREPL.return_value = mock_repl
                    mock_repl.run.side_effect = KeyboardInterrupt()

                    with patch("llm_repl.cli.LLMClient"):
                        with patch("llm_repl.cli.SessionManager") as MockSession:
                            mock_session = MagicMock()
                            mock_session.load_or_create.return_value = MagicMock(
                                agent=None
                            )
                            MockSession.return_value = mock_session

                            result = runner.invoke(main, ["--debug", "llm,rag"])

                # setup_logging should have been called with debug="llm,rag"
                mock_setup.assert_called_once()
                call_args = mock_setup.call_args
                assert call_args[0][0] == "llm,rag"
