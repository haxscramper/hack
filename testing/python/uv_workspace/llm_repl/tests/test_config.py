"""Tests for config module."""

import pytest

from llm_repl.config import (
    FUNCTION_REGISTRY,
    FUNCTION_DEFINITIONS,
    AGENT_CONFIGS,
    ROLE_CONFIGS,
    FunctionDefinition,
    FunctionParameter,
    AgentConfig,
    RoleConfig,
    get_function_tools,
    get_agent_tools,
    execute_python,
    read_file,
    write_file,
    list_directory,
    shell_command,
)


class TestFunctionDefinitions:
    """Tests for function definitions."""

    def test_function_definitions_exist(self):
        """Test that function definitions are defined."""
        assert len(FUNCTION_DEFINITIONS) > 0

    def test_all_functions_have_implementations(self):
        """Test that all defined functions have implementations."""
        for func_def in FUNCTION_DEFINITIONS:
            assert func_def.python_function in FUNCTION_REGISTRY

    def test_function_definition_structure(self):
        """Test that function definitions have correct structure."""
        for func_def in FUNCTION_DEFINITIONS:
            assert isinstance(func_def, FunctionDefinition)
            assert func_def.name
            assert func_def.description
            assert isinstance(func_def.parameters, list)

    def test_parameter_structure(self):
        """Test that parameters have correct structure."""
        for func_def in FUNCTION_DEFINITIONS:
            for param in func_def.parameters:
                assert isinstance(param, FunctionParameter)
                assert param.name
                assert param.type
                assert param.description


class TestAgentConfigs:
    """Tests for agent configurations."""

    def test_agent_configs_exist(self):
        """Test that agent configs are defined."""
        assert len(AGENT_CONFIGS) > 0

    def test_default_agent_exists(self):
        """Test that default agent is defined."""
        assert "default" in AGENT_CONFIGS

    def test_agent_config_structure(self):
        """Test that agent configs have correct structure."""
        for name, config in AGENT_CONFIGS.items():
            assert isinstance(config, AgentConfig)
            assert config.name == name
            assert config.description
            assert config.system_prompt
            assert isinstance(config.functions, list)
            assert config.max_iterations > 0

    def test_agent_functions_exist(self):
        """Test that agent functions are defined in registry."""
        for config in AGENT_CONFIGS.values():
            for func_name in config.functions:
                assert func_name in FUNCTION_REGISTRY


class TestRoleConfigs:
    """Tests for role configurations."""

    def test_role_configs_exist(self):
        """Test that role configs are defined."""
        assert len(ROLE_CONFIGS) > 0

    def test_assistant_role_exists(self):
        """Test that assistant role is defined."""
        assert "assistant" in ROLE_CONFIGS

    def test_role_config_structure(self):
        """Test that role configs have correct structure."""
        for name, config in ROLE_CONFIGS.items():
            assert isinstance(config, RoleConfig)
            assert config.name == name
            assert config.description
            assert config.system_prompt


class TestGetFunctionTools:
    """Tests for get_function_tools helper."""

    def test_returns_list(self):
        """Test that get_function_tools returns a list."""
        tools = get_function_tools()
        assert isinstance(tools, list)

    def test_tool_format(self):
        """Test that tools have correct format."""
        tools = get_function_tools()
        for tool in tools:
            assert tool["type"] == "function"
            assert "function" in tool
            assert "name" in tool["function"]
            assert "description" in tool["function"]
            assert "parameters" in tool["function"]
            assert tool["function"]["parameters"]["type"] == "object"


class TestGetAgentTools:
    """Tests for get_agent_tools helper."""

    def test_returns_list_for_valid_agent(self):
        """Test that get_agent_tools returns a list for valid agent."""
        tools = get_agent_tools("default")
        assert isinstance(tools, list)
        assert len(tools) > 0

    def test_returns_empty_for_invalid_agent(self):
        """Test that get_agent_tools returns empty list for invalid agent."""
        tools = get_agent_tools("nonexistent")
        assert tools == []

    def test_tools_match_agent_functions(self):
        """Test that returned tools match agent's function list."""
        for agent_name, config in AGENT_CONFIGS.items():
            tools = get_agent_tools(agent_name)
            tool_names = [t["function"]["name"] for t in tools]
            assert set(tool_names) == set(config.functions)


class TestExecutePython:
    """Tests for execute_python function."""

    def test_eval_expression(self):
        """Test evaluating a simple expression."""
        result = execute_python("2 + 2")
        assert result["success"] is True
        assert result["result"] == "4"
        assert result["type"] == "eval"

    def test_eval_string(self):
        """Test evaluating a string expression."""
        result = execute_python("'hello'.upper()")
        assert result["success"] is True
        assert result["result"] == "HELLO"

    def test_exec_assignment(self):
        """Test executing an assignment statement."""
        result = execute_python("x = 10")
        assert result["success"] is True
        assert result["type"] == "exec"

    def test_error_handling(self):
        """Test error handling for invalid code."""
        result = execute_python("undefined_variable")
        assert result["success"] is False
        assert "error" in result


class TestReadFile:
    """Tests for read_file function."""

    def test_read_existing_file(self, sample_text_file):
        """Test reading an existing file."""
        result = read_file(sample_text_file)
        assert result["success"] is True
        assert "content" in result
        assert "sample document" in result["content"]

    def test_read_nonexistent_file(self):
        """Test reading a nonexistent file."""
        result = read_file("/nonexistent/file.txt")
        assert result["success"] is False
        assert "error" in result


class TestWriteFile:
    """Tests for write_file function."""

    def test_write_new_file(self, temp_dir):
        """Test writing a new file."""
        filepath = f"{temp_dir}/test_output.txt"
        result = write_file(filepath, "test content")
        assert result["success"] is True

        # Verify file was written
        with open(filepath, "r") as f:
            assert f.read() == "test content"

    def test_overwrite_file(self, sample_text_file):
        """Test overwriting an existing file."""
        result = write_file(sample_text_file, "new content")
        assert result["success"] is True

        with open(sample_text_file, "r") as f:
            assert f.read() == "new content"


class TestListDirectory:
    """Tests for list_directory function."""

    def test_list_existing_directory(self, temp_dir, sample_text_file):
        """Test listing an existing directory."""
        result = list_directory(temp_dir)
        assert result["success"] is True
        assert "entries" in result
        assert "sample.txt" in result["entries"]

    def test_list_nonexistent_directory(self):
        """Test listing a nonexistent directory."""
        result = list_directory("/nonexistent/directory")
        assert result["success"] is False
        assert "error" in result


class TestShellCommand:
    """Tests for shell_command function."""

    def test_simple_command(self):
        """Test running a simple command."""
        result = shell_command("echo 'hello world'")
        assert result["success"] is True
        assert "hello world" in result["stdout"]
        assert result["returncode"] == 0

    def test_command_with_exit_code(self):
        """Test command that returns non-zero exit code."""
        result = shell_command("exit 1")
        assert result["success"] is False
        assert result["returncode"] == 1

    def test_command_with_stderr(self):
        """Test command that writes to stderr."""
        result = shell_command("ls /nonexistent 2>&1 || true")
        assert result["success"] is True
