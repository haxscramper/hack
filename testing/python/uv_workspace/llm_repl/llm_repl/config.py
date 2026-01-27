"""Configuration for LLM REPL - hardcoded functions, agents, and roles."""

import os
import logging
import platform
from beartype.typing import Any, Callable
from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)

# API Configuration
API_KEY = os.environ.get("HAXSCRAMPER_LLM_REPL_KEY", "")
DEFAULT_MODEL = "anthropic/claude-sonnet-4.5"
DEFAULT_EMBEDDING_MODEL = "openai/text-embedding-3-small"

# Data paths
DATA_DIR = os.path.join(os.path.dirname(os.path.dirname(__file__)), "data")
SESSIONS_DIR = os.path.join(DATA_DIR, "sessions")
RAG_DIR = os.path.join(DATA_DIR, "rag")

# Ensure directories exist
os.makedirs(SESSIONS_DIR, exist_ok=True)
os.makedirs(RAG_DIR, exist_ok=True)


class FunctionParameter(BaseModel):
    """Definition of a function parameter."""

    name: str
    type: str
    description: str
    required: bool = True
    default: Any = None


class FunctionDefinition(BaseModel):
    """Pydantic model for function definitions."""

    name: str
    description: str
    parameters: list[FunctionParameter] = Field(default_factory=list)
    python_function: str  # Name of the Python function to call


class AgentConfig(BaseModel):
    """Configuration for an agent."""

    name: str
    description: str
    system_prompt: str
    functions: list[str] = Field(default_factory=list)  # Function names
    max_iterations: int = 10


class RoleConfig(BaseModel):
    """Configuration for a role (system prompt preset)."""

    name: str
    description: str
    system_prompt: str


# =============================================================================
# Function Implementations
# =============================================================================


def execute_python(code: str) -> dict[str, Any]:
    """Execute Python code using eval/exec."""
    logger.info(f"Executing Python code: {code[:100]}...")
    try:
        # Try eval first for expressions
        result = eval(code)
        return {"success": True, "result": str(result), "type": "eval"}
    except SyntaxError:
        # Use exec for statements
        try:
            local_vars: dict[str, Any] = {}
            exec(code, {"__builtins__": __builtins__}, local_vars)
            return {"success": True, "result": str(local_vars), "type": "exec"}
        except Exception as e:
            return {"success": False, "error": str(e), "type": "exec"}
    except Exception as e:
        return {"success": False, "error": str(e), "type": "eval"}


def read_file(path: str) -> dict[str, Any]:
    """Read a file from the filesystem."""
    logger.info(f"Reading file: {path}")
    try:
        with open(path, "r") as f:
            content = f.read()
        return {"success": True, "content": content}
    except Exception as e:
        return {"success": False, "error": str(e)}


def write_file(path: str, content: str) -> dict[str, Any]:
    """Write content to a file."""
    logger.info(f"Writing file: {path}")
    try:
        with open(path, "w") as f:
            f.write(content)
        return {"success": True, "message": f"Written {len(content)} bytes to {path}"}
    except Exception as e:
        return {"success": False, "error": str(e)}


def list_directory(path: str) -> dict[str, Any]:
    """List contents of a directory."""
    logger.info(f"Listing directory: {path}")
    try:
        entries = os.listdir(path)
        return {"success": True, "entries": entries}
    except Exception as e:
        return {"success": False, "error": str(e)}


def shell_command(command: str) -> dict[str, Any]:
    """Execute a shell command."""
    import subprocess

    logger.info(f"Executing shell command: {command}")
    try:
        result = subprocess.run(
            command, shell=True, capture_output=True, text=True, timeout=30
        )
        return {
            "success": result.returncode == 0,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "returncode": result.returncode,
        }
    except subprocess.TimeoutExpired:
        return {"success": False, "error": "Command timed out after 30 seconds"}
    except Exception as e:
        return {"success": False, "error": str(e)}


# =============================================================================
# Function Registry
# =============================================================================

# Mapping of function names to actual Python functions
FUNCTION_REGISTRY: dict[str, Callable[..., dict[str, Any]]] = {
    "execute_python": execute_python,
    "read_file": read_file,
    "write_file": write_file,
    "list_directory": list_directory,
    "shell_command": shell_command,
}

# Function definitions for LLM
FUNCTION_DEFINITIONS: list[FunctionDefinition] = [
    FunctionDefinition(
        name="execute_python",
        description="Execute Python code and return the result. Use eval for expressions, exec for statements.",
        parameters=[
            FunctionParameter(
                name="code", type="string", description="The Python code to execute"
            )
        ],
        python_function="execute_python",
    ),
    FunctionDefinition(
        name="read_file",
        description="Read the contents of a file from the filesystem.",
        parameters=[
            FunctionParameter(
                name="path", type="string", description="The path to the file to read"
            )
        ],
        python_function="read_file",
    ),
    FunctionDefinition(
        name="write_file",
        description="Write content to a file on the filesystem.",
        parameters=[
            FunctionParameter(
                name="path", type="string", description="The path to the file to write"
            ),
            FunctionParameter(
                name="content",
                type="string",
                description="The content to write to the file",
            ),
        ],
        python_function="write_file",
    ),
    FunctionDefinition(
        name="list_directory",
        description="List the contents of a directory.",
        parameters=[
            FunctionParameter(
                name="path",
                type="string",
                description="The path to the directory to list",
            )
        ],
        python_function="list_directory",
    ),
    FunctionDefinition(
        name="shell_command",
        description="Execute a shell command and return the output.",
        parameters=[
            FunctionParameter(
                name="command",
                type="string",
                description="The shell command to execute",
            )
        ],
        python_function="shell_command",
    ),
]

# =============================================================================
# Agent Configurations
# =============================================================================

AGENT_CONFIGS: dict[str, AgentConfig] = {
    "default": AgentConfig(
        name="default",
        description="Default coding agent that can write and execute Python code",
        system_prompt="""You are a helpful coding assistant. You can execute Python code, read and write files, and run shell commands.

When asked to perform a task:
1. Break down the task into steps
2. Use the available tools to accomplish each step
3. Report your progress and results

Always be careful with file operations and shell commands. Verify paths before writing.""",
        functions=[
            "execute_python",
            "read_file",
            "write_file",
            "list_directory",
            "shell_command",
        ],
        max_iterations=10,
    ),
    "coder": AgentConfig(
        name="coder",
        description="Focused Python coding agent",
        system_prompt="""You are an expert Python programmer. You write clean, efficient, and well-documented code.

Use execute_python to test code snippets and verify your solutions work correctly.""",
        functions=["execute_python", "read_file", "write_file"],
        max_iterations=15,
    ),
}

# =============================================================================
# Role Configurations
# =============================================================================

ROLE_CONFIGS: dict[str, RoleConfig] = {
    "assistant": RoleConfig(
        name="assistant",
        description="General helpful assistant",
        system_prompt=f"You are a CLI assistant, running on the {platform.system()} OS, for user {os.getlogin()} on hostname {platform.system()}. Provide a single recommended option, do not list all options at once, be concise in the description.",
    ),
    "coder": RoleConfig(
        name="coder",
        description="Expert programmer",
        system_prompt="""You are an expert programmer with deep knowledge of Python, algorithms, and software design.
Provide clear, well-structured code with explanations. Focus on best practices and efficiency.""",
    ),
    "analyst": RoleConfig(
        name="analyst",
        description="Data analyst",
        system_prompt="""You are a data analyst expert. You help analyze data, create visualizations, and derive insights.
Be precise with statistics and always explain your methodology.""",
    ),
}


def get_function_tools() -> list[dict]:
    """Convert function definitions to OpenAI-compatible tool format."""
    tools = []
    for func_def in FUNCTION_DEFINITIONS:
        properties = {}
        required = []
        for param in func_def.parameters:
            properties[param.name] = {
                "type": param.type,
                "description": param.description,
            }
            if param.required:
                required.append(param.name)

        tools.append(
            {
                "type": "function",
                "function": {
                    "name": func_def.name,
                    "description": func_def.description,
                    "parameters": {
                        "type": "object",
                        "properties": properties,
                        "required": required,
                    },
                },
            }
        )
    return tools


def get_agent_tools(agent_name: str) -> list[dict]:
    """Get tools available for a specific agent."""
    if agent_name not in AGENT_CONFIGS:
        return []

    agent = AGENT_CONFIGS[agent_name]
    tools = []
    for func_def in FUNCTION_DEFINITIONS:
        if func_def.name in agent.functions:
            properties = {}
            required = []
            for param in func_def.parameters:
                properties[param.name] = {
                    "type": param.type,
                    "description": param.description,
                }
                if param.required:
                    required.append(param.name)

            tools.append(
                {
                    "type": "function",
                    "function": {
                        "name": func_def.name,
                        "description": func_def.description,
                        "parameters": {
                            "type": "object",
                            "properties": properties,
                            "required": required,
                        },
                    },
                }
            )
    return tools
