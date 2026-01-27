"""MCP (Model Context Protocol) client for external tool integration."""

import asyncio
import json
import logging
from beartype.typing import Optional, Any

from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

logger = logging.getLogger(__name__)


class MCPServerConfig:
    """Configuration for an MCP server."""

    def __init__(
        self,
        name: str,
        command: str,
        args: list[str] = None,
        env: dict[str, str] = None,
    ):
        self.name = name
        self.command = command
        self.args = args or []
        self.env = env or {}


class MCPManager:
    """Manages connections to MCP servers and tool execution."""

    def __init__(self):
        self.servers: dict[str, MCPServerConfig] = {}
        self.sessions: dict[str, ClientSession] = {}
        self.tools: dict[str, dict] = {}  # tool_name -> {server, tool_info}
        self._loop: Optional[asyncio.AbstractEventLoop] = None
        logger.debug("Initialized MCPManager")

    def register_server(self, config: MCPServerConfig) -> None:
        """Register an MCP server configuration."""
        self.servers[config.name] = config
        logger.info(f"Registered MCP server: {config.name}")
        logger.debug(f"  Command: {config.command} {' '.join(config.args)}")

    async def _connect_server(self, name: str) -> Optional[ClientSession]:
        """Connect to an MCP server."""
        if name not in self.servers:
            logger.error(f"Unknown MCP server: {name}")
            return None

        config = self.servers[name]
        logger.debug(f"Connecting to MCP server: {name}")
        logger.debug(f"  Command: {config.command}")
        logger.debug(f"  Args: {config.args}")

        try:
            server_params = StdioServerParameters(
                command=config.command,
                args=config.args,
                env=config.env if config.env else None,
            )

            async with stdio_client(server_params) as (read, write):
                async with ClientSession(read, write) as session:
                    logger.debug(f"Initializing MCP session for: {name}")
                    await session.initialize()
                    self.sessions[name] = session

                    # List available tools
                    logger.debug(f"Listing tools for MCP server: {name}")
                    tools_result = await session.list_tools()
                    for tool in tools_result.tools:
                        tool_name = f"{name}.{tool.name}"
                        self.tools[tool_name] = {
                            "server": name,
                            "name": tool.name,
                            "description": tool.description,
                            "input_schema": tool.inputSchema,
                        }
                        logger.info(f"Discovered MCP tool: {tool_name}")
                        logger.debug(
                            f"  Description: {tool.description[:100] if tool.description else 'N/A'}..."
                        )

                    logger.info(
                        f"Connected to MCP server {name}: {len(tools_result.tools)} tools available"
                    )
                    return session
        except Exception as e:
            logger.error(f"Failed to connect to MCP server {name}: {e}", exc_info=True)
            return None

    async def _call_tool(
        self, server_name: str, tool_name: str, arguments: dict[str, Any]
    ) -> dict[str, Any]:
        """Call a tool on an MCP server."""
        logger.debug(f"Calling MCP tool: {server_name}.{tool_name}")
        logger.debug(f"  Arguments: {json.dumps(arguments)[:200]}...")

        if server_name not in self.sessions:
            logger.error(f"Not connected to server: {server_name}")
            return {"error": f"Not connected to server: {server_name}"}

        session = self.sessions[server_name]
        try:
            result = await session.call_tool(tool_name, arguments)

            # Convert result to dict
            content = []
            for item in result.content:
                if hasattr(item, "text"):
                    content.append({"type": "text", "text": item.text})
                else:
                    content.append({"type": "unknown", "data": str(item)})

            is_error = result.isError if hasattr(result, "isError") else False
            logger.debug(
                f"MCP tool result: content_items={len(content)}, is_error={is_error}"
            )

            return {"success": True, "content": content, "is_error": is_error}
        except Exception as e:
            logger.error(f"Error calling MCP tool {tool_name}: {e}", exc_info=True)
            return {"error": str(e)}

    def connect_sync(self, server_name: str) -> bool:
        """Synchronously connect to an MCP server."""
        logger.debug(f"Synchronous connect to MCP server: {server_name}")
        try:
            loop = asyncio.get_event_loop()
        except RuntimeError:
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)

        self._loop = loop

        async def _connect():
            return await self._connect_server(server_name)

        result = loop.run_until_complete(_connect())
        return result is not None

    def call_tool_sync(
        self, full_tool_name: str, arguments: dict[str, Any]
    ) -> dict[str, Any]:
        """Synchronously call an MCP tool."""
        logger.debug(f"Synchronous call to MCP tool: {full_tool_name}")

        if full_tool_name not in self.tools:
            logger.error(f"Unknown MCP tool: {full_tool_name}")
            return {"error": f"Unknown tool: {full_tool_name}"}

        tool_info = self.tools[full_tool_name]
        server_name = tool_info["server"]
        tool_name = tool_info["name"]

        if self._loop is None:
            try:
                self._loop = asyncio.get_event_loop()
            except RuntimeError:
                self._loop = asyncio.new_event_loop()
                asyncio.set_event_loop(self._loop)

        return self._loop.run_until_complete(
            self._call_tool(server_name, tool_name, arguments)
        )

    def get_tools_for_llm(self) -> list[dict]:
        """Get MCP tools in OpenAI-compatible format."""
        llm_tools = []
        for full_name, tool_info in self.tools.items():
            llm_tools.append(
                {
                    "type": "function",
                    "function": {
                        "name": full_name.replace(
                            ".", "_"
                        ),  # OpenAI doesn't allow dots
                        "description": tool_info.get("description", ""),
                        "parameters": tool_info.get(
                            "input_schema", {"type": "object", "properties": {}}
                        ),
                    },
                }
            )
        logger.debug(f"Returning {len(llm_tools)} MCP tools for LLM")
        return llm_tools

    def list_tools(self) -> list[dict]:
        """List all available MCP tools."""
        tools_list = [
            {
                "name": name,
                "server": info["server"],
                "description": info.get("description", ""),
            }
            for name, info in self.tools.items()
        ]
        logger.debug(f"Listing {len(tools_list)} MCP tools")
        return tools_list


# Default MCP server configurations (can be extended in config.py)
DEFAULT_MCP_SERVERS: list[MCPServerConfig] = [
    # Example: filesystem server
    # MCPServerConfig(
    #     name="filesystem",
    #     command="npx",
    #     args=["-y", "@modelcontextprotocol/server-filesystem", "/tmp"]
    # ),
]
