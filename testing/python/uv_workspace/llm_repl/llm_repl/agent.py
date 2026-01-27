"""Agent implementation with pause/resume capabilities."""

import json
import logging
from beartype.typing import Optional
from enum import Enum

from rich.console import Console
from rich.syntax import Syntax
from rich.prompt import Prompt

from .config import (
    AGENT_CONFIGS,
    FUNCTION_REGISTRY,
    get_agent_tools,
)
from .llm import LLMClient
from .session import Session

logger = logging.getLogger(__name__)


class AgentState(Enum):
    """Agent execution states."""

    IDLE = "idle"
    RUNNING = "running"
    PAUSED = "paused"
    WAITING_INPUT = "waiting_input"
    COMPLETED = "completed"
    ERROR = "error"


class AgentAction:
    """Represents an action the agent wants to take."""

    def __init__(self, tool_name: str, tool_args: dict, tool_call_id: str):
        self.tool_name = tool_name
        self.tool_args = tool_args
        self.tool_call_id = tool_call_id
        self.result: Optional[dict] = None
        self.overridden: bool = False
        self.override_result: Optional[str] = None


class Agent:
    """
    Agent that can execute tool calls with pause/resume capabilities.

    During agent work, the user can:
    - Pause execution to provide guidance
    - Override a tool call with custom results
    - Add additional context before continuing
    """

    def __init__(
        self, agent_name: str, llm_client: LLMClient, session: Session, console: Console
    ):
        if agent_name not in AGENT_CONFIGS:
            raise ValueError(f"Unknown agent: {agent_name}")

        self.config = AGENT_CONFIGS[agent_name]
        self.llm_client = llm_client
        self.session = session
        self.console = console
        self.state = AgentState.IDLE
        self.current_iteration = 0
        self.pending_actions: list[AgentAction] = []

        # Add system prompt if not already present
        if not any(m.role == "system" for m in session.messages):
            session.add_message("system", self.config.system_prompt)

        logger.info(f"Initialized agent: {agent_name}")
        logger.debug(
            f"Agent config: max_iterations={self.config.max_iterations}, functions={self.config.functions}"
        )

    def _display_action(self, action: AgentAction) -> None:
        """Display a pending tool action to the user."""
        args_json = json.dumps(action.tool_args, indent=2)

        self.console.print(
            f"\n[bold yellow]Tool Call: {action.tool_name}[/bold yellow]"
        )
        self.console.print(Syntax(args_json, "json", theme="monokai"))

    def _execute_action(self, action: AgentAction) -> dict:
        """Execute a tool action."""
        logger.debug(
            f"Executing action: {action.tool_name} with args: {action.tool_args}"
        )

        if action.overridden:
            logger.debug(f"Action overridden by user: {action.override_result}")
            return {"result": action.override_result, "overridden": True}

        func_name = action.tool_name
        if func_name not in FUNCTION_REGISTRY:
            logger.error(f"Unknown function: {func_name}")
            return {"error": f"Unknown function: {func_name}"}

        func = FUNCTION_REGISTRY[func_name]
        try:
            result = func(**action.tool_args)
            logger.debug(f"Action result: {json.dumps(result)[:200]}...")
            return result
        except Exception as e:
            logger.error(f"Error executing {func_name}: {e}", exc_info=True)
            return {"error": str(e)}

    def _prompt_for_action(self, action: AgentAction) -> str:
        """Prompt user for what to do with a pending action."""
        self._display_action(action)

        self.console.print("\n[bold]Options:[/bold]")
        self.console.print("  [green]r[/green] - Run this action")
        self.console.print("  [yellow]o[/yellow] - Override with custom result")
        self.console.print("  [blue]s[/blue] - Skip this action")
        self.console.print("  [magenta]p[/magenta] - Pause and provide guidance")
        self.console.print("  [red]a[/red] - Abort agent")

        choice = Prompt.ask("Choice", choices=["r", "o", "s", "p", "a"], default="r")
        logger.debug(f"User choice for action {action.tool_name}: {choice}")
        return choice

    def _handle_pause(self) -> Optional[str]:
        """Handle pause state and get user guidance."""
        self.state = AgentState.PAUSED
        logger.debug("Agent paused by user")
        self.console.print("\n[bold blue]Agent Paused[/bold blue]")
        self.console.print(
            "Enter guidance, run a command, or type 'continue' to resume."
        )

        guidance_parts = []
        while True:
            user_input = Prompt.ask("[pause]")

            if user_input.lower() == "continue":
                break
            elif user_input.lower() == "abort":
                logger.debug("Agent aborted during pause")
                return None
            elif user_input.startswith("!"):
                # Execute a shell command
                import subprocess

                cmd = user_input[1:].strip()
                logger.debug(f"User executing shell command during pause: {cmd}")
                try:
                    result = subprocess.run(
                        cmd, shell=True, capture_output=True, text=True, timeout=30
                    )
                    output = f"$ {cmd}\n{result.stdout}"
                    if result.stderr:
                        output += f"\n[stderr]: {result.stderr}"
                    self.console.print(output)
                    guidance_parts.append(
                        f"User executed: {cmd}\nOutput: {result.stdout}"
                    )
                except Exception as e:
                    self.console.print(f"[red]Error: {e}[/red]")
            else:
                guidance_parts.append(f"User guidance: {user_input}")

        self.state = AgentState.RUNNING
        guidance = "\n".join(guidance_parts) if guidance_parts else None
        if guidance:
            logger.debug(f"User provided guidance: {guidance[:100]}...")
        return guidance

    def run(self, initial_query: str, interactive: bool = True) -> str:
        """
        Run the agent to complete a task.

        Args:
            initial_query: The initial user query/task
            interactive: If True, prompt before each action. If False, run automatically.

        Returns:
            The final response from the agent
        """
        self.state = AgentState.RUNNING
        self.current_iteration = 0

        logger.info(f"Starting agent run: interactive={interactive}")
        logger.debug(f"Initial query: {initial_query[:100]}...")

        # Add user message
        self.session.add_message("user", initial_query)

        tools = get_agent_tools(self.config.name)
        logger.debug(f"Agent tools: {[t['function']['name'] for t in tools]}")
        final_response = ""

        while self.current_iteration < self.config.max_iterations:
            self.current_iteration += 1
            self.console.print(
                f"\n[dim]Iteration {self.current_iteration}/{self.config.max_iterations}[/dim]"
            )
            logger.debug(
                f"Agent iteration {self.current_iteration}/{self.config.max_iterations}"
            )

            # Get LLM response
            messages = self.session.get_messages_for_api()

            with self.console.status("[bold green]Thinking..."):
                response = self.llm_client.chat(
                    messages=messages, tools=tools, stream=False
                )

            if response.get("type") == "error":
                error_msg = response.get("error")
                self.console.print(f"[red]Error: {error_msg}[/red]")
                logger.error(f"Agent error: {error_msg}")
                self.state = AgentState.ERROR
                return f"Error: {error_msg}"

            content = response.get("content", "")
            tool_calls = response.get("tool_calls")

            logger.debug(
                f"LLM response: content_len={len(content)}, tool_calls={len(tool_calls) if tool_calls else 0}"
            )

            # Display content if any
            if content:
                self.console.print(f"\n[bold green]Agent[/bold green]")
                self.console.print(f"> {content}")

            # If no tool calls, we're done
            if not tool_calls:
                self.session.add_message("assistant", content)
                final_response = content
                logger.info("Agent completed (no more tool calls)")
                break

            # Add assistant message with tool calls
            self.session.add_message("assistant", content, tool_calls=tool_calls)

            # Process tool calls
            for tc in tool_calls:
                func_name = tc["function"]["name"]
                try:
                    func_args = json.loads(tc["function"]["arguments"])
                except json.JSONDecodeError:
                    func_args = {}

                logger.debug(
                    f"Processing tool call: {func_name}({json.dumps(func_args)[:100]}...)"
                )

                action = AgentAction(
                    tool_name=func_name, tool_args=func_args, tool_call_id=tc["id"]
                )

                # Interactive mode: prompt for each action
                if interactive:
                    choice = self._prompt_for_action(action)

                    if choice == "a":
                        self.state = AgentState.IDLE
                        logger.info("Agent aborted by user")
                        return "Agent aborted by user"

                    elif choice == "p":
                        guidance = self._handle_pause()
                        if guidance is None:
                            self.state = AgentState.IDLE
                            return "Agent aborted by user"
                        # Add guidance to conversation
                        self.session.add_message(
                            "user", f"[User intervention]: {guidance}"
                        )
                        # Re-prompt for this action
                        choice = self._prompt_for_action(action)

                    if choice == "o":
                        override = Prompt.ask("Enter custom result")
                        action.overridden = True
                        action.override_result = override
                        logger.debug(
                            f"User overriding action with: {override[:100]}..."
                        )

                    elif choice == "s":
                        action.overridden = True
                        action.override_result = "Action skipped by user"
                        logger.debug("User skipped action")

                # Execute the action
                with self.console.status(f"[bold yellow]Executing {func_name}..."):
                    result = self._execute_action(action)

                result_str = json.dumps(result, indent=2)

                # Display result
                self.console.print(f"\n[bold blue]Result: {func_name}[/bold blue]")
                self.console.print(Syntax(result_str, "json", theme="monokai"))

                # Add tool result to session
                self.session.add_message(
                    "tool", result_str, tool_call_id=tc["id"], name=func_name
                )

        if self.current_iteration >= self.config.max_iterations:
            final_response = "Agent reached maximum iterations"
            self.console.print(f"[yellow]{final_response}[/yellow]")
            logger.warning(
                f"Agent reached max iterations: {self.config.max_iterations}"
            )

        self.state = AgentState.COMPLETED
        logger.info(f"Agent run completed: state={self.state.value}")
        return final_response
