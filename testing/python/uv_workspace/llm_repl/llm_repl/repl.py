"""REPL implementation with rich UI."""

import logging
from beartype.typing import Optional

from rich.console import Console
from rich.markdown import Markdown
from rich.syntax import Syntax
from rich.prompt import Prompt
from rich.live import Live
from rich.spinner import Spinner
from rich.text import Text

from .config import ROLE_CONFIGS, AGENT_CONFIGS, get_function_tools
from .llm import LLMClient
from .session import Session, SessionManager
from .rag import RAGManager
from .agent import Agent

logger = logging.getLogger(__name__)


class REPL:
    """Interactive REPL for LLM conversations."""

    def __init__(
        self,
        session: Session,
        llm_client: LLMClient,
        rag_manager: Optional[RAGManager] = None,
        use_functions: bool = False,
    ):
        self.session = session
        self.llm_client = llm_client
        self.rag_manager = rag_manager
        self.use_functions = use_functions
        self.console = Console()
        self.running = True

        logger.debug(
            f"Initializing REPL: session={session.name}, role={session.role}, agent={session.agent}"
        )

        # Set up system prompt based on role
        self._setup_system_prompt()

    def _setup_system_prompt(self) -> None:
        """Set up the system prompt based on role configuration."""
        role_name = self.session.role
        if role_name in ROLE_CONFIGS:
            role_config = ROLE_CONFIGS[role_name]
            # Check if system prompt already exists
            if not any(m.role == "system" for m in self.session.messages):
                self.session.add_message("system", role_config.system_prompt)
                logger.debug(f"Added system prompt for role: {role_name}")

    def _get_prompt_prefix(self) -> str:
        """Get the prompt prefix showing session/role or agent."""
        session_name = self.session.name
        if self.session.agent:
            return f"{session_name}/{self.session.agent}"
        return f"{session_name}/{self.session.role}"

    def _stream_response(self, query: str) -> str:
        """Stream a response from the LLM with progress indicator."""
        logger.debug(f"Processing query: {query[:100]}...")

        # Optionally augment with RAG
        augmented_query = query
        if self.rag_manager:
            augmented_query = self.rag_manager.augment_query(query)
            if augmented_query != query:
                logger.debug("Query augmented with RAG context")
                query = augmented_query

        # Add user message
        self.session.add_message("user", query)

        # Get messages for API
        messages = self.session.get_messages_for_api()

        # Get system prompt
        system_prompt = None
        if self.session.role in ROLE_CONFIGS:
            system_prompt = ROLE_CONFIGS[self.session.role].system_prompt

        # Get tools if enabled
        tools = get_function_tools() if self.use_functions else None
        if tools:
            logger.debug(f"Function calling enabled with {len(tools)} tools")

        # Stream response with spinner
        full_content = ""
        tool_calls = None

        logger.debug(f"Sending request with {len(messages)}")
        # for message in messages:


        # Show spinner while waiting for first token
        with Live(
            Spinner("dots", text="[bold green]Thinking...[/bold green]"),
            console=self.console,
            refresh_per_second=10,
        ) as live:
            first_token = True
            response_text = Text()

            for chunk in self.llm_client.chat(
                messages=messages, system_prompt=system_prompt, tools=tools, stream=True
            ):
                if chunk.get("type") == "content":
                    content = chunk.get("content", "")
                    full_content += content
                    response_text.append(content)

                    if first_token:
                        first_token = False
                        live.update(response_text)
                    else:
                        live.update(response_text)

                elif chunk.get("type") == "finish":
                    full_content = chunk.get("content", full_content)
                    tool_calls = chunk.get("tool_calls")

                elif chunk.get("type") == "error":
                    error = chunk.get("error", "Unknown error")
                    self.console.print(f"[red]Error: {error}[/red]")
                    logger.error(f"LLM error: {error}")
                    return ""

        # The response was already streamed and displayed by Live
        # No need to display again
        logger.debug(f"Response received: {len(full_content)} chars")

        # Add assistant message
        self.session.add_message("assistant", full_content, tool_calls=tool_calls)

        # Handle tool calls if present
        if tool_calls:
            logger.debug(f"Processing {len(tool_calls)} tool calls")
            self._handle_tool_calls(tool_calls)

        return full_content

    def _handle_tool_calls(self, tool_calls: list[dict]) -> None:
        """Handle tool calls from the LLM."""
        import json
        from .config import FUNCTION_REGISTRY

        for tc in tool_calls:
            func_name = tc["function"]["name"]
            try:
                func_args = json.loads(tc["function"]["arguments"])
            except json.JSONDecodeError:
                func_args = {}

            logger.debug(
                f"Executing function: {func_name}({json.dumps(func_args)[:100]}...)"
            )
            self.console.print(f"\n[yellow]Calling function: {func_name}[/yellow]")
            self.console.print(
                Syntax(json.dumps(func_args, indent=2), "json", theme="monokai")
            )

            if func_name in FUNCTION_REGISTRY:
                result = FUNCTION_REGISTRY[func_name](**func_args)
                result_str = json.dumps(result, indent=2)

                logger.debug(f"Function result: {result_str[:200]}...")
                self.console.print(f"\n[bold blue]Result: {func_name}[/bold blue]")
                self.console.print(Syntax(result_str, "json", theme="monokai"))

                # Add tool result to session
                self.session.add_message(
                    "tool", result_str, tool_call_id=tc["id"], name=func_name
                )

                # Get follow-up response
                self._stream_response("")
            else:
                logger.error(f"Unknown function: {func_name}")
                self.console.print(f"[red]Unknown function: {func_name}[/red]")

    def _handle_command(self, command: str) -> bool:
        """
        Handle special REPL commands.

        Returns True if command was handled, False otherwise.
        """
        parts = command.split(maxsplit=1)
        cmd = parts[0].lower()
        args = parts[1] if len(parts) > 1 else ""

        logger.debug(f"Handling command: {cmd} args={args}")

        if cmd in ["/help", "/h", "/?", "/?"]:
            self._show_help()
            return True

        elif cmd in ["/quit", "/q", "/exit"]:
            self.running = False
            self.console.print("[yellow]Goodbye![/yellow]")
            return True

        elif cmd == "/clear":
            self.session.clear_history()
            self._setup_system_prompt()
            self.console.print("[green]History cleared.[/green]")
            return True

        elif cmd == "/save":
            session_manager = SessionManager()
            session_manager.save(self.session)
            self.console.print(f"[green]Session saved: {self.session.name}[/green]")
            return True

        elif cmd == "/history":
            self._show_history()
            return True

        elif cmd == "/role":
            if args:
                if args in ROLE_CONFIGS:
                    self.session.role = args
                    self.session.clear_history()
                    self._setup_system_prompt()
                    self.console.print(f"[green]Switched to role: {args}[/green]")
                else:
                    self.console.print(f"[red]Unknown role: {args}[/red]")
                    self.console.print(
                        f"Available roles: {', '.join(ROLE_CONFIGS.keys())}"
                    )
            else:
                self.console.print(f"Current role: {self.session.role}")
                self.console.print(f"Available roles: {', '.join(ROLE_CONFIGS.keys())}")
            return True

        elif cmd == "/agent":
            if args:
                if args in AGENT_CONFIGS:
                    self.session.agent = args
                    self.console.print(f"[green]Agent mode: {args}[/green]")
                elif args == "off":
                    self.session.agent = None
                    self.console.print("[green]Agent mode disabled[/green]")
                else:
                    self.console.print(f"[red]Unknown agent: {args}[/red]")
                    self.console.print(
                        f"Available agents: {', '.join(AGENT_CONFIGS.keys())}"
                    )
            else:
                if self.session.agent:
                    self.console.print(f"Current agent: {self.session.agent}")
                else:
                    self.console.print("No agent active")
                self.console.print(
                    f"Available agents: {', '.join(AGENT_CONFIGS.keys())}"
                )
            return True

        elif cmd == "/rag":
            self._handle_rag_command(args)
            return True

        elif cmd == "/functions":
            self.use_functions = not self.use_functions
            status = "enabled" if self.use_functions else "disabled"
            self.console.print(f"[green]Function calling {status}[/green]")
            return True

        return False

    def _handle_rag_command(self, args: str) -> None:
        """Handle RAG-related commands."""
        if not self.rag_manager:
            self.rag_manager = RAGManager()

        parts = args.split(maxsplit=1)
        subcmd = parts[0] if parts else "status"
        subargs = parts[1] if len(parts) > 1 else ""

        logger.debug(f"RAG command: {subcmd} args={subargs}")

        if subcmd == "index":
            if subargs:
                import os

                if os.path.isfile(subargs):
                    count = self.rag_manager.index_file(subargs)
                    self.console.print(
                        f"[green]Indexed {count} chunks from {subargs}[/green]"
                    )
                elif os.path.isdir(subargs):
                    count = self.rag_manager.index_path(subargs)
                    self.console.print(
                        f"[green]Indexed {count} chunks from {subargs}[/green]"
                    )
                else:
                    self.console.print(f"[red]Path not found: {subargs}[/red]")
            else:
                self.console.print("[yellow]Usage: /rag index <path>[/yellow]")

        elif subcmd == "status":
            stats = self.rag_manager.get_stats()
            self.console.print(f"[bold]RAG Status[/bold]")
            self.console.print(f"  Total chunks: {stats['total_chunks']}")
            self.console.print(f"  Indexed files: {stats['indexed_files']}")

        elif subcmd == "clear":
            self.rag_manager.clear()
            self.console.print("[green]RAG index cleared[/green]")

        elif subcmd == "query":
            if subargs:
                results = self.rag_manager.query(subargs)
                if results:
                    for i, r in enumerate(results):
                        self.console.print(f"\n[bold]Result {i + 1}[/bold]")
                        self.console.print(f"[dim]Source: {r['source']}[/dim]")
                        self.console.print(f"{r['content'][:500]}...")
                else:
                    self.console.print("[yellow]No results found[/yellow]")
            else:
                self.console.print("[yellow]Usage: /rag query <text>[/yellow]")

        else:
            self.console.print(
                "[yellow]RAG commands: index, status, clear, query[/yellow]"
            )

    def _show_help(self) -> None:
        """Show help message."""
        help_text = """
**REPL Commands:**

- `/help`, `/h` - Show this help
- `/quit`, `/q` - Exit the REPL
- `/clear` - Clear conversation history
- `/save` - Save current session
- `/history` - Show conversation history
- `/role [name]` - Show or change role
- `/agent [name|off]` - Enable/disable agent mode
- `/rag <subcmd>` - RAG commands (index, status, clear, query)
- `/functions` - Toggle function calling

**In Agent Mode:**
- `r` - Run the action
- `o` - Override with custom result
- `s` - Skip the action
- `p` - Pause and provide guidance
- `a` - Abort agent

**Tips:**
- Use RAG to index files for context-aware responses
- Agent mode allows step-by-step task execution with intervention
"""
        self.console.print(Markdown(help_text))

    def _show_history(self) -> None:
        """Show conversation history."""
        for msg in self.session.messages:
            if msg.role == "system":
                continue

            role_style = {
                "user": "bold blue",
                "assistant": "bold green",
                "tool": "bold yellow",
            }.get(msg.role, "")

            content = (
                msg.content[:200] + "..." if len(msg.content) > 200 else msg.content
            )
            self.console.print(f"[{role_style}]{msg.role}:[/{role_style}] {content}")

    def run(self) -> None:
        """Run the REPL loop."""
        self.console.print(
            f"[bold]LLM REPL[/bold] - Session: [bold]{self.session.name}[/bold]"
        )
        self.console.print(
            f"Role: {self.session.role} | Agent: {self.session.agent or 'none'}"
        )
        self.console.print("Type /help for commands\n")

        logger.info(
            f"Starting REPL: session={self.session.name}, role={self.session.role}, agent={self.session.agent}"
        )

        while self.running:
            try:
                prompt_prefix = self._get_prompt_prefix()
                user_input = Prompt.ask(f"[bold cyan]{prompt_prefix})[/bold cyan]")

                if not user_input.strip():
                    continue

                # Check for commands
                if user_input.startswith("/"):
                    if self._handle_command(user_input):
                        continue

                # Agent mode
                if self.session.agent:
                    logger.debug(f"Running in agent mode: {self.session.agent}")
                    agent = Agent(
                        agent_name=self.session.agent,
                        llm_client=self.llm_client,
                        session=self.session,
                        console=self.console,
                    )
                    agent.run(user_input, interactive=True)
                else:
                    # Regular chat mode
                    self._stream_response(user_input)

            except KeyboardInterrupt:
                self.console.print("\n[yellow]Use /quit to exit[/yellow]")
            except EOFError:
                self.running = False
                self.console.print("\n[yellow]Goodbye![/yellow]")

        # Save session on exit
        logger.info("Saving session on exit")
        session_manager = SessionManager()
        session_manager.save(self.session)
