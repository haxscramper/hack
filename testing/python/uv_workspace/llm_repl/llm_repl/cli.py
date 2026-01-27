"""CLI entry point for LLM REPL."""

import logging
import sys
from beartype.typing import Optional

import click
from rich.console import Console

from .config import ROLE_CONFIGS, AGENT_CONFIGS, DEFAULT_MODEL, API_KEY
from .llm import LLMClient
from .session import SessionManager
from .rag import RAGManager
from .repl import REPL

logger = logging.getLogger(__name__)

# Define module loggers for debug filtering
MODULE_LOGGERS = [
    "llm_repl.llm",
    "llm_repl.rag",
    "llm_repl.agent",
    "llm_repl.repl",
    "llm_repl.session",
    "llm_repl.config",
    "llm_repl.mcp_client",
    "llm_repl.cli",
]


def setup_logging(debug: Optional[str], verbose: bool) -> None:
    """
    Configure logging based on debug and verbose flags.

    Args:
        debug: Optional comma-separated list of logger names to enable debug for.
               If empty string, enables debug for all llm_repl loggers.
               If None, debug is disabled.
        verbose: If True and debug is None, enables INFO level logging.
    """
    # Base configuration - log to file at DEBUG level always
    root_logger = logging.getLogger()
    root_logger.setLevel(logging.DEBUG)

    # Clear any existing handlers
    root_logger.handlers = []

    # File handler - always logs everything
    file_handler = logging.FileHandler("llm_repl.log")
    file_handler.setLevel(logging.DEBUG)
    file_handler.setFormatter(
        logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
    )
    root_logger.addHandler(file_handler)

    # Console handler - respects debug/verbose settings
    console_handler = logging.StreamHandler(sys.stderr)
    console_handler.setFormatter(
        logging.Formatter("%(name)s - %(levelname)s - %(message)s")
    )

    if debug is not None:
        # Debug mode enabled
        if debug == "":
            # Enable debug for all llm_repl loggers
            console_handler.setLevel(logging.DEBUG)
            for logger_name in MODULE_LOGGERS:
                logging.getLogger(logger_name).setLevel(logging.DEBUG)
        else:
            # Enable debug only for specified loggers
            console_handler.setLevel(logging.DEBUG)
            specified_loggers = [name.strip() for name in debug.split(",")]

            # Set all llm_repl loggers to WARNING by default
            for logger_name in MODULE_LOGGERS:
                logging.getLogger(logger_name).setLevel(logging.WARNING)

            # Enable debug for specified loggers
            for logger_name in specified_loggers:
                # Allow shorthand names like "llm" -> "llm_repl.llm"
                if not logger_name.startswith("llm_repl."):
                    logger_name = f"llm_repl.{logger_name}"
                logging.getLogger(logger_name).setLevel(logging.DEBUG)
    elif verbose:
        console_handler.setLevel(logging.INFO)
    else:
        console_handler.setLevel(logging.WARNING)

    root_logger.addHandler(console_handler)


@click.command()
@click.option(
    "--session", "-s", default="default", help="Name of the session to load or create"
)
@click.option(
    "--role",
    "-r",
    default="assistant",
    type=click.Choice(list(ROLE_CONFIGS.keys())),
    help="Role to use for the conversation",
)
@click.option(
    "--agent",
    "-a",
    default=None,
    type=click.Choice(list(AGENT_CONFIGS.keys()) + [None]),
    help="Agent to use (enables agent mode)",
)
@click.option(
    "--model", "-m", default=DEFAULT_MODEL, help="Model to use for the conversation"
)
@click.option("--rag-dir", "-d", default=None, help="Directory to index for RAG")
@click.option(
    "--functions/--no-functions", "-f/-F", default=False, help="Enable function calling"
)
@click.option("--list-sessions", is_flag=True, help="List available sessions and exit")
@click.option("--list-roles", is_flag=True, help="List available roles and exit")
@click.option("--list-agents", is_flag=True, help="List available agents and exit")
@click.option(
    "--verbose", "-v", is_flag=True, help="Enable verbose (INFO level) logging"
)
@click.option(
    "--debug",
    default=None,
    required=False,
    help="Enable debug logging. Specify comma-separated logger names (e.g., --debug=llm,rag) or use --debug-all for all",
)
@click.option("--debug-all", is_flag=True, help="Enable debug logging for all modules")
def main(
    session: str,
    role: str,
    agent: str,
    model: str,
    rag_dir: str,
    functions: bool,
    list_sessions: bool,
    list_roles: bool,
    list_agents: bool,
    verbose: bool,
    debug: Optional[str],
    debug_all: bool,
) -> None:
    """LLM REPL - An AI-based REPL with RAG, function calling, and agent features."""
    console = Console()

    # Set up logging - debug_all takes precedence
    if debug_all:
        setup_logging("", verbose)
    else:
        setup_logging(debug, verbose)

    # Handle list commands
    if list_sessions:
        session_manager = SessionManager()
        sessions = session_manager.list_sessions()
        console.print("[bold]Available Sessions:[/bold]")
        for s in sessions:
            console.print(f"  - {s}")
        if not sessions:
            console.print("  (no sessions)")
        return

    if list_roles:
        console.print("[bold]Available Roles:[/bold]")
        for name, config in ROLE_CONFIGS.items():
            console.print(f"  - [bold]{name}[/bold]: {config.description}")
        return

    if list_agents:
        console.print("[bold]Available Agents:[/bold]")
        for name, config in AGENT_CONFIGS.items():
            console.print(f"  - [bold]{name}[/bold]: {config.description}")
            console.print(f"    Functions: {', '.join(config.functions)}")
        return

    # Check API key
    if not API_KEY:
        console.print(
            "[red]Error: HAXSCRAMPER_LLM_REPL_KEY environment variable not set[/red]"
        )
        console.print("Please set your OpenRouter API key:")
        console.print("  export HAXSCRAMPER_LLM_REPL_KEY=your-api-key")
        sys.exit(1)

    # Initialize components
    try:
        llm_client = LLMClient(model=model)
    except ValueError as e:
        console.print(f"[red]Error: {e}[/red]")
        sys.exit(1)

    session_manager = SessionManager()
    chat_session = session_manager.load_or_create(session, role, agent)

    # Initialize RAG if directory specified
    rag_manager = None
    if rag_dir:
        rag_manager = RAGManager()
        console.print(f"[yellow]Indexing directory: {rag_dir}[/yellow]")
        count = rag_manager.index_path(rag_dir)
        console.print(f"[green]Indexed {count} chunks[/green]")

    # Enable agent mode if specified
    if agent:
        chat_session.agent = agent
        functions = True  # Agents always use functions

    # Create and run REPL
    repl = REPL(
        session=chat_session,
        llm_client=llm_client,
        rag_manager=rag_manager,
        use_functions=functions,
    )

    try:
        repl.run()
    except Exception as e:
        logger.exception("REPL error")
        console.print(f"[red]Error: {e}[/red]")
        sys.exit(1)


if __name__ == "__main__":
    main()
