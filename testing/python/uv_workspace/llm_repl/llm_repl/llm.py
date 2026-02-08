"""LLM client for OpenRouter API using the official SDK."""

import json
import logging
from beartype.typing import Optional, Generator, Any

from openrouter import OpenRouter

from .config import API_KEY, DEFAULT_MODEL

logger = logging.getLogger(__name__)


class LLMClient:
    """Client for interacting with the LLM via OpenRouter SDK."""

    def __init__(self,
                 api_key: Optional[str] = None,
                 model: str = DEFAULT_MODEL):
        self.api_key = api_key or API_KEY
        if not self.api_key:
            raise ValueError(
                "API key not provided. Set HAXSCRAMPER_LLM_REPL_KEY environment variable."
            )

        self.model = model
        logger.info(f"Initialized LLM client with model: {model}")

    def _get_client(self) -> OpenRouter:
        """Create a new OpenRouter client instance."""
        logger.debug("Creating new OpenRouter client instance")
        return OpenRouter(api_key=self.api_key)

    def chat(
        self,
        messages: list[dict],
        system_prompt: Optional[str] = None,
        tools: Optional[list[dict]] = None,
        stream: bool = True,
    ) -> Generator[dict, None, None] | dict:
        """Send a chat request to the LLM."""
        # Prepare messages with optional system prompt
        api_messages = []
        if system_prompt:
            api_messages.append({"role": "system", "content": system_prompt})
        api_messages.extend(messages)

        logger.debug(
            f"Chat request: model={self.model}, messages={len(api_messages)}, stream={stream}, tools={len(tools) if tools else 0}"
        )

        # Log message summaries
        for i, msg in enumerate(api_messages):
            content_preview = msg.get("content", "")[:100]
            logger.debug(
                f"  Message {i}: role={msg.get('role')}, content={content_preview}..."
            )

        if stream:
            return self._stream_response(api_messages, tools)
        else:
            return self._single_response(api_messages, tools)

    def _stream_response(
            self,
            messages: list[dict],
            tools: Optional[list[dict]] = None) -> Generator[dict, None, None]:
        """Stream the response from the LLM."""
        logger.debug(f"Starting streaming request to OpenRouter")
        try:
            with self._get_client() as client:
                # Build kwargs for the SDK call
                kwargs: dict[str, Any] = {
                    "messages": messages,
                    "model": self.model,
                    "stream": True,
                }

                if tools:
                    kwargs["tools"] = tools
                    kwargs["tool_choice"] = "auto"
                    logger.debug(
                        f"Tools enabled: {[t['function']['name'] for t in tools]}"
                    )

                logger.debug(f"Sending chat.send() with stream=True")
                response = client.chat.send(**kwargs)

                current_content = ""
                current_tool_calls: list[dict] = []
                chunk_count = 0

                with response as event_stream:
                    for event in event_stream:
                        chunk_count += 1
                        # Handle different event types from the SDK
                        if hasattr(event, "choices") and event.choices:
                            choice = event.choices[0]
                            delta = getattr(choice, "delta", None)

                            if delta is None:
                                continue

                            # Handle content
                            content = getattr(delta, "content", None)
                            if content:
                                current_content += content
                                yield {"type": "content", "content": content}

                            # Handle tool calls
                            tool_calls = getattr(delta, "tool_calls", None)
                            if tool_calls:
                                for tool_call in tool_calls:
                                    idx = getattr(tool_call, "index", 0)

                                    # Extend list if needed
                                    while len(current_tool_calls) <= idx:
                                        current_tool_calls.append({
                                            "id": "",
                                            "type": "function",
                                            "function": {
                                                "name": "",
                                                "arguments": "",
                                            },
                                        })

                                    tc_id = getattr(tool_call, "id", None)
                                    if tc_id:
                                        current_tool_calls[idx]["id"] = tc_id

                                    func = getattr(tool_call, "function", None)
                                    if func:
                                        name = getattr(func, "name", None)
                                        if name:
                                            current_tool_calls[idx][
                                                "function"]["name"] = name
                                        args = getattr(func, "arguments", None)
                                        if args:
                                            current_tool_calls[idx][
                                                "function"][
                                                    "arguments"] += args

                            # Check for finish reason
                            finish_reason = getattr(choice, "finish_reason",
                                                    None)
                            if finish_reason:
                                logger.debug(
                                    f"Stream finished: chunks={chunk_count}, content_len={len(current_content)}, tool_calls={len(current_tool_calls)}, reason={finish_reason}"
                                )
                                if current_tool_calls:
                                    logger.debug(
                                        f"Tool calls received: {json.dumps(current_tool_calls)}"
                                    )
                                yield {
                                    "type":
                                    "finish",
                                    "content":
                                    current_content,
                                    "tool_calls":
                                    current_tool_calls
                                    if current_tool_calls else None,
                                    "finish_reason":
                                    finish_reason,
                                }
        except Exception as e:
            logger.error(f"Error in stream response: {e}", exc_info=True)
            yield {"type": "error", "error": str(e)}

    def _single_response(self,
                         messages: list[dict],
                         tools: Optional[list[dict]] = None) -> dict:
        """Get a single response from the LLM."""
        logger.debug(f"Starting non-streaming request to OpenRouter")
        try:
            with self._get_client() as client:
                # Build kwargs for the SDK call
                kwargs: dict[str, Any] = {
                    "messages": messages,
                    "model": self.model,
                    "stream": False,
                }

                if tools:
                    kwargs["tools"] = tools
                    kwargs["tool_choice"] = "auto"
                    logger.debug(
                        f"Tools enabled: {[t['function']['name'] for t in tools]}"
                    )

                logger.debug(f"Sending chat.send() with stream=False")
                response = client.chat.send(**kwargs)

                # Handle non-streaming response
                if hasattr(response, "choices") and response.choices:
                    message = response.choices[0].message
                    content = getattr(message, "content", "") or ""

                    tool_calls = None
                    if hasattr(message, "tool_calls") and message.tool_calls:
                        tool_calls = [{
                            "id": tc.id,
                            "type": "function",
                            "function": {
                                "name": tc.function.name,
                                "arguments": tc.function.arguments,
                            },
                        } for tc in message.tool_calls]

                    finish_reason = getattr(response.choices[0],
                                            "finish_reason", None)
                    logger.debug(
                        f"Response received: content_len={len(content)}, tool_calls={len(tool_calls) if tool_calls else 0}, reason={finish_reason}"
                    )
                    if tool_calls:
                        logger.debug(
                            f"Tool calls received: {json.dumps(tool_calls)}")

                    return {
                        "type": "finish",
                        "content": content,
                        "tool_calls": tool_calls,
                        "finish_reason": finish_reason,
                    }

                logger.warning("No choices in response")
                return {"type": "error", "error": "No choices in response"}
        except Exception as e:
            logger.error(f"Error in single response: {e}", exc_info=True)
            return {"type": "error", "error": str(e)}
