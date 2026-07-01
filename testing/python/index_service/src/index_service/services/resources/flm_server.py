# index_service/services/resources/flm_server.py
import atexit
import os
import subprocess
import threading
import time
from typing import Any

from openai import OpenAI
from pydantic import BaseModel, Field

from index_service.services.core.job_types import BaseResource, RunContext


class FlmMessage(BaseModel, extra="forbid"):
    role: str
    content: str


class FlmRequest(BaseModel, extra="forbid"):
    model: str
    messages: list[FlmMessage]
    temperature: float | None = None
    max_tokens: int | None = None


class FlmResponse(BaseModel, extra="forbid"):
    model: str
    content: str
    finish_reason: str | None = None
    usage: dict[str, Any] | None = None


class FlmServerResource(BaseResource):
    resource_key = "flm_server"

    def __init__(
        self,
        base_url: str | None = None,
        api_key: str = "flm",
        host: str | None = None,
        port: int | None = None,
        serve_cmd: list[str] | None = None,
        startup_timeout_sec: float = 20.0,
    ) -> None:
        env_base_url = os.environ.get("FLM_BASE_URL",
                                      "http://127.0.0.1:52625/v1")
        self._base_url = base_url or env_base_url
        self._api_key = api_key
        self._host = host or os.environ.get("FLM_HOST", "127.0.0.1")
        self._port = port or int(os.environ.get("FLM_PORT", "52625"))
        self._startup_timeout_sec = startup_timeout_sec

        self._serve_cmd = serve_cmd or [
            "flm",
            "serve",
            "--host",
            self._host,
            "--port",
            str(self._port),
        ]

        self._client = OpenAI(base_url=self._base_url, api_key=self._api_key)
        self._proc: subprocess.Popen[bytes] | None = None
        self._lock = threading.Lock()

        atexit.register(self.close)
        self._ensure_server_running()

    def _is_server_healthy(self) -> bool:
        if self._proc is not None and self._proc.poll() is not None:
            return False
        try:
            self._client.models.list()
            return True
        except Exception:
            return False

    def _start_server_locked(self) -> None:
        self._proc = subprocess.Popen(
            self._serve_cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )

        deadline = time.monotonic() + self._startup_timeout_sec
        while time.monotonic() < deadline:
            if self._is_server_healthy():
                return
            if self._proc.poll() is not None:
                break
            time.sleep(0.2)

        self._stop_server_locked()
        raise RuntimeError(
            "Failed to start `flm serve` and reach healthy state.")

    def _stop_server_locked(self) -> None:
        if self._proc is None:
            return

        if self._proc.poll() is None:
            self._proc.terminate()
            try:
                self._proc.wait(timeout=3)
            except subprocess.TimeoutExpired:
                self._proc.kill()
                self._proc.wait(timeout=3)

        self._proc = None

    def _restart_server_locked(self) -> None:
        self._stop_server_locked()
        self._start_server_locked()

    def _ensure_server_running(self) -> None:
        with self._lock:
            if self._is_server_healthy():
                return
            self._restart_server_locked()

    def close(self) -> None:
        with self._lock:
            self._stop_server_locked()

    def handle(
        self,
        ctx: RunContext,
        request: FlmRequest,
        resources: dict[str, BaseResource],
    ) -> FlmResponse:
        self._ensure_server_running()

        try:
            completion = self._client.chat.completions.create(
                model=request.model,
                messages=[msg.model_dump() for msg in request.messages],
                temperature=request.temperature,
                max_tokens=request.max_tokens,
            )
        except Exception:
            with self._lock:
                self._restart_server_locked()

            completion = self._client.chat.completions.create(
                model=request.model,
                messages=[msg.model_dump() for msg in request.messages],
                temperature=request.temperature,
                max_tokens=request.max_tokens,
            )

        content = completion.choices[0].message.content or ""
        usage = completion.usage.model_dump(
        ) if completion.usage is not None else None

        return FlmResponse(
            model=completion.model,
            content=content,
            finish_reason=completion.choices[0].finish_reason,
            usage=usage,
        )
