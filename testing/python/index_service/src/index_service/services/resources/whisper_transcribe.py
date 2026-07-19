# index_service/services/resources/whisper_transcribe.py
import atexit
import logging
import os
import subprocess
import threading
import time
from pathlib import Path
from typing import Any

from beartype.typing import ClassVar
import httpx
from pydantic import BaseModel, Field

from index_service.services.core.job_types import BaseResource, BaseResourceConfig, RunContext

log = logging.getLogger(__name__)


class WhisperSegment(BaseModel, extra="forbid"):
    index: int
    start_sec: float
    end_sec: float
    text: str


class WhisperTranscribeRequest(BaseModel, extra="forbid"):
    wav_path: str
    language: str | None = None
    prompt: str | None = None
    temperature: float | None = None


class WhisperTranscribeResult(BaseModel, extra="forbid"):
    ok: bool
    model: str
    text: str
    language: str | None = None
    duration_sec: float | None = None
    segments: list[WhisperSegment] = Field(default_factory=list)
    vtt: str = ""
    error: str | None = None


class WhisperServerConfig(BaseResourceConfig, extra="forbid"):
    base_url: str = Field(default="http://127.0.0.1:8178")
    host: str = Field(default="127.0.0.1")
    port: int = Field(default=8178)
    server_bin: str
    whisper_model_directory: str = Field(...)
    model_name: str = Field(default="ggml-medium.en.bin")
    startup_timeout_sec: float = Field(default=20.0)
    request_timeout_sec: float = Field(default=240.0)
    serve_cmd: list[str] | None = None


class WhisperTranscribeResource(BaseResource):
    resource_key = "whisper_transcribe"
    config_model: ClassVar[type] = WhisperServerConfig

    config: WhisperServerConfig

    def __init__(self, config: WhisperServerConfig) -> None:
        super().__init__(config=config)
        self._model_dir = Path(self.config.whisper_model_directory).expanduser().resolve()
        self._model_file_name, self._download_model_name = self._resolve_model_names(
            self.config.model_name)
        self._model_path = self._model_dir / self._model_file_name
        self._ggml_download_script = self._model_dir / "download-ggml-model.sh"
        self._base_url = self.config.base_url.rstrip("/")
        self._health_url = f"{self._base_url}/health"
        self._transcriptions_url = f"{self._base_url}/v1/audio/transcriptions"

        self._serve_cmd = self.config.serve_cmd or [
            self.config.server_bin,
            "-m",
            str(self._model_path),
            "--host",
            self.config.host,
            "--port",
            str(self.config.port),
        ]

        self._proc: subprocess.Popen[bytes] | None = None
        self._lock = threading.Lock()
        self._stdout_log_path = "/tmp/hax-index-whisper-server-stdout.log"
        self._stderr_log_path = "/tmp/hax-index-whisper-server-stderr.log"
        self._stdout_log_file = None
        self._stderr_log_file = None

        atexit.register(self.close)
        self._ensure_server_running()

    @staticmethod
    def _resolve_model_names(model_name: str) -> tuple[str, str]:
        normalized = model_name.strip()
        if normalized.startswith("ggml-"):
            normalized = normalized[len("ggml-"):]
        if normalized.endswith(".bin"):
            normalized = normalized[:-len(".bin")]

        if not normalized:
            raise ValueError("model_name must not be empty")

        model_file_name = f"ggml-{normalized}.bin"
        return model_file_name, normalized

    def _ensure_model_downloaded(self) -> None:
        self._model_dir.mkdir(parents=True, exist_ok=True)

        if self._model_path.exists():
            return

        if not self._ggml_download_script.exists():
            raise FileNotFoundError(
                f"missing GGML download script: {self._ggml_download_script}")

        log.info(f"Input transcription model is missing {self._model_path}")
        result = subprocess.run(
            ["sh", str(self._ggml_download_script), self._download_model_name],
            cwd=str(self._model_dir),
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            raise RuntimeError(
                "failed to download whisper model "
                f"'{self._download_model_name}' via {self._ggml_download_script}\n"
                f"stdout:\n{result.stdout}\n"
                f"stderr:\n{result.stderr}")

        if not self._model_path.exists():
            raise FileNotFoundError(
                f"model download finished but file was not found: {self._model_path}")

        log.info("Model download OK")

    def _is_server_healthy(self) -> bool:
        if self._proc is not None and self._proc.poll() is not None:
            return False

        try:
            response = httpx.get(self._health_url, timeout=2.0)
            if response.status_code == 200:
                return True
        except Exception:
            pass

        try:
            response = httpx.get(f"{self._base_url}/v1/models", timeout=2.0)
            return response.status_code == 200
        except Exception:
            return False

    def _start_server_locked(self) -> None:
        self._ensure_model_downloaded()
        self._stdout_log_file = open(self._stdout_log_path, "ab")
        self._stderr_log_file = open(self._stderr_log_path, "ab")

        try:
            self._proc = subprocess.Popen(
                self._serve_cmd,
                stdout=self._stdout_log_file,
                stderr=self._stderr_log_file,
            )
        except Exception:
            self._stdout_log_file.close()
            self._stderr_log_file.close()
            self._stdout_log_file = None
            self._stderr_log_file = None
            raise

        deadline = time.monotonic() + self.config.startup_timeout_sec
        while time.monotonic() < deadline:
            if self._is_server_healthy():
                return
            if self._proc.poll() is not None:
                break
            time.sleep(0.2)

        self._stop_server_locked()
        raise RuntimeError("failed to start whisper server and reach healthy state")

    def _stop_server_locked(self) -> None:
        if self._proc is not None:
            if self._proc.poll() is None:
                self._proc.terminate()
                try:
                    self._proc.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    self._proc.kill()
                    self._proc.wait(timeout=3)
            self._proc = None

        if self._stdout_log_file is not None:
            self._stdout_log_file.close()
            self._stdout_log_file = None

        if self._stderr_log_file is not None:
            self._stderr_log_file.close()
            self._stderr_log_file = None

    def _restart_server_locked(self) -> None:
        self._stop_server_locked()
        self._start_server_locked()

    def _ensure_server_running(self) -> None:
        with self._lock:
            if self._is_server_healthy():
                return
            self._restart_server_locked()

    @staticmethod
    def _format_vtt_timestamp(seconds: float) -> str:
        total_ms = int(round(seconds * 1000.0))
        hours, rem_ms = divmod(total_ms, 3_600_000)
        minutes, rem_ms = divmod(rem_ms, 60_000)
        secs, ms = divmod(rem_ms, 1000)
        return f"{hours:02d}:{minutes:02d}:{secs:02d}.{ms:03d}"

    def _build_vtt(self, segments: list[WhisperSegment]) -> str:
        lines = ["WEBVTT", ""]
        for segment in segments:
            lines.append(str(segment.index))
            lines.append(
                f"{self._format_vtt_timestamp(segment.start_sec)} --> {self._format_vtt_timestamp(segment.end_sec)}"
            )
            lines.append(segment.text)
            lines.append("")
        return "\n".join(lines).rstrip() + "\n"

    def _transcribe_once(self,
                         request: WhisperTranscribeRequest) -> WhisperTranscribeResult:
        wav_path = Path(request.wav_path)
        if not wav_path.exists():
            return WhisperTranscribeResult(
                ok=False,
                model=self.config.model_name,
                text="",
                error=f"wav file does not exist: {wav_path}",
            )

        form_data: dict[str, str] = {
            "model": self.config.model_name,
            "response_format": "verbose_json",
        }

        if request.language is not None:
            form_data["language"] = request.language

        if request.prompt is not None:
            form_data["prompt"] = request.prompt

        if request.temperature is not None:
            form_data["temperature"] = str(request.temperature)

        with wav_path.open("rb") as wav_file:
            response = httpx.post(
                self._transcriptions_url,
                data=form_data,
                files={"file": (wav_path.name, wav_file, "audio/wav")},
                timeout=self.config.request_timeout_sec,
            )

        response.raise_for_status()
        payload = response.json()

        raw_segments = payload.get("segments") or []
        segments: list[WhisperSegment] = []
        for index, raw_segment in enumerate(raw_segments, start=1):
            segment = WhisperSegment(
                index=index,
                start_sec=float(raw_segment.get("start", 0.0)),
                end_sec=float(raw_segment.get("end", 0.0)),
                text=str(raw_segment.get("text", "")).strip(),
            )
            segments.append(segment)

        text = str(payload.get("text", "")).strip()
        if not text:
            text = " ".join(seg.text for seg in segments if seg.text).strip()

        duration_sec = None
        if segments:
            duration_sec = max(seg.end_sec for seg in segments)

        return WhisperTranscribeResult(
            ok=True,
            model=str(payload.get("model", self.config.model_name)),
            text=text,
            language=payload.get("language"),
            duration_sec=duration_sec,
            segments=segments,
            vtt=self._build_vtt(segments),
        )

    def close(self) -> None:
        with self._lock:
            self._stop_server_locked()

    def handle(
        self,
        ctx: RunContext,
        request: WhisperTranscribeRequest,
        resources: dict[str, BaseResource],
    ) -> WhisperTranscribeResult:
        self._ensure_server_running()

        try:
            return self._transcribe_once(request)
        except Exception:
            log.error("whisper transcription error", exc_info=True)
            with self._lock:
                self._restart_server_locked()
            return self._transcribe_once(request)
