# index_service/services/indexers/media_transcription.py
import logging
import os
import subprocess
import tempfile
from pathlib import Path
from typing import cast

import magic
from pydantic import BaseModel, Field

from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.core.types import IndexerOutput, IndexerRequest
from index_service.services.resources.whisper_transcribe import (
    WhisperTranscribeRequest,
    WhisperTranscribeResource,
    WhisperTranscribeResult,
)

log = logging.getLogger(__name__)


class MediaTranscriptionAnalysis(BaseModel, extra="forbid"):
    mime: str | None = None
    ffmpeg_ok: bool = False
    ffmpeg_returncode: int | None = None
    ffmpeg_stderr: str | None = None
    transcribe_ok: bool = False
    transcribe_error: str | None = None


class MediaTranscriptionIndexerResult(BaseModel, extra="forbid"):
    transcript_text: str = ""
    transcript_vtt: str = ""
    transcript: WhisperTranscribeResult | None = None
    analysis: MediaTranscriptionAnalysis = Field(
        default_factory=MediaTranscriptionAnalysis)


class MediaTranscriptionIndexer(BaseIndexer):
    asset_name = "media_transcription"
    result_model = MediaTranscriptionIndexerResult
    required_resources = ("whisper_transcribe",)

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self._magic = magic.Magic(mime=True)

    def can_run(self, path: Path) -> bool:
        if not path.exists():
            return False

        mime = self._magic.from_file(str(path.resolve()))
        return mime.startswith("audio/") or mime.startswith("video/")

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        source_path = ctx.get_path(request.file_ref)
        mime = self._magic.from_file(str(source_path.resolve()))

        analysis = MediaTranscriptionAnalysis(mime=mime)

        fd, wav_tmp = tempfile.mkstemp(suffix=".wav", prefix="index-whisper-")
        os.close(fd)
        wav_path = Path(wav_tmp)

        transcript_result: WhisperTranscribeResult | None = None

        try:
            ffmpeg_result = subprocess.run(
                [
                    "ffmpeg",
                    "-y",
                    "-err_detect",
                    "ignore_err",
                    "-fflags",
                    "+discardcorrupt",
                    "-i",
                    str(source_path),
                    "-map",
                    "0:a:0",
                    "-vn",
                    "-ar",
                    "16000",
                    "-ac",
                    "1",
                    "-c:a",
                    "pcm_s16le",
                    str(wav_path),
                ],
                check=False,
                capture_output=True,
                text=True,
            )

            analysis.ffmpeg_returncode = ffmpeg_result.returncode
            analysis.ffmpeg_ok = ffmpeg_result.returncode == 0
            if not analysis.ffmpeg_ok:
                analysis.ffmpeg_stderr = ffmpeg_result.stderr

            if analysis.ffmpeg_ok:
                whisper = cast(WhisperTranscribeResource, resources["whisper_transcribe"])
                transcript_result = whisper.handle(
                    ctx,
                    WhisperTranscribeRequest(wav_path=str(wav_path)),
                    resources=cast(dict[str, object], resources),
                )

                analysis.transcribe_ok = transcript_result.ok
                if not transcript_result.ok:
                    analysis.transcribe_error = transcript_result.error

        except Exception as exc:
            analysis.transcribe_ok = False
            analysis.transcribe_error = str(exc)
            log.error("media transcription indexer error", exc_info=True)

        finally:
            if wav_path.exists():
                wav_path.unlink()

        result = MediaTranscriptionIndexerResult(
            transcript_text=transcript_result.text
            if transcript_result is not None else "",
            transcript_vtt=transcript_result.vtt if transcript_result is not None else "",
            transcript=transcript_result,
            analysis=analysis,
        )

        return IndexerOutput(indexer_id=self.asset_name, result=result)
