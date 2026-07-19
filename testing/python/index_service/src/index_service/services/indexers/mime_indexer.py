import ctypes
from pathlib import Path

import magic
from magic import Magic, libmagic

from index_service.services.core.job_cache import cache_indexer_run
from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import IndexDocument, IndexerOutput, IndexerRequest

# from file/magic.h
MAGIC_PARAM_NAME_MAX = 5


def _set_name_max(magic_instance: Magic, value: int) -> None:
    val = ctypes.c_size_t(value)
    libmagic.magic_setparam(magic_instance.cookie, MAGIC_PARAM_NAME_MAX,
                            ctypes.byref(val))


class FileMimeIndexerResult(IndexDocument, extra="forbid"):
    mime_type: str
    mime_encoding: str
    magic_description: str
    magic_all_matches: list[str]
    magic_extensions: list[str]


class FileMimeIndexer(BaseIndexer):
    asset_name = "file_mime"
    result_model = FileMimeIndexerResult

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self._magic_mime = Magic(mime=True)
        self._magic_encoding = Magic(mime_encoding=True)
        self._magic_description = Magic()
        self._magic_all = Magic(keep_going=True)
        self._magic_extensions = Magic(extension=True)

        for instance in (
                self._magic_mime,
                self._magic_encoding,
                self._magic_description,
                self._magic_all,
                self._magic_extensions,
        ):
            _set_name_max(instance, 256)

    @staticmethod
    def _from_file(magic_instance: Magic, path_str: str, default: str = "") -> str:
        try:
            return magic_instance.from_file(path_str)
        except magic.MagicException as e:
            raw = e.args[0] if e.args else b""
            msg = raw.decode("utf-8", "replace") if isinstance(raw, bytes) else str(raw)
            # libmagic can return a valid, fully-formed description and still
            # signal an error (e.g. "name use count (64) exceeded"). Salvage
            # the usable prefix instead of failing the whole indexer.
            cleaned = msg.split(" name use count ")[0].strip()
            return cleaned or default

    @cache_indexer_run
    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        path = ctx.get_path(request.file_ref)
        path_str = str(path)

        mime_type = self._from_file(self._magic_mime, path_str)
        mime_encoding = self._from_file(self._magic_encoding, path_str)
        magic_description = self._from_file(self._magic_description, path_str)

        all_matches_raw = self._from_file(self._magic_all, path_str)
        magic_all_matches = [
            line.strip() for line in all_matches_raw.splitlines() if line.strip()
        ]

        extensions_raw = self._from_file(self._magic_extensions, path_str)
        magic_extensions = [
            ext.strip() for ext in extensions_raw.split("/") if ext.strip()
        ]

        return IndexerOutput(
            indexer_id=self.asset_name,
            result=FileMimeIndexerResult(
                hash=request.get_hash_str(),
                mime_type=mime_type,
                mime_encoding=mime_encoding,
                magic_description=magic_description,
                magic_all_matches=magic_all_matches,
                magic_extensions=magic_extensions,
            ),
        )
