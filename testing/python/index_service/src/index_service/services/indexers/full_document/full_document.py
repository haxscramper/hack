from argparse import ArgumentError
import hashlib
import json
import logging
from pathlib import Path
from typing import Annotated, Literal, Union

import magic
from beartype.typing import ClassVar, Sequence, cast
from pydantic import BaseModel, Field

from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import (
    IndexDocument,
    IndexerOutput,
    IndexerRequest,
    IndexLink,
    MultiDocumentModel,
)
from index_service.services.indexers.full_document.from_pandoc import pandoc_to_document
from index_service.services.indexers.full_document.full_document_types import _flatten

AnyModel = BaseModel

log = logging.getLogger(__name__)


class DocumentBlockIndexerResult(MultiDocumentModel, extra="forbid"):
    pass


class DocumentBlockIndexer(BaseIndexer):
    asset_name = "document_block"
    result_model = DocumentBlockIndexerResult

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self._magic = magic.Magic(mime=True)

    def can_run(self, path: Path) -> bool:
        mime = self._magic.from_file(str(path.resolve()))
        if mime.startswith("text/"):
            return True

        else:
            return False

    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        path = ctx.get_path(request.file_ref)

        root = pandoc_to_document(path)

        documents: list[IndexDocument] = []
        links: list[IndexLink] = []
        _flatten(root, documents, links, parent_hash=None, order=0)

        return IndexerOutput(
            indexer_id=self.asset_name,
            result=MultiDocumentModel(documents=documents, links=links),
        )
