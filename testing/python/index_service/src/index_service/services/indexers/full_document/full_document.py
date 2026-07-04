from argparse import ArgumentError
import hashlib
import json
import logging
from pathlib import Path
from typing import Annotated, Literal, Union

from index_service.services.utils import ExceptionContextNote
import magic
from beartype.typing import ClassVar, Sequence, cast, Annotated, Union, Any
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

import index_service.services.indexers.full_document.full_document_types as doc_types

log = logging.getLogger(__name__)


class DocumentBlockIndexerResult(MultiDocumentModel, extra="forbid"):
    document_type: ClassVar[Any] = Annotated[
        Union[
            doc_types.Document,
            doc_types.Paragraph,
            doc_types.Math,
            doc_types.Heading,
            doc_types.Code,
            doc_types.BulletListItem,
            doc_types.NumberedListItem,
            doc_types.Quote,
            doc_types.Div,
            doc_types.Table,
            doc_types.RawBlock,
            doc_types.TableRow,
            doc_types.TableCell,
            doc_types.File,
        ],
        Field(discriminator="type"),
    ]

    link_type: ClassVar[type] = doc_types.DocumentLink


class DocumentBlockIndexer(BaseIndexer):
    asset_name = "document_block"
    result_model = DocumentBlockIndexerResult
    max_parallel = 8

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self._magic = magic.Magic(mime=True)

    def can_run(self, path: Path) -> bool:
        mime = self._magic.from_file(str(path.resolve()))
        if mime.startswith("text/") and not mime.startswith("text/x-script"):
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

        mime = self._magic.from_file(str(path.resolve()))
        log.info(f"Converting {path} to full document, using mime {mime}")
        root = pandoc_to_document(path, file_hash=request.get_hash_str())

        documents: list[IndexDocument] = []
        links: list[IndexLink] = []

        file = doc_types.File(
            hash=request.file_ref.hash.hash,
            file_hash=request.get_hash_str(),
        )

        documents.append(file)
        _flatten(root, documents, links, parent_hash=file.hash, order=0)

        return IndexerOutput(
            indexer_id=self.asset_name,
            result=DocumentBlockIndexerResult(
                documents=documents,
                links=links,
            ),
        )
