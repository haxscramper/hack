import json
from concurrent.futures import ThreadPoolExecutor

from arango.database import StandardDatabase
from beartype.typing import Any, cast
from beartype import beartype

from arango import DocumentInsertError
from pydantic import TypeAdapter

from index_service.services.core.db_impl.contracts import BaseIndexProtocol
from index_service.services.core.types import (
    AnyModel,
    FileHash,
    FileRef,
    IndexDocument,
    IndexEdge,
    IndexMultiDocument,
    IndexerOutput,
    MultiDocumentModel,
)
from index_service.services.pydantic_utils import (
    model_from_json_data,
    model_to_json_data,
)
from index_service.services.utils import ExceptionContextNote


@beartype
class StorageMixin:
    _db: StandardDatabase

    def get_edge_name(self, indexer) -> str:
        ...

    def _diagnose_document(self, document: dict[str, Any]) -> str:
        try:
            text = json.dumps(document, allow_nan=False, indent=2)
        except Exception as error:
            raise RuntimeError(f"json.dumps failed: {error}") from error

        try:
            json.loads(text)
        except json.JSONDecodeError as error:
            start = max(0, error.pos - 120)
            end = min(len(text), error.pos + 120)
            raise RuntimeError(
                f"Invalid JSON at line {error.lineno}, column {error.colno}, "
                f"pos {error.pos}\n{text[start:end]}") from error

        return text

    def has_indexer_result(
        self,
        ref: FileHash | FileRef,
        indexer: BaseIndexProtocol | str,
    ) -> bool:
        collection_name = self.get_collection_name(indexer)
        file_hash = self.get_file_hash(ref)

        if file_hash in self._indexer_hashes.get(collection_name, set()):
            return True

        if self.only_short_curcuit_checks:
            return False

        exists = cast(bool, self._db.collection(collection_name).has(file_hash))
        if exists:
            self._indexer_hashes.setdefault(collection_name, set()).add(file_hash)

        return exists

    def _store_indexer_document_one(
        self,
        key: str,
        indexer_id: str,
        result: AnyModel,
    ) -> None:
        assert isinstance(result, IndexDocument), (
            "Final documents inserted into the arango collection must be "
            f"derived from IndexDocument, but got {type(result)}")

        document = {
            "_key": key,
            "indexer_id": indexer_id,
            "result": model_to_json_data(result),
        }
        collection = self._db.collection(indexer_id)

        with ExceptionContextNote(lambda: self._diagnose_document(document)):
            try:
                if collection.has(key):
                    collection.replace(document)
                else:
                    collection.insert(document)
            except DocumentInsertError as error:
                raise error from None

        self._indexer_hashes.setdefault(indexer_id, set()).add(key)

    def _store_indexer_edge_one(
        self,
        indexer_id: str,
        result: AnyModel,
    ) -> None:
        assert isinstance(
            result,
            IndexEdge), ("Final edges inserted into the arango collection must be "
                         f"derived from IndexEdge, but got {type(result)}")

        from_id = f"{indexer_id}/{result.from_}"
        to_id = f"{indexer_id}/{result.to_}"
        document = {
            "_from": from_id,
            "_to": to_id,
            "indexer_id": indexer_id,
            "result": model_to_json_data(result),
        }

        query = f"""
        FOR edge IN {self.get_edge_name(indexer_id)}
            FILTER edge._from == @_from
            FILTER edge._to == @_to
            LIMIT 1
            RETURN edge._key
        """

        collection = self._db.collection(self.get_edge_name(indexer_id))
        with ExceptionContextNote(lambda: self._diagnose_document(document)):
            try:
                cursor = self._db.aql.execute(
                    query,
                    bind_vars={
                        "_from": from_id,
                        "_to": to_id
                    },
                )
                existing_key = next(cursor, None)

                if existing_key is not None:
                    document["_key"] = existing_key
                    collection.replace(document)
                else:
                    collection.insert(document)
            except DocumentInsertError as error:
                raise error from None

    def store_indexer_output(
        self,
        ref: FileRef,
        out: IndexerOutput,
    ) -> None:
        if isinstance(out.result, MultiDocumentModel):
            for document in out.result.documents:
                self._store_indexer_document_one(
                    key=document.hash,
                    indexer_id=out.indexer_id,
                    result=document,
                )

            for edge in out.result.edges:
                self._store_indexer_edge_one(out.indexer_id, edge)
            return

        self._store_indexer_document_one(
            key=ref.hash.hash,
            indexer_id=out.indexer_id,
            result=out.result,
        )

    def validate_indexer_result_document(
        self,
        result: dict[str, Any],
        indexer: BaseIndexProtocol,
    ) -> Any:
        model_type = (indexer.result_model.document_type if issubclass(
            indexer.result_model, MultiDocumentModel) else indexer.result_model)
        return TypeAdapter(model_type).validate_python(
            model_from_json_data(result, model_type))

    def get_indexer_one_document(
        self,
        document_id: str,
        indexer: BaseIndexProtocol,
    ) -> Any:
        query = f"""
        FOR doc IN {indexer.asset_name}
            FILTER doc._key == @hash
            RETURN doc.result
        """

        for document in self._db.aql.execute(  # type: ignore
                query,
                bind_vars={"hash": document_id},
        ):
            return self.validate_indexer_result_document(document, indexer)

        raise KeyError(f"Could not find document with ID {document_id} in the indexer "
                       f"collection {indexer.asset_name}")

    def get_indexer_result(
        self,
        hash: FileHash,
        indexer: BaseIndexProtocol,
    ) -> Any | None:
        return self.get_indexer_result_batch(
            [hash],
            indexer,
            max_workers=1,
        )[0]

    def get_indexer_result_batch(
        self,
        hashes: list[FileHash],
        indexer: BaseIndexProtocol,
        *,
        max_workers: int = 1,
    ) -> list[Any | None]:
        """
        Return the list of the indexer result models for the given list of hashes
        in the same order as the file hashes were used.
        """
        if not hashes:
            return []

        unique_hashes = list(dict.fromkeys(hash.hash for hash in hashes))
        max_workers = min(max_workers, len(unique_hashes))

        if max_workers == 1:
            results = self._get_indexer_result_batch_chunk(
                unique_hashes,
                indexer,
            )
        else:
            chunk_size = (len(unique_hashes) + max_workers - 1) // max_workers
            chunks = [
                unique_hashes[index:index + chunk_size]
                for index in range(0, len(unique_hashes), chunk_size)
            ]

            with ThreadPoolExecutor(max_workers=max_workers) as executor:
                chunk_results = executor.map(
                    lambda chunk: self._get_indexer_result_batch_chunk(
                        chunk,
                        indexer,
                    ),
                    chunks,
                )

            results = {
                file_hash: result for chunk in chunk_results
                for file_hash, result in chunk.items()
            }

        return [results.get(hash.hash, None) for hash in hashes]

    def _get_indexer_result_batch_chunk(
        self,
        hashes: list[str],
        indexer: BaseIndexProtocol,
    ) -> dict[str, Any]:
        model_type = indexer.result_model
        indexer_id = indexer.asset_name

        if not issubclass(model_type, MultiDocumentModel):
            results: dict[str, Any] = {}

            for document in self._db.aql.execute(  # type: ignore
                    f"""
                FOR doc IN {indexer_id}
                    FILTER doc._key IN @hashes
                    RETURN {{
                        file_hash: doc._key,
                        result: doc.result,
                    }}
                """,
                    bind_vars={"hashes": hashes},
            ):
                results[document["file_hash"]] = model_type.model_validate(
                    model_from_json_data(document["result"], model_type),)

            return results

        assert issubclass(model_type.edge_type, IndexEdge), str(model_type.edge_type)

        edge_adapter = TypeAdapter(model_type.edge_type)
        documents_by_hash: dict[str, list[IndexMultiDocument]] = {
            hash: [] for hash in hashes
        }
        edges_by_hash: dict[str, list[IndexEdge]] = {hash: [] for hash in hashes}

        for document in self._db.aql.execute(  # type: ignore
                f"""
            FOR doc IN {indexer_id}
                FILTER doc.result.file_hash IN @hashes
                RETURN doc.result
            """,
                bind_vars={"hashes": hashes},
        ):
            validated = self.validate_indexer_result_document(document, indexer)
            documents_by_hash[document["file_hash"]].append(validated)

        for edge in self._db.aql.execute(  # type: ignore
                f"""
            FOR edge IN {self.get_edge_name(indexer_id)}
                FILTER edge.result.file_hash IN @hashes
                RETURN edge.result
            """,
                bind_vars={"hashes": hashes},
        ):
            file_hash = edge["file_hash"]
            edges_by_hash[file_hash].append(
                edge_adapter.validate_python(
                    model_from_json_data(edge, model_type.edge_type),),)

        return {
            hash:
                model_type(
                    documents=documents_by_hash[hash],
                    edges=edges_by_hash[hash],
                ) for hash in hashes
        }

    def store_derivation(
        self,
        input: list[FileRef],
        output_files: list[str],
        config: dict[str, object],
        return_value: AnyModel,
    ) -> str:
        metadata = self._db.collection("derivations").insert({
            "input_hashes": [file.hash.hash for file in input],
            "output_files": output_files,
            "config": config,
            "return_value": return_value.model_dump(),
        })
        return metadata["_key"]
