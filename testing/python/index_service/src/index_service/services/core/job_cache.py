import hashlib
import json
import re
from datetime import datetime, timezone
from functools import wraps
from time import perf_counter
from typing import Any, Callable, ClassVar, Optional, ParamSpec

from sqlalchemy import (
    JSON,
    Column,
    DateTime,
    Float,
    MetaData,
    String,
    Table,
    select,
)
from sqlalchemy.dialects.sqlite import insert as sqlite_insert
from sqlalchemy.engine import Connection
import logging

from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import IndexerOutput, IndexerRequest
from index_service.services.pydantic_utils import model_from_json_data, model_to_json_data
from index_service.services.utils import get_xdg_cache_dir, ExceptionContextNote

log = logging.getLogger(__name__)

P = ParamSpec("P")


def cache_indexer_run(
    decorated: Callable[P, IndexerOutput],) -> Callable[P, IndexerOutput]:

    @wraps(decorated)
    def wrapper(
        self: BaseIndexer,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        file_hash = request.file_ref.hash.hash

        schema_hash = hashlib.sha256(
            json.dumps(
                self.result_model.model_json_schema(),
                sort_keys=True,
                separators=(",", ":"),
            ).encode("utf-8"),).hexdigest()

        def store_cache_record(
            result: IndexerOutput,
            *,
            function_started_at: datetime,
            function_duration_seconds: float,
        ) -> None:
            result_json = model_to_json_data(result)
            assert isinstance(result_json, dict)

            upsert = sqlite_insert(self.cache_table).values(
                file_hash=file_hash,
                schema_hash=schema_hash,
                result=result_json,
                function_started_at=function_started_at,
                function_duration_seconds=function_duration_seconds,
            )

            upsert = upsert.on_conflict_do_update(
                index_elements=[self.cache_table.c.file_hash],
                set_={
                    "schema_hash":
                        upsert.excluded.schema_hash,
                    "result":
                        upsert.excluded.result,
                    "function_started_at":
                        upsert.excluded.function_started_at,
                    "function_duration_seconds":
                        upsert.excluded.function_duration_seconds,
                },
            )

            with (
                    ctx.trace_scope(
                        "store cache database record",
                        indexer=self.asset_name,
                        file_hash=file_hash,
                    ),
                    self.database.begin() as database,
            ):
                database.execute(upsert)

        if self.should_load_cache:
            with (
                    ctx.trace_scope(
                        "load cache database record",
                        indexer=self.asset_name,
                        file_hash=file_hash,
                    ),
                    self.database.connect() as database,
            ):
                cache_row = database.execute(
                    select(
                        self.cache_table.c.schema_hash,
                        self.cache_table.c.result,
                    ).where(self.cache_table.c.file_hash == file_hash,),
                ).mappings().one_or_none()

            if cache_row is not None:
                if cache_row["schema_hash"] == schema_hash:
                    try:
                        parsed = model_from_json_data(
                            cache_row["result"],
                            IndexerOutput,
                        )
                        assert parsed.indexer_id == self.asset_name

                        result_value = self.result_model.model_validate(parsed.result,)

                        return IndexerOutput(
                            indexer_id=self.asset_name,
                            result=result_value,
                        )

                    except (json.JSONDecodeError, ValueError, TypeError) as err:
                        log.error(
                            f"Could not parse cached database value for "
                            f"{self.asset_name}: {err}",)
                else:
                    log.info(
                        "Cache schema mismatch for indexer {} "
                        "(cached={}, current={}), recomputing.".format(
                            self.asset_name,
                            cache_row["schema_hash"],
                            schema_hash,
                        ),)

        function_started_at = datetime.now(timezone.utc)
        execution_started = perf_counter()

        result = decorated(
            self,  # type: ignore[arg-type]
            ctx=ctx,  # type: ignore[arg-type]
            request=request,  # type: ignore[arg-type]
            resources=resources,  # type: ignore[arg-type]
            assets=assets,  # type: ignore[arg-type]
        )

        store_cache_record(
            result,
            function_started_at=function_started_at,
            function_duration_seconds=perf_counter() - execution_started,
        )

        return result

    return wrapper
