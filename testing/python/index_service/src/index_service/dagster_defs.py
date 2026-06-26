from dataclasses import dataclass
from pathlib import Path

from beartype import beartype
from beartype.typing import List
from dagster import Config, OpExecutionContext, job, op, resource

from index_service.contracts import FileRef
from index_service.db import IndexDatabase
from index_service.orchestrator import (
    ResourceHandle,
    run_rpc_subprocess,
    start_resource,
    stop_resource,
)
from index_service.protocol import ConverterOutput, IndexerOutput, ResourceInfo
from index_service.registry import (
    FILE_REVERSER_MODULE,
    FILE_SIZE_CONVERTER_MODULE,
    FILE_SIZE_MODULE,
    FILE_STATS_MODULE,
    FILE_SUMMARIES_MODULE,
    FLM_GEMMA_RESOURCE_MODULE,
    FULL_TEXT_MODULE,
)
from index_service.runtime import IndexRuntime


class FileConfig(Config):
    md5: str
    paths: List[str]


class ConverterConfig(Config):
    input_files: List[str]
    input_md5s: List[str]
    param: str = ""


@resource(config_schema={
    "host": str,
    "db_name": str,
    "username": str,
    "password": str
})
def arango_resource(context) -> IndexDatabase:
    cfg = context.resource_config
    return IndexDatabase(
        host=cfg["host"],
        db_name=cfg["db_name"],
        username=cfg["username"],
        password=cfg["password"],
    )


@resource(config_schema={"socket_dir": str})
def flm_gemma_resource(context):
    handle: ResourceHandle = start_resource(
        "flm-gemma",
        FLM_GEMMA_RESOURCE_MODULE,
        Path(context.resource_config["socket_dir"]),
        ready_timeout=120.0,
    )
    try:
        yield ResourceInfo(endpoint=handle.endpoint, status="ready")
    finally:
        stop_resource(handle)


@resource(config_schema={"path": str})
def socket_dir_resource(context) -> str:
    return context.resource_config["path"]


@resource(config_schema={"socket_dir": str})
def file_reverser_resource(context):
    handle: ResourceHandle = start_resource(
        "file-reverser",
        FILE_REVERSER_MODULE,
        Path(context.resource_config["socket_dir"]),
    )
    try:
        yield ResourceInfo(endpoint=handle.endpoint, status="ready")
    finally:
        stop_resource(handle)


@resource
def index_runtime_resource(_context) -> IndexRuntime:
    runtime = IndexRuntime()
    try:
        yield runtime
    finally:
        runtime.stop()


@op(required_resource_keys={"arango"})
def provide_file_op(context: OpExecutionContext,
                    config: FileConfig) -> FileRef:
    context.resources.arango.ensure_file(config.md5, config.paths)
    return FileRef(md5=config.md5, paths=config.paths)


@beartype
def _store_indexer_output(
    context: OpExecutionContext,
    file_ref: FileRef,
    out: IndexerOutput,
) -> None:
    context.resources.arango.store_indexer_result(
        file_ref.md5,
        out.indexer_id,
        out.result_type,
        out.result,
    )


@op(required_resource_keys={"arango", "index_runtime"})
def file_size_op(context: OpExecutionContext,
                 file_ref: FileRef) -> IndexerOutput:
    out = context.resources.index_runtime.run_indexers(
        file_ref=file_ref,
        names=["file-size"],
    )["file-size"]
    _store_indexer_output(context, file_ref, out)
    return out


@op(required_resource_keys={"arango", "index_runtime"})
def file_stats_op(context: OpExecutionContext,
                  file_ref: FileRef) -> IndexerOutput:
    out = context.resources.index_runtime.run_indexers(
        file_ref=file_ref,
        names=["file-stats"],
    )["file-stats"]
    _store_indexer_output(context, file_ref, out)
    return out


@op(required_resource_keys={"arango", "index_runtime"})
def full_text_op(context: OpExecutionContext,
                 file_ref: FileRef) -> IndexerOutput:
    out = context.resources.index_runtime.run_indexers(
        file_ref=file_ref,
        names=["full-text"],
    )["full-text"]
    _store_indexer_output(context, file_ref, out)
    return out


@op(required_resource_keys={"arango", "index_runtime"})
def file_summaries_op(
    context: OpExecutionContext,
    file_ref: FileRef,
) -> IndexerOutput:
    out = context.resources.index_runtime.run_indexers(
        file_ref=file_ref,
        names=["file-summaries"],
    )["file-summaries"]
    _store_indexer_output(context, file_ref, out)
    return out


@op(required_resource_keys={"arango", "index_runtime"})
def file_embedding_op(
    context: OpExecutionContext,
    file_ref: FileRef,
) -> IndexerOutput:
    out = context.resources.index_runtime.run_indexers(
        file_ref=file_ref,
        names=["file-embedding"],
    )["file-embedding"]
    _store_indexer_output(context, file_ref, out)
    return out


@op(required_resource_keys={"arango", "index_runtime"})
def file_size_converter_op(
    context: OpExecutionContext,
    config: ConverterConfig,
) -> ConverterOutput:
    out = context.resources.index_runtime.run_converter(
        converter_name="file-size-converter",
        input_files=config.input_files,
        param=config.param,
    )
    context.resources.arango.store_derivation(
        config.input_md5s,
        out.output_files,
        {"param": config.param},
        out.return_value,
    )
    return out


@job(
    resource_defs={
        "arango": arango_resource,
        "socket_dir": socket_dir_resource,
        "reverser": file_reverser_resource,
        "flm_gemma": flm_gemma_resource,
        "index_runtime": index_runtime_resource,
    })
def index_file_job() -> None:
    file_ref = provide_file_op()
    file_size_op(file_ref)
    file_stats_op(file_ref)
    full_text_op(file_ref)
    file_summaries_op(file_ref)
    file_embedding_op(file_ref)


@job(
    resource_defs={
        "arango": arango_resource,
        "socket_dir": socket_dir_resource,
        "index_runtime": index_runtime_resource,
    })
def convert_files_job() -> None:
    file_size_converter_op()
