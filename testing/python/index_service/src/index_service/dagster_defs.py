from dataclasses import dataclass
from pathlib import Path

from beartype import beartype
from beartype.typing import List
from dagster import Config, OpExecutionContext, job, op, resource

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


@beartype
@dataclass
class FileRef:
    """A single file to be processed by the indexing ops."""

    md5: str
    """Content identity of the file."""

    paths: List[str]
    """Last known absolute paths of the file."""


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


@op(required_resource_keys={"arango", "socket_dir", "flm_gemma"})
def file_summaries_op(context: OpExecutionContext,
                      file_ref: FileRef) -> IndexerOutput:
    socket_dir = Path(context.resources.socket_dir)
    flm_gemma: ResourceInfo = context.resources.flm_gemma
    init = {
        "md5": file_ref.md5,
        "paths": file_ref.paths,
        "dependencies": {},
        "available_resources": {
            "flm-gemma": {
                "endpoint": flm_gemma.endpoint,
                "status": flm_gemma.status,
            }
        },
        "config": {},
    }
    res = run_rpc_subprocess(FILE_SUMMARIES_MODULE, init, socket_dir, 240.0)
    out = IndexerOutput.from_dict(res.result)
    context.resources.arango.store_indexer_result(
        file_ref.md5,
        out.indexer_id,
        out.result_type,
        out.result,
    )
    return out


@op(required_resource_keys={"arango"})
def provide_file_op(context: OpExecutionContext,
                    config: FileConfig) -> FileRef:
    context.resources.arango.ensure_file(config.md5, config.paths)
    return FileRef(md5=config.md5, paths=config.paths)


@op(required_resource_keys={"arango", "socket_dir"})
def file_size_op(context: OpExecutionContext,
                 file_ref: FileRef) -> IndexerOutput:
    socket_dir = Path(context.resources.socket_dir)
    init = {
        "md5": file_ref.md5,
        "paths": file_ref.paths,
        "dependencies": {},
        "available_resources": {},
        "config": {},
    }
    res = run_rpc_subprocess(FILE_SIZE_MODULE, init, socket_dir, 30.0)
    out = IndexerOutput.from_dict(res.result)
    context.resources.arango.store_indexer_result(file_ref.md5, out.indexer_id,
                                                  out.result_type, out.result)
    return out


@op(required_resource_keys={"arango", "socket_dir"})
def file_stats_op(context: OpExecutionContext,
                  file_ref: FileRef) -> IndexerOutput:
    socket_dir = Path(context.resources.socket_dir)
    init = {
        "md5": file_ref.md5,
        "paths": file_ref.paths,
        "dependencies": {},
        "available_resources": {},
        "config": {},
    }
    res = run_rpc_subprocess(FILE_STATS_MODULE, init, socket_dir, 30.0)
    out = IndexerOutput.from_dict(res.result)
    context.resources.arango.store_indexer_result(file_ref.md5, out.indexer_id,
                                                  out.result_type, out.result)
    return out


@op(required_resource_keys={"arango", "socket_dir", "reverser"})
def full_text_op(context: OpExecutionContext,
                 file_ref: FileRef) -> IndexerOutput:
    socket_dir = Path(context.resources.socket_dir)
    reverser: ResourceInfo = context.resources.reverser
    init = {
        "md5": file_ref.md5,
        "paths": file_ref.paths,
        "dependencies": {},
        "available_resources": {
            "file-reverser": {
                "endpoint": reverser.endpoint,
                "status": reverser.status
            }
        },
        "config": {},
    }
    res = run_rpc_subprocess(FULL_TEXT_MODULE, init, socket_dir, 30.0)
    out = IndexerOutput.from_dict(res.result)
    context.resources.arango.store_indexer_result(file_ref.md5, out.indexer_id,
                                                  out.result_type, out.result)
    return out


@op(required_resource_keys={"arango", "socket_dir"})
def file_size_converter_op(context: OpExecutionContext,
                           config: ConverterConfig) -> ConverterOutput:
    socket_dir = Path(context.resources.socket_dir)
    init = {
        "input_files": config.input_files,
        "config": {
            "param": config.param
        }
    }
    res = run_rpc_subprocess(FILE_SIZE_CONVERTER_MODULE, init, socket_dir,
                             30.0)
    out = ConverterOutput.from_dict(res.result)
    context.resources.arango.store_derivation(config.input_md5s,
                                              out.output_files,
                                              {"param": config.param},
                                              out.return_value)
    return out


@job(
    resource_defs={
        "arango": arango_resource,
        "socket_dir": socket_dir_resource,
        "reverser": file_reverser_resource,
        "flm_gemma": flm_gemma_resource,
    })
def index_file_job() -> None:
    file_ref = provide_file_op()
    file_size_op(file_ref)
    file_stats_op(file_ref)
    full_text_op(file_ref)
    file_summaries_op(file_ref)


@job(resource_defs={
    "arango": arango_resource,
    "socket_dir": socket_dir_resource,
})
def convert_files_job() -> None:
    file_size_converter_op()
