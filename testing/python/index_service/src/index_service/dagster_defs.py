from dagster import Config, OpExecutionContext, job, op, resource

from index_service.db import IndexDatabase
from index_service.protocol import ConverterOutput, FileRef, IndexerOutput
from index_service.runtime import IndexRuntime


class FileConfig(Config):
    md5: str
    paths: list[str]


class ConverterConfig(Config):
    input_files: list[str]
    input_md5s: list[str]
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
    out = context.resources.index_runtime.run_indexers(file_ref=file_ref,
                                                       names=["file-size"
                                                              ])["file-size"]
    _store_indexer_output(context, file_ref, out)
    return out


@op(required_resource_keys={"arango", "index_runtime"})
def file_stats_op(context: OpExecutionContext,
                  file_ref: FileRef) -> IndexerOutput:
    out = context.resources.index_runtime.run_indexers(file_ref=file_ref,
                                                       names=["file-stats"
                                                              ])["file-stats"]
    _store_indexer_output(context, file_ref, out)
    return out


@op(required_resource_keys={"arango", "index_runtime"})
def full_text_op(context: OpExecutionContext,
                 file_ref: FileRef) -> IndexerOutput:
    out = context.resources.index_runtime.run_indexers(file_ref=file_ref,
                                                       names=["full-text"
                                                              ])["full-text"]
    _store_indexer_output(context, file_ref, out)
    return out


@op(required_resource_keys={"arango", "index_runtime"})
def file_summaries_op(context: OpExecutionContext,
                      file_ref: FileRef) -> IndexerOutput:
    out = context.resources.index_runtime.run_indexers(
        file_ref=file_ref, names=["file-summaries"])["file-summaries"]
    _store_indexer_output(context, file_ref, out)
    return out


@op(required_resource_keys={"arango", "index_runtime"})
def file_embedding_op(context: OpExecutionContext,
                      file_ref: FileRef) -> IndexerOutput:
    out = context.resources.index_runtime.run_indexers(
        file_ref=file_ref, names=["file-embedding"])["file-embedding"]
    _store_indexer_output(context, file_ref, out)
    return out


@op(required_resource_keys={"arango", "index_runtime"})
def file_size_converter_op(context: OpExecutionContext,
                           config: ConverterConfig) -> ConverterOutput:
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


@job(resource_defs={
    "arango": arango_resource,
    "index_runtime": index_runtime_resource
})
def index_file_job() -> None:
    file_ref = provide_file_op()
    file_size_op(file_ref)
    file_stats_op(file_ref)
    full_text_op(file_ref)
    file_summaries_op(file_ref)
    file_embedding_op(file_ref)


@job(resource_defs={
    "arango": arango_resource,
    "index_runtime": index_runtime_resource
})
def convert_files_job() -> None:
    file_size_converter_op()
