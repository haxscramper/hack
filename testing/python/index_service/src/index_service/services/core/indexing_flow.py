from __future__ import annotations

import logging
from pathlib import Path

from index_service.cli.cli_config import IndexPathConfig, IndexConfig
from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.core.job_types import RunContext
from index_service.services.core.types import FileRef, RootRef
from index_service.services.file_iteration import RootFilter, prepare_root_filters, DirConfig

log = logging.getLogger(__name__)


def _is_file_selected_by_filters(file: Path, filters: list[RootFilter]) -> bool:
    file_str = str(file)

    for root_filter in filters:
        # Fast reject when file is not under this configured directory.
        if not file_str.startswith(root_filter.root_str):
            continue

        try:
            relative = file.relative_to(root_filter.root_path)
        except ValueError:
            continue

        if root_filter.ignore_spec is None:
            return True

        if not root_filter.ignore_spec.match_file(relative.as_posix()):
            return True

    return False


def _assert_dir_paths_under_root(path_cfg: IndexPathConfig) -> None:
    root = path_cfg.root_path.resolve()
    for dir_cfg in path_cfg.paths:
        candidate = dir_cfg.path.resolve()
        assert candidate.is_relative_to(root), (
            f"configured path '{dir_cfg.path}' must be inside root '{path_cfg.root_path}'"
        )


def collect_files_for_path(
    dir_configs: list[DirConfig],
    filters: list[RootFilter],
    limit_per_path: int | None,
) -> list[Path]:
    files: list[Path] = []
    seen: set[Path] = set()

    for dir_cfg in dir_configs:
        source = dir_cfg.path

        if source.is_file():
            candidates = [source]
        else:
            candidates = (p for p in source.rglob("*") if p.is_file())

        for file in candidates:
            if file in seen:
                continue
            if not _is_file_selected_by_filters(file, filters):
                continue

            seen.add(file)
            files.append(file)

            if limit_per_path is not None and len(files) >= limit_per_path:
                return files

    return files


def build_refs_for_root(
    db: IndexDatabase,
    root: RootRef,
    files: list[Path],
) -> list[FileRef]:
    return [db.as_ref(root, file) for file in files]


def run_indexing_per_root_plan(
    db: IndexDatabase,
    runner: IndexRuntime,
    ctx: RunContext,
    cfg: IndexConfig,
    paths: tuple[IndexPathConfig, ...],
    indexers: tuple[str, ...],
) -> None:
    indexed_total = 0
    log.info("constructing index jobs plan")

    for path_cfg in paths:
        if cfg.limit_total is not None and cfg.limit_total <= indexed_total:
            return

        root = db.add_root(path_cfg.name, path_cfg.root_path)

        with ctx.trace_scope("index path", path=path_cfg.name):
            with ctx.trace_scope("validate configured paths", path=path_cfg.name):
                _assert_dir_paths_under_root(path_cfg)

            with ctx.trace_scope("prepare root filters", path=path_cfg.name):
                root_filters = prepare_root_filters(path_cfg.paths)

            with ctx.trace_scope("collect files for path", path=path_cfg.name):
                files = collect_files_for_path(
                    path_cfg.paths,
                    root_filters,
                    cfg.limit_per_path,
                )

            if cfg.limit_total is not None:
                remaining = max(0, cfg.limit_total - indexed_total)
                files = files[:remaining]

            if not files:
                continue

            plan_run_size = cfg.max_plan_run_size or len(files)
            assert plan_run_size > 0, "max_plan_run_size must be > 0"

            total_batches = (len(files) + plan_run_size - 1) // plan_run_size

            for batch_idx, start in enumerate(range(0, len(files), plan_run_size),
                                              start=1):
                batch_files = files[start:start + plan_run_size]

                with ctx.trace_scope(
                        "build refs for root",
                        path=path_cfg.name,
                        batch=batch_idx,
                        batch_files=len(batch_files),
                ):
                    refs = build_refs_for_root(db, root, batch_files)

                indexed_total += len(refs)

                with ctx.trace_scope(
                        "root plan construction",
                        root=root.name,
                        batch=batch_idx,
                        total_batches=total_batches,
                        files=len(refs),
                        indexers=len(indexers),
                ):
                    plan = runner.create_plan(refs, list(indexers))

                log.info("execution plan for root={} path={} batch={}/{}\n{}".format(
                    root.name,
                    str(path_cfg.root_path),
                    batch_idx,
                    total_batches,
                    plan.to_text(),
                ))

                with ctx.trace_scope(
                        "root plan execution",
                        root=root.name,
                        batch=batch_idx,
                        total_batches=total_batches,
                        batches=len(plan.batches),
                        total_runs=plan.total_runs(),
                ):
                    runner.execute_plan(plan)
