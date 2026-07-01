from __future__ import annotations

import logging
from pathlib import Path

from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_runtime import IndexRuntime
from index_service.services.core.job_types import RunContext
from index_service.services.core.types import FileRef, RootRef

log = logging.getLogger(__name__)


def collect_files_for_path(
    path: Path,
    limit_per_path: int | None,
) -> list[Path]:
    if path.is_file():
        return [path]

    files: list[Path] = []
    for file in path.rglob("*"):
        if not file.is_file():
            continue
        files.append(file)
        if limit_per_path is not None and len(files) >= limit_per_path:
            break

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
    paths: tuple[Path, ...],
    indexers: tuple[str, ...],
    limit_total: int | None,
    limit_per_path: int | None,
) -> None:
    indexed_total = 0

    for path in paths:
        if limit_total is not None and indexed_total >= limit_total:
            return

        root = db.add_root(path.name, path)

        with ctx.trace_scope("index path", path=str(path)):
            files = collect_files_for_path(path, limit_per_path)

            if limit_total is not None:
                remaining = max(0, limit_total - indexed_total)
                files = files[:remaining]

            if not files:
                continue

            refs = build_refs_for_root(db, root, files)
            indexed_total += len(refs)

            with ctx.trace_scope(
                    "root plan construction",
                    root=root.name,
                    files=len(refs),
                    indexers=len(indexers),
            ):
                plan = runner.create_plan(refs, list(indexers))

            log.info(
                "execution plan for root={} path={}\n{}".format(
                    root.name, str(path), plan.to_text()), )

            with ctx.trace_scope(
                    "root plan execution",
                    root=root.name,
                    batches=len(plan.batches),
                    total_runs=plan.total_runs(),
            ):
                runner.execute_plan(plan)
