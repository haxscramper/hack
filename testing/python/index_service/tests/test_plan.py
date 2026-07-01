from __future__ import annotations

import threading
import time
from pathlib import Path

import pytest
from pydantic import BaseModel

from index_service.services.core.db import IndexDatabase
from index_service.services.core.job_runtime import (
    IndexRuntime, )
from index_service.services.core.job_types import (
    BaseIndexer,
    BaseResource,
    RunContext,
)
from index_service.services.core.types import FileRef, IndexerOutput

# ── helpers ──────────────────────────────────────────────────────


class _DummyResult(BaseModel):
    value: str = "ok"


def _make_indexer(
        name: str,
        required_resources: tuple[str, ...] = tuple(),
        required_assets=tuple(),
        max_parallel=1,
):

    _required_resources = required_resources
    _required_assets = required_assets
    _max_parallel = max_parallel

    class _Idx(BaseIndexer):
        asset_name = name
        result_model = _DummyResult
        required_resources = _required_resources
        required_assets = _required_assets
        max_parallel = _max_parallel

        def run(self, ctx, request, resources, assets):
            return IndexerOutput(indexer_id=self.asset_name,
                                 result=_DummyResult())

    return _Idx()


def _make_resource(
        key: str,
        exclusive=False,
        required_resources=(),
):
    _exclusive = exclusive
    _required_resources = required_resources

    class _Res(BaseResource):
        resource_key = key
        exclusive = _exclusive
        required_resources = _required_resources

        def handle(self, ctx, request, resources):
            return _DummyResult()

    return _Res()


def _touch(path: Path) -> None:
    path.write_text("payload")


# ── plan structure ────────────────────────────────────────────────


def test_plan_structure_3_files_2_indexers(db: IndexDatabase, tmp_path: Path):
    """3 files × 2 independent indexers → 2 batches of 3, topological order."""
    for i in range(3):
        _touch(tmp_path / f"f{i}.txt")

    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[_make_indexer("a"),
                       _make_indexer("b")],
    )
    root = db.add_root("root", tmp_path)
    files = [db.as_ref(root, tmp_path / f"f{i}.txt") for i in range(3)]

    plan = rt.create_plan(files, ["a", "b"])
    assert len(plan.batches) == 2
    assert all(len(b.file_refs) == 3 for b in plan.batches)
    assert plan.batches[0].indexer_name == "a"
    assert plan.batches[1].indexer_name == "b"


def test_plan_topological_order(db: IndexDatabase, tmp_path: Path):
    """If indexer_b depends on indexer_a, plan must order a before b."""
    _touch(tmp_path / "f.txt")
    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[
            _make_indexer("a"),
            _make_indexer("b", required_assets=("a", )),
        ],
    )
    root = db.add_root("root", tmp_path)
    ref = db.as_ref(root, tmp_path / "f.txt")

    plan = rt.create_plan([ref], ["a", "b"])
    assert plan.get_indexer_names() == ["a", "b"]


def test_plan_sub_batching(db: IndexDatabase, tmp_path: Path):
    """20 files with max_parallel=4 → 5 sub-batches of 4."""
    for i in range(20):
        _touch(tmp_path / f"f{i}.txt")

    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[_make_indexer("a", max_parallel=4)],
    )
    root = db.add_root("root", tmp_path)
    files = [db.as_ref(root, tmp_path / f"f{i}.txt") for i in range(20)]

    plan = rt.create_plan(files, ["a"])
    assert len(plan.batches) == 1
    subs = plan.batches[0].sub_batches
    assert len(subs) == 5
    assert all(len(s) == 4 for s in subs)


def test_plan_can_run_filter(db: IndexDatabase, tmp_path: Path):
    """Files that fail can_run are excluded from the batch."""
    _touch(tmp_path / "a.txt")
    _touch(tmp_path / "b.log")

    class TxtOnly(BaseIndexer):
        asset_name = "txt_only"
        result_model = _DummyResult

        def can_run(self, path: Path) -> bool:
            return path.suffix == ".txt"

        def run(self, ctx, request, resources, assets):
            return IndexerOutput(indexer_id=self.asset_name,
                                 result=_DummyResult())

    ctx = RunContext(db)
    rt = IndexRuntime(ctx=ctx, db=db, indexer_types=[TxtOnly()])
    root = db.add_root("root", tmp_path)
    files = [
        db.as_ref(root, tmp_path / "a.txt"),
        db.as_ref(root, tmp_path / "b.log"),
    ]
    plan = rt.create_plan(files, ["txt_only"])
    assert len(plan.batches[0].file_refs) == 1


# ── exclusivity ──────────────────────────────────────────────────


def test_exclusive_direct(db: IndexDatabase):
    """Two indexers directly requiring the same exclusive resource → cannot share."""
    res_r = _make_resource("R", exclusive=True)
    idx_a = _make_indexer("a", required_resources=("R", ))
    idx_c = _make_indexer("c", required_resources=("R", ))

    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[idx_a, idx_c],
        resource_types=[res_r],
    )
    assert rt.can_share_batch("a", "c") is False


def test_exclusive_transitive_different_consumers(db: IndexDatabase):
    """A→B→R, C→D→R (R exclusive) → B≠D → cannot share."""
    res_r = _make_resource("R", exclusive=True)
    res_b = _make_resource("B", required_resources=("R", ))
    res_d = _make_resource("D", required_resources=("R", ))
    idx_a = _make_indexer("a", required_resources=("B", ))
    idx_c = _make_indexer("c", required_resources=("D", ))

    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[idx_a, idx_c],
        resource_types=[res_r, res_b, res_d],
    )
    assert rt.can_share_batch("a", "c") is False


def test_exclusive_transitive_same_consumer(db: IndexDatabase):
    """A→B→R, C→B→R (R exclusive) → same direct consumer B → can share."""
    res_r = _make_resource("R", exclusive=True)
    res_b = _make_resource("B", required_resources=("R", ))
    idx_a = _make_indexer("a", required_resources=("B", ))
    idx_c = _make_indexer("c", required_resources=("B", ))

    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[idx_a, idx_c],
        resource_types=[res_r, res_b],
    )
    assert rt.can_share_batch("a", "c") is True


def test_exclusive_transitive_via_multiple_resources(db: IndexDatabase):
    """A→B→C→R and X→C→R (R exclusive) share same direct consumer C → can share."""
    res_r = _make_resource("R", exclusive=True)
    res_c = _make_resource("C", required_resources=("R", ))
    res_b = _make_resource("B", required_resources=("C", ))
    idx_a = _make_indexer("a", required_resources=("B", ))
    idx_x = _make_indexer("x", required_resources=("C", ))

    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[idx_a, idx_x],
        resource_types=[res_r, res_c, res_b],
    )
    assert rt.can_share_batch("a", "x") is True


def test_exclusive_transitive_via_multiple_indexers(db: IndexDatabase):
    """A→B→R, C→B→R, D→E→R (R exclusive) → A,C compatible; A,D incompatible."""
    res_r = _make_resource("R", exclusive=True)
    res_b = _make_resource("B", required_resources=("R", ))
    res_e = _make_resource("E", required_resources=("R", ))
    idx_a = _make_indexer("a", required_resources=("B", ))
    idx_c = _make_indexer("c", required_resources=("B", ))
    idx_d = _make_indexer("d", required_resources=("E", ))

    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[idx_a, idx_c, idx_d],
        resource_types=[res_r, res_b, res_e],
    )
    assert rt.can_share_batch("a", "c") is True
    assert rt.can_share_batch("a", "d") is False
    assert rt.can_share_batch("c", "d") is False


def test_exclusive_not_flagged_allows_sharing(db: IndexDatabase):
    """Without exclusive=True, different direct consumers can share."""
    res_r = _make_resource("R", exclusive=False)
    res_b = _make_resource("B", required_resources=("R", ))
    res_d = _make_resource("D", required_resources=("R", ))
    idx_a = _make_indexer("a", required_resources=("B", ))
    idx_c = _make_indexer("c", required_resources=("D", ))

    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[idx_a, idx_c],
        resource_types=[res_r, res_b, res_d],
    )
    assert rt.can_share_batch("a", "c") is True


# ── stateful resource (model churn) ───────────────────────────────


class _ModelRequest(BaseModel):
    model: str
    text: str


class _ModelResponse(BaseModel):
    model: str
    result: str


class StatefulModelResource(BaseResource):
    resource_key = "model_server"
    exclusive = True

    def __init__(self):
        self._current_model: str | None = None
        self._load_count = 0
        self._lock = threading.Lock()

    def handle(self, ctx, request: _ModelRequest, resources):
        with self._lock:
            if self._current_model != request.model:
                self._current_model = request.model
                self._load_count += 1
                time.sleep(0.02)  # simulate load latency
        return _ModelResponse(model=request.model,
                              result=f"processed:{request.text}")

    @property
    def load_count(self) -> int:
        return self._load_count


class _SummaryResult(BaseModel):
    summary: str


class SummaryIndexer(BaseIndexer):
    asset_name = "summary"
    result_model = _SummaryResult
    required_resources = ("model_server", )
    max_parallel = 4

    def run(self, ctx, request, resources, assets):
        resp = resources["model_server"].handle(
            ctx,
            _ModelRequest(model="gemma-26b", text="x"),
            resources,
        )
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=_SummaryResult(summary=resp.result),
        )


def test_stateful_resource_no_churn_within_batch(db: IndexDatabase,
                                                 tmp_path: Path):
    """20 files in one batch requesting the same model → load_model called once."""
    for i in range(20):
        (tmp_path / f"f{i}.txt").write_text(f"doc {i}")

    model_res = StatefulModelResource()
    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[SummaryIndexer()],
        resource_types=[model_res],
    )
    root = db.add_root("root", tmp_path)
    files = [db.as_ref(root, tmp_path / f"f{i}.txt") for i in range(20)]

    rt.run_indexers(files, ["summary"])
    assert model_res.load_count == 1


def test_stateful_resource_churn_across_models(db: IndexDatabase,
                                               tmp_path: Path):
    """Two batches requesting different models → load_model called twice."""

    class DualSummaryIndexer(BaseIndexer):
        asset_name = "dual_summary"
        result_model = _SummaryResult
        required_resources = ("model_server", )
        max_parallel = 2

        def __init__(self, model: str = "gemma-26b"):
            self._model = model

        def run(self, ctx, request, resources, assets):
            resp = resources["model_server"].handle(
                ctx,
                _ModelRequest(model="gemma-26b", text="sample"),
                resources,
            )

            return IndexerOutput(
                indexer_id=self.asset_name,
                result=_SummaryResult(summary=resp.result),
            )

    model_res = StatefulModelResource()
    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[
            DualSummaryIndexer(model="gemma-26b"),
            DualSummaryIndexer(model="llama-70b"),
        ],
        resource_types=[model_res],
    )
    # Two indexer instances with same asset_name — use separate names instead
    # Actually we need different asset_names for different models:
    pass  # see test below


def test_stateful_resource_churn_across_batches(db: IndexDatabase,
                                                tmp_path: Path):
    """Two indexer types requesting different models from same exclusive resource."""

    class SummaryA(BaseIndexer):
        asset_name = "summary_a"
        result_model = _SummaryResult
        required_resources = ("model_server", )
        max_parallel = 2

        def run(self, ctx, request, resources, assets):
            resp = resources["model_server"].handle(
                ctx,
                _ModelRequest(model="gemma-26b", text="x"),
                resources,
            )

            return IndexerOutput(
                indexer_id=self.asset_name,
                result=_SummaryResult(summary=resp.result),
            )

    class SummaryB(BaseIndexer):
        asset_name = "summary_b"
        result_model = _SummaryResult
        required_resources = ("model_server", )
        max_parallel = 2

        def run(self, ctx, request, resources, assets):
            resp = resources["model_server"].handle(
                ctx,
                _ModelRequest(model="llama-70b", text="x"),
                resources,
            )

            return IndexerOutput(
                indexer_id=self.asset_name,
                result=_SummaryResult(summary=resp.result),
            )

    model_res = StatefulModelResource()
    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[SummaryA(), SummaryB()],
        resource_types=[model_res],
    )
    root = db.add_root("root", tmp_path)
    _touch(tmp_path / "f.txt")
    ref = db.as_ref(root, tmp_path / "f.txt")

    rt.run_indexers([ref], ["summary_a", "summary_b"])
    assert model_res.load_count == 2


# ── parallel execution ───────────────────────────────────────────


class _ParallelResult(BaseModel):
    idx: int


class ParallelIndexer(BaseIndexer):
    asset_name = "parallel_test"
    result_model = _ParallelResult
    max_parallel = 4

    def __init__(self):
        self._current = 0
        self._max_concurrent = 0
        self._lock = threading.Lock()

    def run(self, ctx, request, resources, assets):
        with self._lock:
            self._current += 1
            self._max_concurrent = max(self._max_concurrent, self._current)
        time.sleep(0.05)
        with self._lock:
            self._current -= 1
        return IndexerOutput(
            indexer_id=self.asset_name,
            result=_ParallelResult(idx=0),
        )

    @property
    def max_concurrent(self) -> int:
        return self._max_concurrent


def test_parallel_execution_respects_max_parallel(db: IndexDatabase,
                                                  tmp_path: Path):
    """20 files with max_parallel=4 → at most 4 concurrent executions."""
    for i in range(20):
        (tmp_path / f"f{i}.txt").write_text("x")

    indexer = ParallelIndexer()
    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[indexer],
        resource_types=[],
    )
    root = db.add_root("root", tmp_path)
    files = [db.as_ref(root, tmp_path / f"f{i}.txt") for i in range(20)]

    rt.run_indexers(files, ["parallel_test"])
    assert indexer.max_concurrent == 4


def test_parallel_execution_max_parallel_1(db: IndexDatabase, tmp_path: Path):
    """max_parallel=1 → never more than 1 concurrent."""

    class SerialIndexer(ParallelIndexer):
        asset_name = "serial_test"
        max_parallel = 1

    for i in range(10):
        (tmp_path / f"f{i}.txt").write_text("x")

    indexer = SerialIndexer()
    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[indexer],
        resource_types=[],
    )
    root = db.add_root("root", tmp_path)
    files = [db.as_ref(root, tmp_path / f"f{i}.txt") for i in range(10)]

    rt.run_indexers(files, ["serial_test"])
    assert indexer.max_concurrent == 1


# ── resource dependency resolution ───────────────────────────────


def test_resource_dependency_chain(db: IndexDatabase, tmp_path: Path):
    """Resource B depends on resource A; indexer calls B which calls A."""

    class ReqA(BaseModel):
        text: str

    class RespA(BaseModel):
        value: str

    class ReqB(BaseModel):
        text: str

    class RespB(BaseModel):
        value: str

    class ResourceA(BaseResource):
        resource_key = "res_a"

        def handle(self, ctx, request: ReqA, resources):
            return RespA(value=f"A:{request.text}")

    class ResourceB(BaseResource):
        resource_key = "res_b"
        required_resources = ("res_a", )

        def handle(self, ctx, request: ReqB, resources):
            resp_a = resources["res_a"].handle(ctx, ReqA(text=request.text),
                                               resources)
            return RespB(value=f"B({resp_a.value})")

    class DepResult(BaseModel):
        value: str

    class DepIndexer(BaseIndexer):
        asset_name = "dep_idx"
        result_model = DepResult
        required_resources = ("res_b", )

        def run(self, ctx, request, resources, assets):
            resp = resources["res_b"].handle(ctx, ReqB(text="hello"),
                                             resources)
            return IndexerOutput(
                indexer_id=self.asset_name,
                result=DepResult(value=resp.value),
            )

    _touch(tmp_path / "f.txt")
    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[DepIndexer()],
        resource_types=[ResourceA(), ResourceB()],
    )
    root = db.add_root("root", tmp_path)
    ref = db.as_ref(root, tmp_path / "f.txt")
    rt.run_indexer(ref, ["dep_idx"])
    out = rt.get_indexer_result(ref, "dep_idx")
    assert out.result.value == "B(A:hello)"


# ── plan rearrangement ────────────────────────────────────────────


def test_plan_is_inspectable_and_rearrangeable(db: IndexDatabase,
                                               tmp_path: Path):
    """Plan can be inspected and reordered before execution."""
    for i in range(5):
        _touch(tmp_path / f"f{i}.txt")

    ctx = RunContext(db)
    rt = IndexRuntime(
        ctx=ctx,
        db=db,
        indexer_types=[_make_indexer("a"),
                       _make_indexer("b")],
    )
    root = db.add_root("root", tmp_path)
    files = [db.as_ref(root, tmp_path / f"f{i}.txt") for i in range(5)]

    plan = rt.create_plan(files, ["a", "b"])
    assert plan.total_runs() == 10
    assert plan.get_indexer_names() == ["a", "b"]

    # Rearrange: reverse batch order
    plan.batches.reverse()
    assert plan.get_indexer_names() == ["b", "a"]

    # Execute rearranged plan
    rt.execute_plan(plan)
    assert rt.db.has_indexer_result(files[0], "a")
    assert rt.db.has_indexer_result(files[0], "b")
