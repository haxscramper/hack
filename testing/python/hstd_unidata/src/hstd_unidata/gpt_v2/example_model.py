#!/usr/bin/env python
from __future__ import annotations

from pathlib import Path

from beartype.typing import Annotated, Any
from loguru import logger
from model_dsl import (
    EnFieldAnnotation,
    FixedArray,
    Float32,
    Float64,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    __check__,
    __config__,
    __variant__,
    get_dsl_model,
    op_all,
    op_and,
    op_filter_map,
    op_group_by,
    op_map,
    op_not,
    op_or,
    op_tuple,
    op_walk,
    type_case,
    type_cond,
    typeof,
)
from model_dsl_operations import standard_operations

GvModel = get_dsl_model("graphviz.pkg")
KwModel = get_dsl_model("kiwi.pkg")
M = get_dsl_model(
    "org.sem.model",
    operations=standard_operations(),
)


class TaskStatus(M.Enum):
    PENDING: Annotated[int, EnFieldAnnotation(doc="Task is pending")] = 1
    DONE: Annotated[int, EnFieldAnnotation(doc="Task is completed")] = 2


class ScalarShowcase(M.Class):
    i8: Int8 = M.Field(default=0)
    i16: Int16 = M.Field(default=0)
    i32: Int32 = M.Field(default=0)
    i64: Int64 = M.Field(default=0)
    u8: UInt8 = M.Field(default=0)
    u16: UInt16 = M.Field(default=0)
    u32: UInt32 = M.Field(default=0)
    u64: UInt64 = M.Field(default=0)
    f32: Float32 = M.Field(default=0.0)
    f64: Float64 = M.Field(default=0.0)
    matrix_row: FixedArray[Int32, 4]
    coords: tuple[Int32, Float64, str]
    metadata: dict[str, Int64]
    opaque_payload: Any
    status: TaskStatus = M.Field(default=TaskStatus.PENDING)

    __check__(
        id="scalar-constraints",
        cond=op_and(
            M.This["coords"]._Nth.get(0) <= 100,
            M.This["metadata"]._Map.contains("version"),
            M.This["status"]._Enum.is_val("PENDING"),
            0.5 < M.This["i8"],
            typeof(M.This["metadata"]) == M.Type(dict[str, Int64]),
        ),
        message="Scalar showcase validation failed",
    )


class ColumnView(M.Class):
    class Summary(M.Class):
        class CheckboxAggregate(M.Case):
            class Kind(M.Enum):
                IfAllNested: Annotated[int, EnFieldAnnotation(doc="All nested")] = 0
                AggregateFractionRec: Annotated[
                    int, EnFieldAnnotation(doc="Fraction")
                ] = 1
                AggregatePercentRec: Annotated[
                    int, EnFieldAnnotation(doc="Percent")
                ] = 2

            kind: Kind

        class MathAggregate(M.Case):
            class Kind(M.Enum):
                Min: Annotated[int, EnFieldAnnotation(doc="Minimum")] = 0
                Max: Annotated[int, EnFieldAnnotation(doc="Maximum")] = 1
                Mean: Annotated[int, EnFieldAnnotation(doc="Mean")] = 2
                Sum: Annotated[int, EnFieldAnnotation(doc="Sum")] = 3
                LowHighEst: Annotated[
                    int, EnFieldAnnotation(doc="Low-High estimate")
                ] = 4

            kind: Kind
            formatDigits: Int32 | None = M.Field(default=None)
            formatPrecision: Int32 | None = M.Field(default=None)

        __variant__(
            CheckboxAggregate,
            MathAggregate,
        )

        def __eq__(self, other: Summary) -> bool: ...

    class Column(M.Class):
        summary: ColumnView.Summary | None = M.Field(default=None)
        width: Int32 | None = M.Field(
            default=None,
            check=op_or(
                op_not(M.This._Opt.has_value()),
                0 < M.This._Opt.value(),
            ),
        )
        property: str | None = M.Field(default=None)
        propertyTitle: str | None = M.Field(default=None)

        def __eq__(self, other: ColumnView.Column) -> bool:
            return (
                self.summary,
                self.width,
                self.property,
                self.propertyTitle,
            ) == (
                other.summary,
                other.width,
                other.property,
                other.propertyTitle,
            )

        def __hash__(self) -> int:
            return hash(
                (
                    self.summary,
                    self.width,
                    self.property,
                    self.propertyTitle,
                )
            )

    columns: list[Column] = M.Field(default=[])

    def __eq__(self, other: ColumnView) -> bool: ...


class DiaElement(M.Class):
    class NoAlgo(M.Case):
        pass

    class Graphviz(M.Case):
        graphviz: GvModel._T["GroupAttributePayload"]
        graphviz2: GvModel._T["some.Nested.Thing"]

    class Kiwi(M.Case):
        kiwi: KwModel._T["KiwiThing"]

    id: str

    __variant__(
        NoAlgo,
        Graphviz,
        Kiwi,
        field_name="data",
        enum_name="Kind",
    )

    __check__(
        id="nonempty-id",
        cond=0 < M.This["id"]._Str.size(),
        message="Diagram element ID must not be empty",
    )


class DiaNode(DiaElement, M.Class):
    label: str = M.Field(default="node")


class DiaEdge(DiaElement, M.Class):
    source: str
    target: str
    weight: Int32 | None = M.Field(default=None)


class DiaCluster(DiaElement, M.Class):
    nodes: list[DiaNode]
    edges: list[DiaEdge]
    clusters: list[DiaCluster]

    default_node: DiaNode = M.Field(
        default={
            "id": "root",
            "label": "root_node",
        }
    )

    __check__(
        id="short-id",
        cond=M.This["id"]._Str.size() <= 10,
        message="Cluster ID must not exceed ten characters",
    )

    __check__(
        id="explicit-kiwi-propagation",
        cond=op_or(
            op_not(M.This["data"]._Case.is_case("Kiwi")),
            op_and(
                op_all(
                    M.This["nodes"],
                    "node",
                    M.Iter("node")["data"]._Case.is_case("Kiwi"),
                ),
                op_all(
                    M.This["edges"],
                    "edge",
                    M.Iter("edge")["data"]._Case.is_case("Kiwi"),
                ),
            ),
        ),
        message="Kiwi clusters require Kiwi nodes and edges",
    )

    __check__(
        id="derived-node-types",
        cond=op_all(
            M.This["nodes"],
            "node",
            M.Iter("node").is_derived_from(DiaElement),
        ),
        message=M._Str.format(
            [
                "Unexpected node types; edge algorithms: %s",
                op_map(
                    M.This["edges"],
                    "edge",
                    M.Iter("edge")["data"]._Case.which_case(),
                ),
            ]
        ),
    )

    __check__(
        id="nested-binders",
        cond=op_all(
            M.This["clusters"],
            "cluster",
            op_all(
                M.Iter("cluster")["edges"],
                "edge",
                op_or(
                    M.Iter("edge")["source"] != M.Iter("cluster")["id"],
                    M.Iter("edge")["target"] != M.Iter("cluster")["id"],
                ),
            ),
        ),
        message="An edge must not use its containing cluster at both endpoints",
    )


def diagram_ids(root):
    return op_filter_map(
        op_walk(root),
        "entry",
        type_cond(
            M.Iter("entry"),
            type_case(
                DiaNode,
                "node",
                op_tuple(
                    M.Iter("node")["id"],
                    M.Iter("node").path(),
                ),
            ),
            type_case(
                DiaEdge,
                "edge",
                op_tuple(
                    M.Iter("edge")["id"],
                    M.Iter("edge").path(),
                ),
            ),
            type_case(
                DiaCluster,
                "cluster",
                op_tuple(
                    M.Iter("cluster")["id"],
                    M.Iter("cluster").path(),
                ),
            ),
            unhandled_ignore=True,
        ),
    )


class Diagram(M.Class):
    root: DiaCluster

    __check__(
        id="globally-unique-element-ids",
        cond=op_all(
            op_group_by(
                diagram_ids(M.This["root"]),
                "entry",
                M.Iter("entry")._Nth.get(0),
            )._Map.values(),
            "group",
            M.Iter("group")._List.size() == 1,
        ),
        message="Node, edge, and cluster IDs must be globally unique",
    )


class Org(M.Class):
    fields: list[Org]

    __config__(
        backend={
            "cpp": {
                "section_code": [
                    {
                        "kind": "public",
                        "text": (
                            "[[refl]] virtual OrgSemKind getKind() const = 0;\n"
                            "[[refl]] hstd::Opt<parse::SourceLoc> loc = std::nullopt;"
                        ),
                    }
                ],
            },
        }
    )

    def __init__(self, fields: list[Org]) -> None: ...

    @M.Method(
        backend={
            "cpp": {
                "attributes": ["refl"],
                "is_pure_virtual": True,
            },
        }
    )
    def getKind(self) -> str: ...

    @M.Method()
    def transform[T](self, item: T, *extra: Int32) -> T: ...


class Text(Org, M.Class):
    text: str

    __check__(
        id="empty-subfields",
        cond=M.This["fields"]._List.size() == 0,
        message="Text nodes do not allow subfields",
    )


if __name__ == "__main__":
    result = M.compile()
    Path("/tmp/result.json").write_text(result.model_dump_json(indent=2))
    logger.info("Soufflé model and expression type checking passed")
