#!/usr/bin/env python
import json
from pathlib import Path

from beartype.typing import Annotated, Any, Dict, List, Optional, Tuple
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
    op_map,
    op_not,
    op_or,
)

GvModel = get_dsl_model("graphviz.pkg")
KwModel = get_dsl_model("kiwi.pkg")
M = get_dsl_model("org.sem.model")


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
    coords: Tuple[Int32, Float64, str]
    metadata: Dict[str, Int64]
    opaque_payload: Any
    status: TaskStatus = M.Field(default=TaskStatus.PENDING)

    __check__(
        id="scalar-constraints",
        cond=op_and(
            M.This["coords"]._Nth.get(0) <= 100,
            M.This["metadata"]._Map.contains("version"),
            M.This["status"]._Enum.is_val("PENDING"),
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
            formatDigits: Optional[Int32] = M.Field(default=None, required=False)
            formatPrecision: Optional[Int32] = M.Field(default=None, required=False)

        __variant__(CheckboxAggregate, MathAggregate)

        def __eq__(self, other: "Summary") -> bool: ...

    class Column(M.Class):
        summary: Optional["ColumnView.Summary"] = M.Field(default=None, required=False)
        width: Optional[Int32] = M.Field(
            default=None,
            required=False,
            check=op_or(
                op_not(M.This._Opt.has_value()),
                0 < M.This._Opt.value(),
            ),
        )
        property: Optional[str] = M.Field(default=None, required=False)
        propertyTitle: Optional[str] = M.Field(default=None, required=False)

        def __eq__(self, other: "Column") -> bool: ...

    columns: List[Column] = M.Field(default=[])

    def __eq__(self, other: "ColumnView") -> bool: ...


class DiaElement(M.Class):
    class NoAlgo(M.Case):
        no_algorithm: None

    class Graphviz(M.Case):
        graphviz: GvModel._T["GroupAttributePayload"]
        graphviz2: GvModel._T["some.Nested.Thing"]

    class Kiwi(M.Case):
        kiwi: KwModel._T["KiwiThing"]

    data: Annotated[NoAlgo | Graphviz | Kiwi, M.Variant()] = M.Field()


class DiaNode(DiaElement, M.Class):
    label: str = M.Field(default="node")


class DiaEdge(DiaElement, M.Class):
    weight: Int32 = M.Field(default=1)


class DiaCluster(M.Class):
    field: str = M.Field(
        doc="Cluster identifier",
        check=op_and(
            0 < M.This._Str.size(),
            M.This._Str.size() <= 10,
        ),
    )

    default_node: DiaNode = M.Field(
        default={"label": "root_node"},
    )

    data: Annotated[
        DiaElement.NoAlgo | DiaElement.Graphviz | DiaElement.Kiwi,
        M.Variant(),
    ] = M.Field()

    nodes: List[DiaNode]
    edges: List[DiaEdge]

    __check__(
        id="unified-algorithm",
        message="All nodes and edges must use Kiwi when cluster uses no_algorithm",
        cond=op_or(
            op_not(M.This["data"]._Case.is_case("no_algorithm")),
            op_and(
                op_all(M.This["edges"], M.Iter["data"]._Case.is_case("kiwi")),
                op_all(M.This["nodes"], M.Iter["data"]._Case.is_case("kiwi")),
            ),
        ),
    )

    __check__(
        id="test-format",
        cond=op_all(M.This["nodes"], M.Iter.is_derived_from(DiaElement)),
        message=M._Str.format(
            [
                "edges=[%s]",
                op_map(M.This["edges"], M.Iter["data"]._Case.which_case()),
            ]
        ),
    )


class Org(M.Class):
    fields: List["Org"]

    __config__(
        backend=dict(
            cpp=dict(
                section_code=[
                    dict(
                        kind="public",
                        text=(
                            "[[refl]] virtual OrgSemKind getKind() const = 0;\n"
                            "[[refl]] hstd::Opt<parse::SourceLoc> loc = std::nullopt;"
                        ),
                    )
                ]
            )
        )
    )

    def __init__(self, fields: List["Org"]) -> None: ...

    @staticmethod
    @M.Method(
        backend=dict(
            cpp=dict(
                attributes=["refl"],
                is_pure_virtual=True,
            )
        )
    )
    def getKind() -> str: ...

    @M.Method()
    def transform[T](self, item: T, *extra: Int32) -> T: ...


class Text(Org, M.Class):
    text: str

    __check__(
        id="empty-subfields",
        cond=M.This["fields"]._List.size() == 0,
        message="text nodes don't allow sub-fields",
    )


if __name__ == "__main__":
    M.validate_with_souffle()
    logger.info("Souffle Datalog validation passed")
    Path("/tmp/result.json").write_text(json.dumps(M.to_dict(), indent=2))
