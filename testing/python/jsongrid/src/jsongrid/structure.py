import logging
from dataclasses import dataclass
from enum import Enum

from typing import Any, Dict, List, Optional, Union

log = logging.getLogger(__name__)


class ScalarType(Enum):
    """Concrete JSON scalar type of a leaf value."""

    NULL = "null"
    BOOL = "bool"
    INT = "int"
    FLOAT = "float"
    STRING = "string"


class NodeKind(Enum):
    """Presentation kind inferred for a JSON value."""

    SCALAR = "scalar"
    OBJECT = "object"
    OBJECT_TABLE = "object_table"
    SCALAR_LIST = "scalar_list"
    MATRIX = "matrix"
    MIXED_LIST = "mixed_list"


class TabularityVerdict(Enum):
    """Outcome of the heuristic deciding whether an array renders as a table."""

    TABULAR = "tabular"
    TOO_MANY_COLUMNS = "too_many_columns"
    TOO_SPARSE = "too_sparse"


ScalarValue = Union[None, bool, int, float, str]


def scalar_type_of(value: ScalarValue) -> ScalarType:
    match value:
        case None:
            return ScalarType.NULL
        case bool():
            return ScalarType.BOOL
        case int():
            return ScalarType.INT
        case float():
            return ScalarType.FLOAT
        case str():
            return ScalarType.STRING


def is_scalar(value: Any) -> bool:
    return not isinstance(value, (dict, list))


@dataclass
class TabularityConfig:
    """Thresholds controlling when an array degenerates from a table into a list."""

    min_fill_ratio: float = 0.4
    """Minimum ratio of populated cells to total grid capacity required for a table."""

    max_columns: int = 32
    """Largest column count still considered readable as a table."""


@dataclass
class TabularityDecision:
    """Recorded result of applying the tabularity heuristic to one array."""

    row_count: int
    """Number of elements in the source array."""

    column_count: int
    """Size of the inferred column set."""

    present_count: int
    """Number of populated cells across all rows."""

    fill_ratio: float
    """Populated cells divided by the full row times column capacity."""

    verdict: TabularityVerdict
    """Whether the array is tabular and, if not, why it was rejected."""

    @property
    def tabular(self) -> bool:
        return self.verdict is TabularityVerdict.TABULAR


@dataclass
class NodeBase:
    """Common state of every inferred structure node."""

    path: str
    """JSONPath expression locating this node in the source document."""

    @property
    def kind(self) -> NodeKind:
        raise NotImplementedError(
            f"Structure node class {type(self).__name__} does not declare a NodeKind"
        )


@dataclass
class ScalarNode(NodeBase):
    """Leaf value rendered as a single typed cell."""

    value: ScalarValue
    """Raw value taken from the parsed document."""

    scalar_type: ScalarType
    """Type used to pick the cell styling."""

    @property
    def kind(self) -> NodeKind:
        return NodeKind.SCALAR


@dataclass
class ObjectEntry:
    """Single key and value pair of an object grid."""

    key: str
    """Key as written in the source object."""

    value: "JsonNode"
    """Inferred structure of the associated value."""


@dataclass
class ObjectNode(NodeBase):
    """Object rendered as a two column key and value grid."""

    entries: List[ObjectEntry]
    """Entries in source insertion order."""

    @property
    def kind(self) -> NodeKind:
        return NodeKind.OBJECT


@dataclass
class TableColumn:
    """Single inferred column of a record table."""

    name: str
    """Key name shared by at least one record."""

    occurrence_count: int
    """Number of records that carry this key."""

    fill_ratio: float
    """Occurrence count divided by the record count."""


@dataclass
class TableRow:
    """Single record of a table, with absent keys simply missing from the mapping."""

    index: int
    """Position of the record in the source array."""

    cells: Dict[str, "JsonNode"]
    """Inferred structure of each present key."""


@dataclass
class ObjectTableNode(NodeBase):
    """Array of objects rendered as a table over the union of their keys."""

    columns: List[TableColumn]
    """Columns in order of first appearance across the records."""

    rows: List[TableRow]
    """Records in source order."""

    decision: TabularityDecision
    """Heuristic result that accepted this array as a table."""

    @property
    def kind(self) -> NodeKind:
        return NodeKind.OBJECT_TABLE


@dataclass
class ScalarListNode(NodeBase):
    """Array of leaf values rendered as an index gutter next to a value column."""

    items: List[ScalarNode]
    """Elements in source order."""

    @property
    def kind(self) -> NodeKind:
        return NodeKind.SCALAR_LIST


@dataclass
class MatrixNode(NodeBase):
    """Array of arrays rendered as a positional matrix."""

    column_count: int
    """Length of the longest inner array."""

    rows: List[List["JsonNode"]]
    """Ragged rows that the renderer pads out to the column count."""

    decision: TabularityDecision
    """Heuristic result that accepted this array as a matrix."""

    @property
    def kind(self) -> NodeKind:
        return NodeKind.MATRIX


@dataclass
class MixedListNode(NodeBase):
    """Array rendered as index and value pairs because no column set fits it."""

    items: List["JsonNode"]
    """Elements in source order."""

    decision: Optional[TabularityDecision] = None
    """Heuristic result that rejected the tabular rendering, when one was attempted."""

    @property
    def kind(self) -> NodeKind:
        return NodeKind.MIXED_LIST


ContainerNode = Union[ObjectNode, ObjectTableNode, ScalarListNode, MatrixNode,
                      MixedListNode]
JsonNode = Union[ScalarNode, ObjectNode, ObjectTableNode, ScalarListNode,
                 MatrixNode, MixedListNode]


def container_item_count(node: ContainerNode) -> int:
    match node:
        case ObjectNode():
            return len(node.entries)
        case ObjectTableNode():
            return len(node.rows)
        case ScalarListNode():
            return len(node.items)
        case MatrixNode():
            return len(node.rows)
        case MixedListNode():
            return len(node.items)


def is_empty_container(node: JsonNode) -> bool:
    match node:
        case ScalarNode():
            return False
        case _:
            return container_item_count(node) == 0


def node_summary(node: ContainerNode) -> str:
    match node:
        case ObjectNode():
            return f"{{}} {len(node.entries)} keys"
        case ObjectTableNode():
            return (
                f"[] {len(node.rows)} records, {len(node.columns)} columns, "
                f"fill {node.decision.fill_ratio:.0%}")
        case ScalarListNode():
            return f"[] {len(node.items)} values"
        case MatrixNode():
            return f"[] {len(node.rows)} rows, {node.column_count} columns"
        case MixedListNode():
            if node.decision is None:
                return f"[] {len(node.items)} mixed items"
            return f"[] {len(node.items)} items, {node.decision.verdict.value}"


class StructureInference:
    """Recursive classifier turning parsed JSON into presentation nodes."""

    def __init__(self, config: TabularityConfig) -> None:
        self.config = config

    def classify(self, value: Any, path: str) -> JsonNode:
        match value:
            case dict():
                node = self.classify_object(value, path)
            case list():
                node = self.classify_array(value, path)
            case _:
                return ScalarNode(path=path,
                                  value=value,
                                  scalar_type=scalar_type_of(value))

        log.debug(
            f"{path} -> {node.kind.value} ({container_item_count(node)} items)"
        )
        return node

    def classify_object(self, value: Dict[str, Any], path: str) -> ObjectNode:
        return ObjectNode(
            path=path,
            entries=[
                ObjectEntry(key=key,
                            value=self.classify(item, f"{path}.{key}"))
                for key, item in value.items()
            ],
        )

    def classify_array(self, items: List[Any], path: str) -> ContainerNode:
        if len(items) == 0:
            return ScalarListNode(path=path, items=[])

        if all(isinstance(item, dict) for item in items):
            return self.classify_records(items, path)

        if all(isinstance(item, list) for item in items):
            return self.classify_matrix(items, path)

        if all(is_scalar(item) for item in items):
            return ScalarListNode(
                path=path,
                items=[
                    ScalarNode(
                        path=f"{path}[{index}]",
                        value=item,
                        scalar_type=scalar_type_of(item),
                    ) for index, item in enumerate(items)
                ],
            )

        return self.classify_mixed(items, path, None)

    def classify_records(self, items: List[Any], path: str) -> ContainerNode:
        occurrences: Dict[str, int] = {}
        for item in items:
            for key in item:
                occurrences[key] = occurrences.get(key, 0) + 1

        decision = self.evaluate(
            row_count=len(items),
            column_count=len(occurrences),
            present_count=sum(occurrences.values()),
        )
        if not decision.tabular:
            log.debug(f"{path} rejected as record table: {decision}")
            return self.classify_mixed(items, path, decision)

        return ObjectTableNode(
            path=path,
            columns=[
                TableColumn(
                    name=name,
                    occurrence_count=count,
                    fill_ratio=count / len(items),
                ) for name, count in occurrences.items()
            ],
            rows=[
                TableRow(
                    index=index,
                    cells={
                        key: self.classify(item[key], f"{path}[{index}].{key}")
                        for key in item
                    },
                ) for index, item in enumerate(items)
            ],
            decision=decision,
        )

    def classify_matrix(self, items: List[Any], path: str) -> ContainerNode:
        column_count = max(len(item) for item in items)
        decision = self.evaluate(
            row_count=len(items),
            column_count=column_count,
            present_count=sum(len(item) for item in items),
        )
        if not decision.tabular:
            log.debug(f"{path} rejected as matrix: {decision}")
            return self.classify_mixed(items, path, decision)

        return MatrixNode(
            path=path,
            column_count=column_count,
            rows=[[
                self.classify(cell, f"{path}[{row}][{column}]")
                for column, cell in enumerate(item)
            ] for row, item in enumerate(items)],
            decision=decision,
        )

    def classify_mixed(
            self, items: List[Any], path: str,
            decision: Optional[TabularityDecision]) -> MixedListNode:
        return MixedListNode(
            path=path,
            items=[
                self.classify(item, f"{path}[{index}]")
                for index, item in enumerate(items)
            ],
            decision=decision,
        )

    def evaluate(self, row_count: int, column_count: int,
                 present_count: int) -> TabularityDecision:
        capacity = row_count * column_count
        fill_ratio = present_count / capacity if 0 < capacity else 0.0
        if self.config.max_columns < column_count:
            verdict = TabularityVerdict.TOO_MANY_COLUMNS
        elif fill_ratio < self.config.min_fill_ratio:
            verdict = TabularityVerdict.TOO_SPARSE
        else:
            verdict = TabularityVerdict.TABULAR

        return TabularityDecision(
            row_count=row_count,
            column_count=column_count,
            present_count=present_count,
            fill_ratio=fill_ratio,
            verdict=verdict,
        )
