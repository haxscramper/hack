#!/usr/bin/env python

from __future__ import annotations

import json
import re
import sqlite3
from contextlib import closing
from dataclasses import asdict, dataclass, field
from pathlib import Path

from beartype import beartype
from loguru import logger
from plumbum import local


@beartype
@dataclass(frozen=True)
class AdtValue:
    """Consumer-independent wire value; head names a Souffle constructor."""

    head: str
    fields: dict[str, AdtValue | str | int | float | None] = field(default_factory=dict)


@beartype
def _adt(head: str, **fields: AdtValue | str | int | float | None) -> AdtValue:
    return AdtValue(head=head, fields=fields)


@beartype
def _io_parameter(ast: str, parameter: str) -> str:
    """Souffle prints embedded I/O JSON without escaping its internal quotes."""
    match = re.search(rf'\b{re.escape(parameter)}="(?=\{{)', ast)
    if match is None:
        raise ValueError(
            f"Compiler output contains no {parameter!r} I/O metadata; "
            f"expected --show=transformed-ast for a program with .input or .output"
        )
    start = match.end()
    _, end = json.JSONDecoder().raw_decode(ast, start)
    return ast[start:end]


@beartype
def _prepare_metadata(souffle: Path, program: Path, bits: int, work: Path) -> str:
    ast = local[str(souffle)]["--show=transformed-ast", str(program)]()
    work.joinpath("transformed.dl").write_text(ast, encoding="utf-8")
    types = json.loads(_io_parameter(ast, "types"))
    params = json.loads(_io_parameter(ast, "params"))
    names: dict[str, list[str]] = {}

    logger.info(f"{ast}")

    for declaration in re.finditer(r"(?m)^\.type\s+[^\s=]+\s*=\s*([^\n]+)", ast):
        for branch in re.finditer(r"([\w?.]+)\s*\{([^{}]*)\}", declaration.group(1)):
            fields: list[str] = []
            for attribute in filter(
                None, (item.strip() for item in branch.group(2).split(","))
            ):
                match = re.fullmatch(r"([\w?]+)\s*:\s*([\w?.]+)", attribute)
                if match is None:
                    raise ValueError(
                        f"Cannot extract field names from constructor {branch.group(1)!r}: "
                        f"attribute {attribute!r} is not a named field with a named type"
                    )
                fields.append(match.group(1))
            names[branch.group(1)] = fields

    for type_name, info in types["ADTs"].items():
        for branch in info["branches"]:
            constructor = branch["name"]
            if constructor not in names:
                raise ValueError(
                    f"Runtime metadata for {type_name!r} names constructor {constructor!r}, "
                    f"but no matching .type branch was found in the transformed AST"
                )
            branch["names"] = names[constructor]
            if len(branch["names"]) != len(branch["types"]):
                raise ValueError(
                    f"Constructor {constructor!r} in {type_name!r} has fields "
                    f"{branch['names']!r}, but runtime types {branch['types']!r}; "
                    f"compiler output is inconsistent with the field-name extractor"
                )

    for type_name, info in types["records"].items():
        record_name = type_name.split(":", 1)[1]
        info["names"] = params["records"][record_name]["params"]

    metadata = {
        "format": 1,
        "domain_bits": bits,
        "ADTs": types["ADTs"],
        "records": types["records"],
    }
    text = json.dumps(metadata, ensure_ascii=False, sort_keys=True)
    work.joinpath("schema.json").write_text(text + "\n", encoding="utf-8")
    logger.info(
        f"Derived serde metadata for {len(metadata['ADTs'])} ADT types from {program}"
    )
    return text


@beartype
def _samples() -> list[AdtValue]:
    primitive = _adt("TPrim", prim=_adt("PString"))
    scope = _adt("QNameValue", name="demo", scope=_adt("NoScope"))
    qualified = _adt("QNameValue", name="Widget", scope=_adt("SomeScope", value=scope))
    elements = [
        _adt("TNamed", qname=qualified),
        _adt("TExternal", package="vendor", qname=qualified),
        _adt("TVariant", owner=qualified, field="payload"),
        _adt("TDyn", root=_adt("TParam", name="T")),
        _adt("TUnresolved", text="Future[Widget]"),
    ]
    sequence = _adt("TypeNil")
    for element in reversed(elements):
        sequence = _adt("TypeCons", first=element, rest=sequence)
    return [
        primitive,
        _adt("TArray", elem=_adt("TList", elem=primitive), size=4),
        _adt("TMap", key=primitive, value=_adt("TTuple", elems=sequence)),
    ]


@beartype
def main() -> None:
    root = Path(__file__).resolve().parent
    runtime = json.loads(
        root.joinpath("build", "runtime.json").read_text(encoding="utf-8")
    )
    souffle = Path(runtime["souffle"])
    version = local[str(souffle)]["--version"]()
    match = re.search(r"Word size: (32|64) bits", version)
    if match is None or int(match.group(1)) != runtime["domain_bits"]:
        raise RuntimeError(
            f"Souffle at {souffle} no longer matches the built functor's "
            f"{runtime['domain_bits']}-bit ABI; rebuild with just build. Got: {version}"
        )

    work = root.joinpath("build", "example")
    work.mkdir(parents=True, exist_ok=True)
    metadata = _prepare_metadata(
        souffle, root / "model.dl", runtime["domain_bits"], work
    )
    database = work / "exchange.sqlite"
    database.unlink(missing_ok=True)

    with closing(sqlite3.connect(database)) as connection:
        with connection:
            connection.execute(
                "CREATE TABLE Input (id INTEGER PRIMARY KEY, payload TEXT NOT NULL)"
            )
            connection.execute("CREATE TABLE SerdeSchema (schema TEXT NOT NULL)")
            connection.execute("INSERT INTO SerdeSchema VALUES (?)", (metadata,))
            for identifier, value in enumerate(_samples(), start=1):
                payload = json.dumps(asdict(value), ensure_ascii=False)
                connection.execute(
                    "INSERT INTO Input VALUES (?, ?)", (identifier, payload)
                )
                logger.info(f"Python → SQLite, id={identifier}: {payload}")

    with local.cwd(work):
        local[str(souffle)][
            "-L",
            runtime["library_dir"],
            "-l",
            "hstd_serde",
            "-F",
            str(work),
            "-D",
            str(work),
            str(root / "model.dl"),
        ]()

    with closing(sqlite3.connect(database)) as connection:
        connection.row_factory = sqlite3.Row
        for row in connection.execute("SELECT id, payload FROM Output ORDER BY id"):
            value = json.loads(row["payload"])
            logger.info(
                f"Souffle → SQLite → Python, id={row['id']}: {json.dumps(value, ensure_ascii=False)}"
            )
        for row in connection.execute("SELECT id, size FROM ArraySize ORDER BY id"):
            logger.info(
                f"Souffle extracted array size {row['size']} from input id={row['id']}"
            )

    logger.info(f"SQLite exchange database: {database}")


if __name__ == "__main__":
    main()
