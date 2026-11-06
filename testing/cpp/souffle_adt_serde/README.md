# Souffle ADT ↔ JSON through SQLite

Minimal C++23 stateful functor library with nlohmann JSON, CMake, Conan 2, and one Python driver. The `.dl` file is the only manually maintained serde schema.

## Run

Prerequisites: installed Souffle and its matching C++ headers, SQLite and libffi support in that Souffle build, GCC/OpenMP, CMake, Conan 2 with a configured default profile, just, and uv.

```sh
just
```

Conan resolves the C++ dependencies; uv resolves the Python dependencies. Initial dependency resolution requires network access. CMake derives `RAM_DOMAIN_SIZE` from the installed `souffle --version` output rather than assuming a 32-bit ABI.

`example.py` regenerates `build/example/exchange.sqlite` on every run. No other database is removed. It also saves `schema.json` and the compiler's `transformed.dl` output in that directory.

## Exchange

1. Python asks the same Souffle executable for `--show=transformed-ast`.
2. Python combines compiler-generated I/O type metadata with field names from canonical `.type` declarations.
3. Python inserts metadata into `SerdeSchema.schema TEXT` and example values into `Input.payload TEXT` in SQLite.
4. Souffle reads those columns as symbols; `hstd_from_json` recursively constructs genuine ADT values in its current record and symbol tables.
5. Datalog wraps each value in `TOpt` and extracts an array size through ordinary ADT pattern matching.
6. `hstd_to_json` recursively serializes the resulting ADT, and Souffle writes the JSON symbol to SQLite.
7. Python selects JSON text from the `Output` view and decodes it with `json.loads`.

There are no CSV/TSV fact files. The schema sidecar is for inspection; runtime metadata also travels through SQLite.

## Wire format

`head` is the Souffle constructor name, not the enclosing sum-type name: a value of `Type` has a head such as `TArray`, not `Type`. Constructor and field names retain compiler qualification.

```json
{
  "head": "TArray",
  "fields": {
    "elem": {
      "head": "TPrim",
      "fields": {
        "prim": {"head": "PString", "fields": {}}
      }
    },
    "size": 4
  }
}
```

Every ADT, including an enum or a zero-field constructor, uses the same object shape. Primitive fields are JSON strings or numbers. Records use the same shape with their declared type name as `head`; record `nil` is JSON `null`.

Linked lists remain linked ADT structures, not special-cased JSON arrays. The C++ codec has no knowledge of `Type`, `Prim`, `QName`, or the list constructors.

The example adds the missing `TypeList` declaration and a `TUnresolved` constructor corresponding to the supplied Python model. The Python `AdtValue` dataclass is only a generic wire-value builder, not another TypeRef schema or Pydantic integration.

## Metadata and initialization

`--show=type-analysis` reports inferred argument types; it is not a complete named-field reflection interface. The transformed AST is more useful here: I/O `types` metadata gives exact runtime branch order, field type qualifiers, and the enum flag, while `.type` declarations supply ADT field names absent from that metadata.

The extractor only parses named constructor fields in Souffle's canonical printed declarations, not arbitrary Datalog source. Record field names come directly from I/O `params` metadata. An incompatible compiler output format fails explicitly rather than guessing tags or field positions.

`hstd_init_souffle_analysis` caches an immutable parsed schema under a mutex and returns the original metadata symbol as a token. Repeated calls reuse the cache. Both serde functors also resolve that token through the cache, so correctness never depends on an initialization-only side effect or rule scheduling.

The metadata cache contains no symbol IDs, record IDs, or pointers to a program's runtime tables. A repeated invocation uses the tables passed to that invocation. Regenerate metadata whenever the Datalog schema changes.

Souffle functor declarations remain statically typed. This example declares the two serde functors for root `Type`; their C++ implementation dispatches entirely through metadata. To use another root in a separate program, change those declarations and the root-name argument, not the C++ traversal. Simultaneous distinct root signatures need distinct exported wrapper names.

## SQLite details and verified assumptions

Native SQLite input does not parse ADT constructor syntax or JSON into ADTs. Reading JSON into a `symbol` and invoking a stateful functor avoids that limitation; CSV/TSV is not required for this exchange.

Native SQLite output does not print ADT constructor syntax. It writes internal numeric ADT values, which are not a portable serialization. This example outputs JSON symbols instead of outputting an ADT column directly.

Souffle's native SQLite writer interns symbol text in `__SymbolTable.symbol TEXT`, stores IDs in `_Output`, and exposes decoded strings through the `Output` view. Thus `SELECT id, payload FROM Output` returns JSON text, but `Output` is not a flat physical TEXT-column table. The array-size output follows the same native table/view convention.

Stateful functors receive symbol and record tables, not schema reflection. The compiler metadata bridge is necessary for generic named-field serde without a duplicate handwritten schema.

## Runtime representation

The codec follows the compiler's representation: enum-only ADTs are integer tags; other ADTs are two-field records containing a tag and payload. A one-field branch uses its argument directly as payload; zero-field and multi-field branches use a packed argument record.

Branch indices come from compiler-generated metadata, never declaration-order assumptions. Signed and unsigned integers are range checked against the active RAM domain. Floats use Souffle's active precision and bit representation; nonfinite values are rejected because JSON has no representation for them.

Exceptions are not converted into sentinel values. Invalid JSON or incompatible data fails the Souffle invocation; exceptions from functors are not a recoverable Datalog error channel, especially under parallel execution.

## Source verification

Checked against upstream commit `2adf8c07210fe54f235e6e2d343c49b9d131463d`. Compiler metadata and runtime layout are implementation interfaces, not a promised cross-version reflection ABI; this POC needs revisiting if those interfaces change.

- [SQLite input](https://github.com/souffle-lang/souffle/blob/2adf8c07210fe54f235e6e2d343c49b9d131463d/src/include/souffle/io/ReadStreamSQLite.h)
- [SQLite output](https://github.com/souffle-lang/souffle/blob/2adf8c07210fe54f235e6e2d343c49b9d131463d/src/include/souffle/io/WriteStreamSQLite.h)
- [I/O metadata](https://github.com/souffle-lang/souffle/blob/2adf8c07210fe54f235e6e2d343c49b9d131463d/src/ast/transform/IOAttributes.h)
- [ADT input layout](https://github.com/souffle-lang/souffle/blob/2adf8c07210fe54f235e6e2d343c49b9d131463d/src/include/souffle/io/ReadStream.h)
- [ADT compiler lowering](https://github.com/souffle-lang/souffle/blob/2adf8c07210fe54f235e6e2d343c49b9d131463d/src/ast2ram/seminaive/ValueTranslator.cpp)
- [Stateful functor interface](https://souffle-lang.github.io/functors)

No serde conversion tests are included. Builds, execution, linters, and formatters were not run while creating this project.
