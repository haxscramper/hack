#!/usr/bin/env python

import argparse
import ast
import math
import re
from dataclasses import dataclass
from pathlib import Path

import igraph
import tomllib
from beartype import beartype
from beartype.typing import Iterable
from graphviz import Digraph
from plumbum import local
from rich.console import Console
from rich.markup import escape
from rich.tree import Tree


@beartype
@dataclass(frozen=True)
class PackageSource:
    path: Path
    module: str
    is_package: bool


@beartype
@dataclass(frozen=True)
class Package:
    name: str
    root: Path
    pyproject: Path
    sources: tuple[PackageSource, ...]
    module_roots: tuple[str, ...]


@beartype
@dataclass(frozen=True)
class ImportUse:
    module: str
    conditional: bool


@beartype
@dataclass(frozen=True)
class ImportCount:
    source: str
    target: str
    target_internal: bool
    unconditional: int
    conditional: int


@beartype
class ImportVisitor(ast.NodeVisitor):
    def __init__(self, source: PackageSource) -> None:
        self.source = source
        self.conditional_depth = 0
        self.imports: list[ImportUse] = []

    def record(self, module: str) -> None:
        if module:
            self.imports.append(
                ImportUse(
                    module=module,
                    conditional=0 < self.conditional_depth,
                )
            )

    def visit_conditional(self, nodes: Iterable[ast.AST]) -> None:
        self.conditional_depth += 1
        for node in nodes:
            self.visit(node)
        self.conditional_depth -= 1

    def resolve_relative(self, node: ast.ImportFrom) -> str:
        if self.source.is_package:
            package_parts = self.source.module.split(".")
        else:
            package_parts = self.source.module.split(".")[:-1]

        remove_count = node.level - 1
        if len(package_parts) < remove_count:
            raise ValueError(
                f"Relative import level {node.level} escapes module "
                f"{self.source.module!r} in {self.source.path}"
            )

        if remove_count:
            package_parts = package_parts[:-remove_count]

        if node.module:
            package_parts.extend(node.module.split("."))

        return ".".join(package_parts)

    def visit_Import(self, node: ast.Import) -> None:
        for alias in node.names:
            self.record(alias.name)

    def visit_ImportFrom(self, node: ast.ImportFrom) -> None:
        if node.level:
            self.record(self.resolve_relative(node))
        elif node.module:
            self.record(node.module)

    def visit_If(self, node: ast.If) -> None:
        self.visit(node.test)
        self.visit_conditional(node.body)
        self.visit_conditional(node.orelse)

    def visit_For(self, node: ast.For) -> None:
        self.visit(node.target)
        self.visit(node.iter)
        self.visit_conditional(node.body)
        self.visit_conditional(node.orelse)

    def visit_AsyncFor(self, node: ast.AsyncFor) -> None:
        self.visit(node.target)
        self.visit(node.iter)
        self.visit_conditional(node.body)
        self.visit_conditional(node.orelse)

    def visit_While(self, node: ast.While) -> None:
        self.visit(node.test)
        self.visit_conditional(node.body)
        self.visit_conditional(node.orelse)

    def visit_Try(self, node: ast.Try) -> None:
        self.visit_conditional(node.body)
        self.visit_conditional(node.handlers)
        self.visit_conditional(node.orelse)
        self.visit_conditional(node.finalbody)

    def visit_TryStar(self, node: ast.TryStar) -> None:
        self.visit_conditional(node.body)
        self.visit_conditional(node.handlers)
        self.visit_conditional(node.orelse)
        self.visit_conditional(node.finalbody)

    def visit_ExceptHandler(self, node: ast.ExceptHandler) -> None:
        if node.type:
            self.visit(node.type)
        for statement in node.body:
            self.visit(statement)

    def visit_Match(self, node: ast.Match) -> None:
        self.visit(node.subject)
        self.visit_conditional(node.cases)

    def visit_match_case(self, node: ast.match_case) -> None:
        self.visit(node.pattern)
        if node.guard:
            self.visit(node.guard)
        for statement in node.body:
            self.visit(statement)


@beartype
def read_pyproject(path: Path) -> dict[str, object]:
    with path.open("rb") as stream:
        return tomllib.load(stream)


@beartype
def find_package_name(data: dict[str, object], path: Path) -> str:
    project = data.get("project")
    if isinstance(project, dict):
        name = project.get("name")
        if isinstance(name, str) and name:
            return name

    tool = data.get("tool")
    if isinstance(tool, dict):
        poetry = tool.get("poetry")
        if isinstance(poetry, dict):
            name = poetry.get("name")
            if isinstance(name, str) and name:
                return name

    raise ValueError(f"Neither project.name nor tool.poetry.name is defined in {path}")


@beartype
def find_git_files(
    target: Path,
    patterns: tuple[str, ...],
) -> tuple[Path, ...]:
    git = local["git"]

    inside_worktree = git[
        "-C",
        str(target),
        "rev-parse",
        "--is-inside-work-tree",
    ]().strip()

    if inside_worktree != "true":
        raise ValueError(f"Target directory is not inside a Git worktree: {target}")

    result = git[
        "-C",
        str(target),
        "ls-files",
        "-z",
        "--cached",
        "--others",
        "--exclude-standard",
        "--",
        *patterns,
    ]()

    return tuple(
        sorted(
            {
                (target / relative).resolve()
                for relative in result.split("\0")
                if relative
            },
            key=str,
        )
    )


@beartype
def module_name(path: Path, source_root: Path) -> str:
    relative = path.relative_to(source_root)
    parts = list(relative.parts)

    if parts[-1] == "__init__.py":
        parts = parts[:-1]
    else:
        parts[-1] = path.stem

    return ".".join(parts)


@beartype
def contains_python(
    path: Path,
    python_files: tuple[Path, ...],
) -> bool:
    resolved = path.resolve()
    return any(
        candidate == resolved or resolved in candidate.parents
        for candidate in python_files
    )


@beartype
def find_setuptools_source_root(
    root: Path,
    data: dict[str, object],
) -> Path | None:
    tool = data.get("tool")
    if not isinstance(tool, dict):
        return None

    setuptools = tool.get("setuptools")
    if not isinstance(setuptools, dict):
        return None

    package_dir = setuptools.get("package-dir")
    if not isinstance(package_dir, dict):
        return None

    configured_root = package_dir.get("")
    if isinstance(configured_root, str):
        return (root / configured_root).resolve()

    return None


@beartype
def find_poetry_package_paths(
    root: Path,
    data: dict[str, object],
) -> list[tuple[Path, Path]]:
    tool = data.get("tool")
    if not isinstance(tool, dict):
        return []

    poetry = tool.get("poetry")
    if not isinstance(poetry, dict):
        return []

    configured_packages = poetry.get("packages")
    if not isinstance(configured_packages, list):
        return []

    result: list[tuple[Path, Path]] = []

    for entry in configured_packages:
        if not isinstance(entry, dict):
            raise ValueError(
                f"Poetry package entry in {root / 'pyproject.toml'} is not "
                f"a table: {entry!r}"
            )

        include = entry.get("include")
        source = entry.get("from", ".")

        if not isinstance(include, str) or not isinstance(source, str):
            raise ValueError(
                f"Poetry package entry in {root / 'pyproject.toml'} must "
                f"contain string include and from values: {entry!r}"
            )

        if any(character in include for character in "*?["):
            raise ValueError(
                f"Globbed Poetry package include {include!r} is not supported "
                f"in {root / 'pyproject.toml'}"
            )

        source_root = (root / source).resolve()
        package_path = (source_root / include).resolve()
        result.append((package_path, source_root))

    return result


@beartype
def find_candidate_package_paths(
    root: Path,
    data: dict[str, object],
    package_name: str,
    python_files: tuple[Path, ...],
) -> list[tuple[Path, Path]]:
    poetry_paths = find_poetry_package_paths(root, data)
    if poetry_paths:
        return poetry_paths

    configured_root = find_setuptools_source_root(root, data)
    if configured_root:
        source_root = configured_root
    elif contains_python(root / "src", python_files):
        source_root = (root / "src").resolve()
    else:
        source_root = root.resolve()

    normalized_name = re.sub(r"[-.]+", "_", package_name)
    expected_path = source_root.joinpath(*normalized_name.split(".")).resolve()

    if contains_python(expected_path, python_files):
        return [(expected_path, source_root)]

    expected_module = (source_root / f"{normalized_name}.py").resolve()
    if expected_module in python_files:
        return [(expected_module, source_root)]

    candidates: set[Path] = set()

    for path in python_files:
        if source_root not in path.parents:
            continue

        relative = path.relative_to(source_root)
        if not relative.parts:
            continue

        if 1 < len(relative.parts):
            candidates.add((source_root / relative.parts[0]).resolve())

    if not candidates:
        raise ValueError(
            f"No non-ignored Python package sources were found for "
            f"{package_name!r} under {source_root}"
        )

    return [(candidate, source_root) for candidate in sorted(candidates, key=str)]


@beartype
def list_worktree_files(target: Path) -> tuple[Path, ...]:
    result = local["git"][
        "-C",
        str(target),
        "ls-files",
        "-z",
        "--cached",
        "--others",
        "--exclude-standard",
    ]()

    return tuple(
        sorted(
            {
                target / relative
                for relative in result.split("\0")
                if relative and (target / relative).is_file()
            },
            key=str,
        )
    )


@beartype
def owning_project(path: Path, project_roots: set[Path]) -> Path | None:
    for parent in path.parents:
        if parent in project_roots:
            return parent

    return None


@beartype
def module_parts(path: Path, root: Path, files: set[Path]) -> list[str]:
    parts = [] if path.name == "__init__.py" else [path.stem]
    directory = path.parent

    while directory != root and directory / "__init__.py" in files:
        parts.insert(0, directory.name)
        directory = directory.parent

    return parts


@beartype
def discover_packages(target: Path) -> tuple[Package, ...]:
    files = list_worktree_files(target)
    file_set = set(files)

    pyprojects = [path for path in files if path.name == "pyproject.toml"]
    if not pyprojects:
        raise ValueError(
            f"No non-ignored pyproject.toml files were found under {target}"
        )

    project_roots = {pyproject.parent for pyproject in pyprojects}
    sources_by_root: dict[Path, list[PackageSource]] = {
        root: [] for root in project_roots
    }

    for path in files:
        if path.suffix != ".py":
            continue

        root = owning_project(path, project_roots)
        if root is None:
            continue

        parts = module_parts(path, root, file_set)
        if not parts:
            continue

        sources_by_root[root].append(
            PackageSource(
                path=path,
                module=".".join(parts),
                is_package=path.name == "__init__.py",
            )
        )

    packages: list[Package] = []

    for pyproject in pyprojects:
        root = pyproject.parent
        name = find_package_name(read_pyproject(pyproject), pyproject)
        sources = tuple(sources_by_root[root])
        module_roots = tuple(
            sorted(
                {
                    source.module.split(".")[0]
                    for source in sources
                    if source.is_package or "." in source.module
                }
            )
        )

        packages.append(
            Package(
                name=name,
                root=root,
                pyproject=pyproject,
                sources=sources,
                module_roots=module_roots,
            )
        )

    package_paths: dict[str, Path] = {}

    for package in packages:
        previous = package_paths.get(package.name)
        if previous:
            raise ValueError(
                f"Package name {package.name!r} is defined by both "
                f"{previous} and {package.pyproject}"
            )

        package_paths[package.name] = package.pyproject

    module_owners: dict[str, str] = {}

    for package in packages:
        for imported_root in package.module_roots:
            previous = module_owners.get(imported_root)
            if previous and previous != package.name:
                raise ValueError(
                    f"Import root {imported_root!r} is owned by both "
                    f"{previous!r} and {package.name!r}"
                )

            module_owners[imported_root] = package.name

    return tuple(packages)


@beartype
def parse_imports(source: PackageSource) -> tuple[ImportUse, ...]:
    text = source.path.read_text(encoding="utf-8")
    syntax = ast.parse(
        text,
        filename=str(source.path),
        type_comments=True,
    )
    visitor = ImportVisitor(source)
    visitor.visit(syntax)
    return tuple(visitor.imports)


@beartype
def collect_import_counts(
    packages: tuple[Package, ...],
) -> tuple[ImportCount, ...]:
    module_owners: dict[str, str] = {}

    for package in packages:
        for imported_root in package.module_roots:
            module_owners[imported_root] = package.name

    counts: dict[tuple[str, str, bool], list[int]] = {}

    for package in packages:
        for source in package.sources:
            for imported in parse_imports(source):
                imported_root = imported.module.split(".")[0]
                internal_target = module_owners.get(imported_root)

                if internal_target == package.name:
                    continue

                target = internal_target or imported_root
                key = (
                    package.name,
                    target,
                    internal_target is not None,
                )
                values = counts.setdefault(key, [0, 0])

                if imported.conditional:
                    values[1] += 1
                else:
                    values[0] += 1

    return tuple(
        ImportCount(
            source=source,
            target=target,
            target_internal=target_internal,
            unconditional=values[0],
            conditional=values[1],
        )
        for (
            source,
            target,
            target_internal,
        ), values in sorted(counts.items())
    )


@beartype
def build_graph(
    packages: tuple[Package, ...],
    counts: tuple[ImportCount, ...],
) -> igraph.Graph:
    internal_names = {package.name for package in packages}
    external_names = {count.target for count in counts if not count.target_internal}

    vertices = [(f"internal:{name}", name, True) for name in sorted(internal_names)]
    vertices.extend(
        (f"external:{name}", name, False) for name in sorted(external_names)
    )

    graph = igraph.Graph(directed=True)
    graph.add_vertices(len(vertices))
    graph.vs["name"] = [identifier for identifier, label, internal in vertices]
    graph.vs["label"] = [label for identifier, label, internal in vertices]
    graph.vs["internal"] = [internal for identifier, label, internal in vertices]

    vertex_indices = {vertex["name"]: vertex.index for vertex in graph.vs}

    edges: list[tuple[int, int]] = []

    for count in counts:
        source = vertex_indices[f"internal:{count.source}"]
        target_kind = "internal" if count.target_internal else "external"
        target = vertex_indices[f"{target_kind}:{count.target}"]
        edges.append((source, target))

    graph.add_edges(edges)
    graph.es["unconditional"] = [count.unconditional for count in counts]
    graph.es["conditional"] = [count.conditional for count in counts]
    graph.es["weight"] = [count.unconditional + count.conditional for count in counts]

    return graph


@beartype
def write_tree(
    packages: tuple[Package, ...],
    counts: tuple[ImportCount, ...],
    output: Path,
) -> None:
    imports_by_source: dict[str, list[ImportCount]] = {
        package.name: [] for package in packages
    }

    for count in counts:
        imports_by_source[count.source].append(count)

    tree = Tree("[bold]Package imports[/bold]")

    for package in sorted(packages, key=lambda value: value.name):
        branch = tree.add(f"[bold cyan]{escape(package.name)}[/bold cyan]")
        imports = sorted(
            imports_by_source[package.name],
            key=lambda value: (
                not value.target_internal,
                value.target,
            ),
        )

        if not imports:
            branch.add("[dim](no package imports)[/dim]")
            continue

        for imported in imports:
            if imported.target_internal:
                target = f"[cyan]{escape(imported.target)}[/cyan]"
            else:
                target = f"[yellow]{escape(imported.target)}[/yellow]"

            branch.add(
                f"{target} "
                f"[dim](unconditional={imported.unconditional}, "
                f"conditional={imported.conditional})[/dim]"
            )

    with output.open("w", encoding="utf-8") as stream:
        console = Console(
            file=stream,
            force_terminal=False,
            color_system=None,
            width=160,
        )
        console.print(tree)


@beartype
def write_graph(
    graph: igraph.Graph,
    output: Path,
) -> None:
    dot = Digraph(name="imports", engine="dot", format="png")
    dot.attr("graph", overlap="false", rankdir="LR")
    dot.attr("node", fontname="DejaVu Sans")
    dot.attr("edge", fontname="DejaVu Sans")

    for vertex in graph.vs:
        dot.node(
            name=f"v{vertex.index}",
            label=vertex["label"],
            shape=("box" if vertex["internal"] else "ellipse"),
        )

    for edge in graph.es:
        unconditional = edge["unconditional"]
        conditional = edge["conditional"]
        weight = edge["weight"]

        dot.edge(
            tail_name=f"v{edge.source}",
            head_name=f"v{edge.target}",
            label=(f"U: {unconditional}, C: {conditional}"),
            weight=str(weight),
            penwidth=(f"{1.0 + math.log2(weight + 1):.2f}"),
        )

    dot.render(
        outfile=output,
        cleanup=True,
    )


@beartype
def main() -> None:
    parser = argparse.ArgumentParser(
        description=("Analyze syntax-level imports between Python packages.")
    )
    parser.add_argument(
        "target_directory",
        type=Path,
        help=("Git worktree directory recursively containing pyproject.toml files."),
    )
    arguments = parser.parse_args()

    target = arguments.target_directory.resolve()
    if not target.is_dir():
        raise ValueError(
            f"Target directory does not exist or is not a directory: {target}"
        )

    packages = discover_packages(target)
    counts = collect_import_counts(packages)
    graph = build_graph(packages, counts)

    write_tree(
        packages,
        counts,
        Path("/tmp/imports.txt"),
    )
    write_graph(
        graph,
        Path("/tmp/imports.dot.png"),
    )


if __name__ == "__main__":
    main()
