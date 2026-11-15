#!/usr/bin/env python

import argparse
import ast
import math
import re
from dataclasses import dataclass
from enum import Enum
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
    names: tuple[str, ...] = ()


class CycleKind(Enum):
    NONE = "none"
    FILE = "file"
    PACKAGE = "package"


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

    def record(
        self,
        module: str,
        names: tuple[str, ...] = (),
    ) -> None:
        if module:
            self.imports.append(
                ImportUse(
                    module=module,
                    conditional=0 < self.conditional_depth,
                    names=names,
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
        names = tuple(alias.name for alias in node.names)

        if node.level:
            self.record(self.resolve_relative(node), names)
        elif node.module:
            self.record(node.module, names)

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
def resolve_import_source(
    module: str,
    package: Package,
    sources: dict[tuple[str, str], list[PackageSource]],
    importing_source: PackageSource,
    allow_prefix: bool,
) -> PackageSource | None:
    parts = module.split(".")

    while parts:
        candidate = ".".join(parts)
        matches = sources.get((package.name, candidate), [])

        if 1 < len(matches):
            paths = ", ".join(str(match.path) for match in matches)
            raise ValueError(
                f"Cannot resolve import {module!r} in {importing_source.path}: "
                f"module {candidate!r} has multiple sources in package "
                f"{package.name!r}: {paths}"
            )

        if matches:
            return matches[0]

        if not allow_prefix:
            return None

        parts.pop()

    return None


@beartype
def collect_file_import_counts(
    packages: tuple[Package, ...],
    ignore_external: bool,
) -> tuple[ImportCount, ...]:
    module_owners: dict[str, Package] = {}
    sources: dict[tuple[str, str], list[PackageSource]] = {}

    for package in packages:
        for imported_root in package.module_roots:
            module_owners[imported_root] = package

        for source in package.sources:
            sources.setdefault((package.name, source.module), []).append(source)

    counts: dict[tuple[str, str, bool], list[int]] = {}

    for package in packages:
        for source in package.sources:
            for imported in parse_imports(source):
                imported_root = imported.module.split(".")[0]
                owner = module_owners.get(imported_root)
                targets: set[str] = set()

                if owner is None:
                    if ignore_external:
                        continue

                    targets.add(imported_root)
                else:
                    base_source = resolve_import_source(
                        imported.module,
                        owner,
                        sources,
                        source,
                        allow_prefix=True,
                    )

                    if imported.names:
                        for name in imported.names:
                            nested_source = None

                            if name != "*":
                                nested_source = resolve_import_source(
                                    f"{imported.module}.{name}",
                                    owner,
                                    sources,
                                    source,
                                    allow_prefix=False,
                                )

                            resolved = nested_source or base_source

                            if resolved is None:
                                raise ValueError(
                                    f"Cannot map import "
                                    f"'from {imported.module} import {name}' "
                                    f"in {source.path} to a file: package "
                                    f"{owner.name!r} owns the import root, but "
                                    f"no matching module or package initializer "
                                    f"was discovered"
                                )

                            targets.add(str(resolved.path))
                    elif base_source is not None:
                        targets.add(str(base_source.path))
                    else:
                        raise ValueError(
                            f"Cannot map import {imported.module!r} in "
                            f"{source.path} to a file: package {owner.name!r} "
                            f"owns the import root, but no matching module "
                            f"or package initializer was discovered"
                        )

                for target in targets:
                    key = (
                        str(source.path),
                        target,
                        owner is not None,
                    )
                    values = counts.setdefault(key, [0, 0])
                    values[1 if imported.conditional else 0] += 1

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
def cyclic_components(graph: igraph.Graph) -> dict[int, int]:
    """Map vertices in directed cycles to their strongly connected component."""
    result: dict[int, int] = {}

    for component_index, component in enumerate(
        graph.connected_components(mode="strong")
    ):
        if 1 < len(component):
            for vertex_index in component:
                result[vertex_index] = component_index
        else:
            vertex_index = component[0]
            if graph.are_adjacent(vertex_index, vertex_index):
                result[vertex_index] = component_index

    return result


@beartype
def mark_cycles(graph: igraph.Graph, file_level: bool) -> None:
    package_names = sorted(
        {vertex["package"] for vertex in graph.vs if vertex["internal"]}
    )
    package_indices = {name: index for index, name in enumerate(package_names)}

    package_graph = igraph.Graph(directed=True)
    package_graph.add_vertices(len(package_names))

    package_edges: set[tuple[int, int]] = set()

    for edge in graph.es:
        source = graph.vs[edge.source]
        target = graph.vs[edge.target]

        if not source["internal"] or not target["internal"]:
            continue

        if source["package"] != target["package"]:
            package_edges.add(
                (
                    package_indices[source["package"]],
                    package_indices[target["package"]],
                )
            )

    package_graph.add_edges(sorted(package_edges))
    package_components = cyclic_components(package_graph)
    file_components = cyclic_components(graph) if file_level else {}

    for vertex in graph.vs:
        cycle = CycleKind.NONE

        if vertex["internal"]:
            package_index = package_indices[vertex["package"]]

            if package_index in package_components:
                cycle = CycleKind.PACKAGE

            if vertex.index in file_components:
                cycle = CycleKind.FILE

        vertex["cycle"] = cycle

    for edge in graph.es:
        cycle = CycleKind.NONE
        source = graph.vs[edge.source]
        target = graph.vs[edge.target]

        if source["internal"] and target["internal"]:
            source_package = package_indices[source["package"]]
            target_package = package_indices[target["package"]]

            if (
                source_package != target_package
                and source_package in package_components
                and package_components.get(source_package)
                == package_components.get(target_package)
            ):
                cycle = CycleKind.PACKAGE

            if edge.source in file_components and file_components.get(
                edge.source
            ) == file_components.get(edge.target):
                cycle = CycleKind.FILE

        edge["cycle"] = cycle


@beartype
def build_graph(
    packages: tuple[Package, ...],
    counts: tuple[ImportCount, ...],
    file_level: bool = False,
) -> igraph.Graph:
    graph = igraph.Graph(directed=True)

    for package in sorted(packages, key=lambda value: value.name):
        if file_level:
            for source in sorted(package.sources, key=lambda value: str(value.path)):
                relative = source.path.relative_to(package.root)
                graph.add_vertex(
                    name=f"internal:{source.path}",
                    label=relative.as_posix(),
                    internal=True,
                    package=package.name,
                    directory=relative.parent.parts,
                    filename=relative.name,
                )
        else:
            graph.add_vertex(
                name=f"internal:{package.name}",
                label=package.name,
                internal=True,
                package=package.name,
                directory=(),
                filename="",
            )

    external_names = sorted(
        {count.target for count in counts if not count.target_internal}
    )

    for name in external_names:
        graph.add_vertex(
            name=f"external:{name}",
            label=name,
            internal=False,
            package="",
            directory=(),
            filename="",
        )

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

    mark_cycles(graph, file_level)
    return graph


@beartype
def cycle_color(cycle: CycleKind) -> str:
    match cycle:
        case CycleKind.FILE:
            return "red"
        case CycleKind.PACKAGE:
            return "orange"
        case CycleKind.NONE:
            return "black"

    raise ValueError(f"Unsupported dependency cycle kind: {cycle!r}")


@beartype
def add_tree_imports(
    branch: Tree,
    graph: igraph.Graph,
    source_index: int,
) -> None:
    edges = sorted(
        graph.es.select(_source=source_index),
        key=lambda edge: (
            not graph.vs[edge.target]["internal"],
            graph.vs[edge.target]["label"],
        ),
    )

    if not edges:
        branch.add("[dim](no imports)[/dim]")
        return

    for edge in edges:
        target = graph.vs[edge.target]
        label = target["label"]

        if graph["file_level"] and target["internal"]:
            label = f"{target['package']}/{label}"

        match edge["cycle"]:
            case CycleKind.FILE:
                color = "red"
                annotation = ", file cycle"
            case CycleKind.PACKAGE:
                color = "dark_orange"
                annotation = ", package cycle"
            case CycleKind.NONE:
                color = "cyan" if target["internal"] else "yellow"
                annotation = ""

        branch.add(
            f"[{color}]{escape(label)}[/{color}] "
            f"[dim](unconditional={edge['unconditional']}, "
            f"conditional={edge['conditional']}{annotation})[/dim]"
        )


@beartype
def write_tree(
    graph: igraph.Graph,
    output: Path,
) -> None:
    tree = Tree("[bold]Imports[/bold]")
    internal_vertices = [vertex for vertex in graph.vs if vertex["internal"]]

    if graph["file_level"]:
        package_names = sorted({vertex["package"] for vertex in internal_vertices})

        for package_name in package_names:
            package_branch = tree.add(f"[bold cyan]{escape(package_name)}[/bold cyan]")

            for vertex in sorted(
                (
                    vertex
                    for vertex in internal_vertices
                    if vertex["package"] == package_name
                ),
                key=lambda value: value["label"],
            ):
                branch = package_branch.add(escape(vertex["label"]))
                add_tree_imports(branch, graph, vertex.index)
    else:
        for vertex in sorted(
            internal_vertices,
            key=lambda value: value["label"],
        ):
            branch = tree.add(f"[bold cyan]{escape(vertex['label'])}[/bold cyan]")
            add_tree_imports(branch, graph, vertex.index)

    with output.open("w") as stream:
        console = Console(
            file=stream,
            force_terminal=False,
            color_system=None,
            width=160,
        )
        console.print(tree)


@beartype
def add_graph_vertex(
    dot: Digraph,
    vertex: igraph.Vertex,
    directory_clusters: bool,
) -> None:
    label = vertex["label"]

    if directory_clusters and vertex["internal"]:
        label = vertex["filename"]

    color = cycle_color(vertex["cycle"])
    attributes = {
        "color": color,
        "shape": "box" if vertex["internal"] else "ellipse",
    }

    if vertex["cycle"] != CycleKind.NONE:
        attributes["penwidth"] = "2.5"

    dot.node(
        name=f"v{vertex.index}",
        label=label,
        **attributes,
    )


@beartype
def _add_file_level_vertices(
    dot: Digraph,
    graph: igraph.Graph,
    directory_clusters: bool,
) -> None:
    package_names = sorted(
        {vertex["package"] for vertex in graph.vs if vertex["internal"]}
    )

    for package_index, package_name in enumerate(package_names):
        package_vertices = [
            vertex
            for vertex in graph.vs
            if vertex["internal"] and vertex["package"] == package_name
        ]
        package_dot = Digraph(name=f"cluster_package_{package_index}")
        package_dot.attr(label=package_name)

        if directory_clusters:
            directories: set[tuple[str, ...]] = {()}

            for vertex in package_vertices:
                directory = tuple(vertex["directory"])
                for length in range(1, len(directory) + 1):
                    directories.add(directory[:length])

            directory_graphs: dict[tuple[str, ...], Digraph] = {(): package_dot}

            for directory_index, directory in enumerate(
                sorted(
                    directories - {()},
                    key=lambda value: (len(value), value),
                )
            ):
                directory_dot = Digraph(
                    name=f"cluster_directory_{package_index}_{directory_index}"
                )
                directory_dot.attr(label=directory[-1])
                directory_graphs[directory] = directory_dot

            for vertex in package_vertices:
                add_graph_vertex(
                    directory_graphs[tuple(vertex["directory"])],
                    vertex,
                    directory_clusters=True,
                )

            for directory in sorted(
                directories - {()},
                key=lambda value: (-len(value), value),
            ):
                directory_graphs[directory[:-1]].subgraph(directory_graphs[directory])
        else:
            for vertex in package_vertices:
                add_graph_vertex(
                    package_dot,
                    vertex,
                    directory_clusters=False,
                )

        dot.subgraph(package_dot)

    for vertex in graph.vs:
        if not vertex["internal"]:
            add_graph_vertex(
                dot,
                vertex,
                directory_clusters=False,
            )


@beartype
def _add_non_file_level_vertices(
    dot: Digraph,
    graph: igraph.Graph,
) -> None:
    for vertex in graph.vs:
        add_graph_vertex(
            dot,
            vertex,
            directory_clusters=False,
        )


@beartype
def write_graph(
    graph: igraph.Graph,
    output: Path,
    directory_clusters: bool = False,
) -> None:
    dot = Digraph(name="imports", engine="fdp", format="png")
    dot.attr(
        "graph",
        overlap="false",
        rankdir="LR",
        compound="true",
        newrank="true",
    )

    dot.attr("node", fontname="DejaVu Sans")
    dot.attr("edge", fontname="DejaVu Sans")

    if graph["file_level"]:
        _add_file_level_vertices(
            dot,
            graph,
            directory_clusters=directory_clusters,
        )
    else:
        _add_non_file_level_vertices(dot, graph)

    for edge in graph.es:
        weight = edge["weight"]

        dot.edge(
            tail_name=f"v{edge.source}",
            head_name=f"v{edge.target}",
            label=f"U: {edge['unconditional']}, C: {edge['conditional']}",
            color=cycle_color(edge["cycle"]),
            weight=str(weight),
            penwidth=f"{1.0 + math.log2(weight + 1):.2f}",
        )

    dot.render(outfile=output, cleanup=True)


@beartype
def parse_bool(value: str) -> bool:
    match value.casefold():
        case "true":
            return True
        case "false":
            return False
        case _:
            raise argparse.ArgumentTypeError(f"Expected True or False, got {value!r}")


@beartype
def main() -> None:
    parser = argparse.ArgumentParser(
        description="Analyze syntax-level imports between Python packages or files."
    )
    parser.add_argument(
        "target_directory",
        type=Path,
        help="Git worktree directory recursively containing pyproject.toml files.",
    )
    parser.add_argument(
        "--ignore-external",
        type=parse_bool,
        default=False,
        metavar="{True,False}",
        help="Exclude dependencies whose import roots belong to no discovered package.",
    )
    parser.add_argument(
        "--file-level",
        type=parse_bool,
        default=False,
        metavar="{True,False}",
        help="Show individual files grouped into package clusters.",
    )
    parser.add_argument(
        "--directory-clusters",
        type=parse_bool,
        default=False,
        metavar="{True,False}",
        help="Nest directory clusters inside package clusters; requires --file-level=True.",
    )
    arguments = parser.parse_args()

    if arguments.directory_clusters and not arguments.file_level:
        parser.error(
            "--directory-clusters=True requires --file-level=True "
            "because directory clusters group individual source files"
        )

    target = arguments.target_directory.resolve()
    if not target.is_dir():
        raise ValueError(
            f"Target directory does not exist or is not a directory: {target}"
        )

    packages = discover_packages(target)

    if arguments.file_level:
        counts = collect_file_import_counts(
            packages,
            ignore_external=arguments.ignore_external,
        )
    else:
        counts = collect_import_counts(packages)

        if arguments.ignore_external:
            counts = tuple(count for count in counts if count.target_internal)

    graph = build_graph(
        packages,
        counts,
        file_level=arguments.file_level,
    )
    graph["file_level"] = arguments.file_level

    write_tree(
        graph,
        Path("/tmp/imports.txt"),
    )
    write_graph(
        graph,
        Path("/tmp/imports.dot.png"),
        directory_clusters=arguments.directory_clusters,
    )


if __name__ == "__main__":
    main()

if __name__ == "__main__":
    main()
