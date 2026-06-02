from header_source_reorder.main import *
from header_source_reorder.main import _make_parser

PARSER = _make_parser()


@beartype
def _init_repo(tmp_path: Path) -> Path:
    root = tmp_path / "repo"
    root.mkdir(parents=True, exist_ok=True)
    subprocess.run(["git", "-C", str(root), "init"],
                   check=True,
                   capture_output=True)
    return root


@beartype
def _edge_exists(repo_graph: RepositoryGraph, src_id: str,
                 dst_id: str) -> bool:
    src = repo_graph.id_to_index[src_id]
    dst = repo_graph.id_to_index[dst_id]
    return repo_graph.graph.are_adjacent(src, dst)


@beartype
def _first_block_by_key(parsed: ParsedFile, key: str) -> CodeBlock:
    for block in parsed.blocks:
        if block.key == key:
            return block
    raise ValueError(f"Missing key {key} in {parsed.path}")


@beartype
def _parse_and_build(root: Path) -> Tuple[RepositoryParse, RepositoryGraph]:
    repo = parse_repository(root)
    graph = build_repository_graph(repo)
    return repo, graph


def test_templates_and_specialization_order(tmp_path: Path) -> None:
    root = _init_repo(tmp_path)
    header = root / "a.hpp"
    source = root / "a.cpp"
    header.write_text(
        """
#pragma once

template <typename T>
T add(T a, T b);

template <>
int add<int>(int a, int b);

int plain();
        """,
        encoding="utf-8",
    )
    source.write_text(
        """
#include "a.hpp"

int plain() { return 1; }

template <typename T>
T add(T a, T b) { return a + b; }

template <>
int add<int>(int a, int b) { return a - b; }
        """,
        encoding="utf-8",
    )
    repo, graph = _parse_and_build(root)
    visualize_repository_graph(
        repo,
        graph,
        Path(
            "/tmp/haxscramper-hack/test_templates_and_specialization_order.png"
        ),
    )
    parsed_source = next(f for f in repo.files if f.path == source)
    assert 4 == len(parsed_source.blocks)
    assert rebuild_identity(parsed_source) == parsed_source.text
    assert len(graph.blocks) == sum(len(f.blocks) for f in repo.files)

    src_add = _first_block_by_key(parsed_source, "add")
    src_add_spec = _first_block_by_key(parsed_source, "add<int>")
    src_plain = _first_block_by_key(parsed_source, "plain")
    assert src_add.id in graph.id_to_index
    assert src_add_spec.id in graph.id_to_index
    assert src_plain.id in graph.id_to_index
    assert _edge_exists(graph, src_add.id, src_add_spec.id)
    assert _edge_exists(graph, src_add_spec.id, src_plain.id)

    sort_repository(root, None)
    out = source.read_text(encoding="utf-8")
    assert out.find("T add(T a, T b)") < out.find("add<int>(int a, int b)")
    assert out.find("add<int>(int a, int b)") < out.find("int plain()")


def test_anonymous_namespace_name_collision(tmp_path: Path) -> None:
    root = _init_repo(tmp_path)
    header = root / "b.hpp"
    source = root / "b.cpp"
    header.write_text(
        """
#pragma once
int ext_a();
int ext_b();
""",
        encoding="utf-8",
    )
    source.write_text(
        """
#include "b.hpp"

namespace {
    int helper() { return 1; }
}

namespace {
    int helper() { return 2; }
}

int ext_b() { return helper(); }
int ext_a() { return helper(); }
""",
        encoding="utf-8",
    )

    repo, graph = _parse_and_build(root)
    visualize_repository_graph(
        repo,
        graph,
        Path(
            "/tmp/haxscramper-hack/test_anonymous_namespace_name_collision.png"
        ),
    )
    parsed_source = next(f for f in repo.files if f.path == source)
    assert rebuild_identity(parsed_source) == parsed_source.text
    helper_keys = [b.key for b in parsed_source.blocks if "helper" in b.key]
    assert 2 == len(helper_keys)
    assert helper_keys[0] != helper_keys[1]

    src_ext_a = _first_block_by_key(parsed_source, "ext_a")
    src_ext_b = _first_block_by_key(parsed_source, "ext_b")
    assert _edge_exists(graph, src_ext_a.id, src_ext_b.id)

    sort_repository(root, None)
    out = source.read_text(encoding="utf-8")
    assert out.find("int ext_a()") < out.find("int ext_b()")
    assert out.find("int helper() { return 1; }") < out.find(
        "int helper() { return 2; }")


def test_multi_header_multi_source_graph_edges(tmp_path: Path) -> None:
    root = _init_repo(tmp_path)
    h1 = root / "h1.hpp"
    h2 = root / "h2.hxx"
    s1 = root / "s1.cpp"
    s2 = root / "s2.cpp"

    h1.write_text(
        """
#pragma once
int A();
int B();
        """,
        encoding="utf-8",
    )

    source_parsed = parse_cpp_file(h1, PARSER)
    assert source_parsed.blocks[0].key == "__includes__"
    assert len(source_parsed.blocks) == 3

    h2.write_text(
        """
#pragma once
int C();
        """,
        encoding="utf-8",
    )
    s1.write_text(
        """
#include "h1.hpp"
#include "h2.hxx"
int C() { return 3; }
int A() { return 1; }
        """,
        encoding="utf-8",
    )
    s2.write_text(
        """
#include "h1.hpp"
int B() { return 2; }
        """,
        encoding="utf-8",
    )

    repo, graph = _parse_and_build(root)
    visualize_repository_graph(
        repo,
        graph,
        Path(
            "/tmp/haxscramper-hack/test_multi_header_multi_source_graph_edges.png"
        ),
    )
    parsed_s1 = next(f for f in repo.files if f.path == s1)
    parsed_s2 = next(f for f in repo.files if f.path == s2)
    assert rebuild_identity(parsed_s1) == parsed_s1.text
    assert rebuild_identity(parsed_s2) == parsed_s2.text

    s1_a = _first_block_by_key(parsed_s1, "A")
    s2_b = _first_block_by_key(parsed_s2, "B")
    assert _edge_exists(graph, s1_a.id, s2_b.id)
    assert len(graph.blocks) == sum(len(f.blocks) for f in repo.files)

    sort_repository(root, None)
    out_s1 = s1.read_text(encoding="utf-8")
    assert out_s1.find("int A()") < out_s1.find("int C()")


def test_clang_format_off_region_is_not_reordered(tmp_path: Path) -> None:
    root = _init_repo(tmp_path)
    header = root / "c.h"
    source = root / "c.cc"
    header.write_text(
        """
#pragma once
int f();
int g();
        """,
        encoding="utf-8",
    )
    original = """
#include "c.h"

/* clang-format off */
int g() { return 2; }
int f() { return 1; }
/* clang-format on */
    """
    source.write_text(original, encoding="utf-8")

    repo, graph = _parse_and_build(root)
    visualize_repository_graph(
        repo,
        graph,
        Path(
            "/tmp/haxscramper-hack/test_clang_format_off_region_is_not_reordered.png"
        ),
    )
    parsed_source = next(f for f in repo.files if f.path == source)
    assert rebuild_identity(parsed_source) == parsed_source.text

    sort_repository(root, None)
    out = source.read_text(encoding="utf-8")
    assert out == original
