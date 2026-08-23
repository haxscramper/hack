from header_source_reorder.main import *
import logging
from header_source_reorder.main import _make_parser
from header_source_reorder.types import BlockType, QualType

logging.getLogger("main.parser").setLevel(logging.ERROR)

PARSER = _make_parser()


def dump_node(node, source: bytes, indent: int = 0) -> str:
    lines = [
        '  ' * indent + f'{node.type} ' +
        ('' if node.children else
         f'{source[node.start_byte:node.end_byte].decode()!r}')
    ]
    for child in node.children:
        lines.append(dump_node(child, source, indent + 1))
    return '\n'.join(lines)


def _get_test_parse(text: str, suffix: str = ".cpp") -> tuple[ParsedFile, str]:
    file = Path(f"/tmp/file{suffix}")
    file.write_text(text.strip("\n") + "\n")
    file_result = parse_cpp_file(file, PARSER)
    debug = f"""
{dump_node(PARSER.parse(text.encode()).root_node, text.encode())}
{file_result.model_dump_json(indent=2)}
    """
    return file_result, debug


def test_source_one_function():
    parsed, debug = _get_test_parse("""
int main() {
    return 0;
}
""")
    assert len(parsed.blocks) == 1, debug
    assert [b.block_type
            for b in parsed.blocks] == [BlockType.DEFINITION], debug
    assert parsed.blocks[0].qualified_name is not None, debug
    assert parsed.blocks[0].qualified_name.name == "main", debug


def test_source_inline_namespace_function():
    parsed, debug = _get_test_parse("""
void other::space::function(int value) {
    (void)value;
}
""")
    assert len(parsed.blocks) == 1, debug
    assert [b.block_type
            for b in parsed.blocks] == [BlockType.DEFINITION], debug
    assert parsed.blocks[0].qualified_name is not None, debug
    assert parsed.blocks[0].qualified_name.name == "function", debug


def test_source_parent_namespace_declaration():
    parsed, debug = _get_test_parse("""
namespace test {
void other();
}
""")
    assert len(parsed.blocks) == 3, debug
    assert [b.block_type for b in parsed.blocks] == [
        BlockType.NAMESPACE_OPEN,
        BlockType.LOCAL_ENTRY,
        BlockType.NAMESPACE_CLOSE,
    ], debug
    assert parsed.blocks[1].qualified_name is None, debug


def test_source_nested_parent_namespace_declaration():
    parsed, debug = _get_test_parse("""
namespace test {
namespace random {
void whatever();
}
}
""")
    assert len(parsed.blocks) == 5, debug
    assert [b.block_type for b in parsed.blocks] == [
        BlockType.NAMESPACE_OPEN,
        BlockType.NAMESPACE_OPEN,
        BlockType.LOCAL_ENTRY,
        BlockType.NAMESPACE_CLOSE,
        BlockType.NAMESPACE_CLOSE,
    ], debug


def test_source_two_functions_nested_namespace():
    parsed, debug = _get_test_parse("""
namespace test {
int a() { return 1; }
int b() { return 2; }
}
""")
    assert len(parsed.blocks) == 4, debug
    assert [b.block_type for b in parsed.blocks] == [
        BlockType.NAMESPACE_OPEN,
        BlockType.DEFINITION,
        BlockType.DEFINITION,
        BlockType.NAMESPACE_CLOSE,
    ], debug
    assert parsed.blocks[1].qualified_name is not None, debug
    assert parsed.blocks[2].qualified_name is not None, debug


def test_header_enum_declaration():
    parsed, debug = _get_test_parse(
        """
enum Value {
    One,
    Two,
};
""",
        suffix=".hpp",
    )
    assert len(parsed.blocks) >= 1, debug
    assert parsed.blocks[0].block_type == BlockType.DECLARATION, debug
    assert parsed.blocks[0].qualified_name is not None, debug
    assert parsed.blocks[0].qualified_name.name == "Value", debug


def test_header_enum_class_declaration():
    parsed, debug = _get_test_parse(
        """
enum class Mode {
    A,
    B,
};
""",
        suffix=".hpp",
    )
    assert len(parsed.blocks) >= 1, debug
    assert parsed.blocks[0].block_type == BlockType.DECLARATION, debug
    assert parsed.blocks[0].qualified_name is not None, debug
    assert parsed.blocks[0].qualified_name.name == "Mode", debug


def test_header_union_declaration():
    parsed, debug = _get_test_parse(
        """
union Data {
    int i;
    float f;
    void reset();
};
""",
        suffix=".hpp",
    )
    assert 1 <= len(parsed.blocks), debug

    assert parsed.blocks[0].block_type == BlockType.DECLARATION, debug
    assert parsed.blocks[0].qualified_name is not None, debug
    assert parsed.blocks[0].qualified_name.name == "Data", debug


def test_header_struct_class_declaration():
    parsed, debug = _get_test_parse(
        """
struct S {
    int v;
    void set(int a);
};

class C {
public:
    int get() const;
};
""",
        suffix=".hpp",
    )
    assert 2 <= len(parsed.blocks), debug
    assert parsed.blocks[0].block_type == BlockType.DECLARATION, debug
    assert parsed.blocks[1].block_type == BlockType.DECLARATION, debug


def test_header_nested_enum_union_struct():
    parsed, debug = _get_test_parse(
        """
struct Outer {
    enum Kind { A, B };
    union Payload { int i; float f; };
    struct Node { int x; };
};
""",
        suffix=".hpp",
    )
    assert 1 <= len(parsed.blocks), debug
    idx = 0
    while idx < len(parsed.blocks):
        assert parsed.blocks[idx].block_type == BlockType.DECLARATION, debug
        idx += 1


def test_header_multi_nested_struct():
    parsed, debug = _get_test_parse(
        """
struct A {
    struct B {
        struct C {
            void run();
        };
    };
};
""",
        suffix=".hpp",
    )
    assert 1 <= len(parsed.blocks), debug
    assert parsed.blocks[0].block_type == BlockType.DECLARATION, debug


def test_clang_format_block_type():
    parsed, debug = _get_test_parse("""
/* clang-format off */
int one() {}
int two() {}
/* clang-format on */
""")
    assert len(parsed.blocks) == 1, debug
    assert parsed.blocks[0].block_type == BlockType.CLANG_FORMAT_BLOCK, debug
    assert parsed.blocks[0].qualified_name is None, debug


def test_local_entry_merge():
    parsed, debug = _get_test_parse("""
int x;
int y;
""")
    assert len(parsed.blocks) == 1, debug
    assert parsed.blocks[0].block_type == BlockType.LOCAL_ENTRY, debug


def test_qualified_type_normalization():
    q = QualType(
        name="  test  ",
        parent_namespaces=(QualType(name="  a "), ),
        parameters=(QualType(name=" const int & "), ),
    )
    assert q.name == "test"
    assert q.parent_namespaces[0].name == "a"
    assert q.parameters[0].name == "const int &"


def test_qualified_type_flatten_tuple():
    q = QualType(
        name="fn",
        parent_namespaces=(QualType(name="file.cpp"), QualType(name="Ns")),
        parameters=(QualType(name="int"), QualType(name="float")),
    )
    flat = q.to_flat_tuple()
    assert 0 < len(flat)
    assert flat[0] == "fn"


def test_qualified_type_signature_with_qualifiers():
    parsed, debug = _get_test_parse("""
void fn(const int& value, volatile float* ptr) {}
""")
    assert len(parsed.blocks) == 1, debug
    q = parsed.blocks[0].qualified_name
    assert q is not None, debug
    assert q.name == "fn", debug
    assert len(q.parameters) == 2, debug
    assert q.parameters[0].name == "int", debug
    assert q.parameters[1].name == "float", debug


def test_qualified_type_file_namespace_injected():
    parsed, debug = _get_test_parse(
        """
struct LocalType {};
""",
        suffix=".hpp",
    )
    q = parsed.blocks[0].qualified_name
    assert q is not None, debug
    assert 1 <= len(q.parent_namespaces), debug
    assert q.parent_namespaces[0].name == "file.hpp", debug
