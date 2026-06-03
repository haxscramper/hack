from header_source_reorder.main import *
import logging
from header_source_reorder.main import _make_parser

logging.getLogger("main.parser").setLevel(logging.ERROR)

PARSER = _make_parser()


def _get_test_parse(text: str) -> ParsedFile:
    file = Path("/tmp/file.cpp")
    file.write_text(text + "\n")
    return parse_cpp_file(file, PARSER)


def test_trivial_file():
    parsed = _get_test_parse("")
    assert len(parsed.blocks) == 0


def test_single_include_file():
    parsed = _get_test_parse("#include <random-test.hpp>")
    assert len(parsed.blocks) == 1
    assert parsed.blocks[0].is_include_block == True


def test_leading_pragma():
    parsed = _get_test_parse("#pragma once")
    assert len(parsed.blocks) == 1


def test_include_and_source():
    parsed = _get_test_parse("""
#include <test>
int main() {}
    """)

    assert len(parsed.blocks) == 2
    assert parsed.blocks[0].is_include_block == True
    assert parsed.blocks[1].line_range == slice(0, 1)
    assert parsed.lines[parsed.blocks[0].line_range] == ["#include <test>"]
    assert parsed.blocks[1].line_range == slice(1, 2)
    assert parsed.lines[parsed.blocks[1].line_range] == ["int main() {}"]


def test_clang_format_ranges():
    parsed = _get_test_parse("""
/* clang-format off */
int one() {}
int two() {}
/* clang-format on */
    """)

    assert len(parsed.blocks) == 1
