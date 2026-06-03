from beartype.typing import Dict, List, Optional, Sequence, Set, Tuple

CPP_HEADERS: Set[str] = {
    ".h",
    ".hh",
    ".hpp",
    ".hxx",
    ".h++",
    ".ipp",
    ".tpp",
    ".inl",
    ".inc",
}
CPP_SOURCES: Set[str] = {
    ".c",
    ".cc",
    ".cpp",
    ".cxx",
    ".c++",
    ".cu",
}
CPP_ALL: Set[str] = set(CPP_HEADERS) | set(CPP_SOURCES)
CAPTURE_TYPES: Set[str] = {
    "function_definition",
    "declaration",
    "template_declaration",
    "type_definition",
    "using_declaration",
    "alias_declaration",
    "concept_definition",
    "linkage_specification",
}
