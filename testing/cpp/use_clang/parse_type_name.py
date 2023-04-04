#!/usr/bin/env python

import sys
import glob
import os
import clang.cindex
from generate_type_info import clangCursorTreeRepr

def find_best_matching_libclang() -> str:
    search_paths = [
        "/usr/lib/llvm-*/lib",
        "/usr/lib/x86_64-linux-gnu",
        "/usr/local/lib",
        "/usr/lib",
        "/usr/local/lib64",
        "/usr/lib64",
    ]

    # Glob patterns for libclang
    libclang_globs = [
        "libclang.so.*",
        "libclang.so",
    ]

    best_libclang = None
    best_version = -1

    for path in search_paths:
        for libclang_glob in libclang_globs:
            for libclang_file in glob.glob(os.path.join(path, libclang_glob)):
                # Extract the version number
                version_str = libclang_file.split(".so")[-1]
                if version_str:
                    version = int(version_str.split(".")[-1])
                else:
                    version = 0

                if version > best_version:
                    best_version = version
                    best_libclang = libclang_file

    return best_libclang

# Ensure libclang can be found
clang_so = find_best_matching_libclang()
print("Using", clang_so)
clang.cindex.Config.set_library_file(clang_so)

def simplify_function_signature(code: str) -> str:
    index = clang.cindex.Index.create()
    # Parse the code as a C++ translation unit
    tu = index.parse(
        "temp.cpp",
        args=["-std=c++17", "-fsyntax-only"],
        unsaved_files=[("temp.cpp", code)],
        options=0,
    )

    def simplify_node(node):
        if node.kind == clang.cindex.CursorKind.TYPE_REF:
            # Remove unnecessary template arguments
            return node.spelling.split("<")[0]
        elif node.kind == clang.cindex.CursorKind.PARM_DECL:
            # Simplify the parameter type and add the parameter name
            return (
                simplify_node(node.type.get_declaration())
                + " "
                + node.spelling
            )
        else:
            # For other nodes, just return their spelling
            return node.spelling

    # Find the first function declaration in the AST
    func_decl = None
    print(clangCursorTreeRepr(tu.cursor))
    for cursor in tu.cursor.walk_preorder():
        if cursor.kind == clang.cindex.CursorKind.FUNCTION_DECL:
            func_decl = cursor
            break

    if func_decl is None:
        print("No function declaration found.")
        return ""

    # Simplify the function signature
    simplified_signature = []
    for child in func_decl.get_children():
        simplified_signature.append(simplify_node(child))

    return " ".join(simplified_signature)


if __name__ == "__main__":
    code = "std::vector<int> a(){}"
    simplified = simplify_function_signature(code)
    print(simplified)
