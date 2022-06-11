#!/usr/bin/env python


def normalize_lang(lang: str) -> str:
    rename = {
        "c++": "cpp",
        "f#": "fsharp",
        "clang": "cpp",
        "g++": "cpp",
        "ghc": "haskell",
        "dartexe": "dart",
    }

    lang = lang.replace(" ", "").replace("_", "").lower()
    if lang in rename:
        lang = rename[lang]

    return lang
