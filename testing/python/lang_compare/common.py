#!/usr/bin/env python


def normalize_lang(lang: str) -> str:
    # Нормализация названия языка - убирает различия между вариантам
    # написания, такими как `Cxx`, `Cpp`, `C++` и т.д. и переводит название
    # в нижний регистр.
    rename = {
        "c++": "cpp",
        "f#": "fsharp",
        "clang": "cpp",
        "gpp": "cpp",
        "g++": "cpp",
        "ghc": "haskell",
        "dartexe": "dart",
    }

    lang = lang.replace(" ", "").replace("_", "").lower()
    if lang in rename:
        lang = rename[lang]

    return lang
