#!/usr/bin/env python

import sqlite3
import importlib
import logging
import os
from common import *


def str_to_class(module_name, class_name):
    # Загрузить класс из одноименного модуля
    class_ = None
    try:
        module_ = importlib.import_module(module_name)
        try:
            class_ = getattr(module_, class_name)
        except AttributeError:
            logging.error(f"Class {class_name} does not exist")
    except ImportError:
        logging.error(f"Module {module_name} does not exist")
    return class_ or None


def load_indexer(class_name: str):
    # Загрузить класс для индексирования входных данных
    return str_to_class(class_name, class_name)


def load_analyzer(class_name: str):
    # Загрузить анализатор для сравнения языков
    return str_to_class(class_name, class_name)


if __name__ == "__main__":
    # Подключится к базе данных
    os.remove("main.sqlite")
    con = sqlite3.connect("main.sqlite")
    indexers = []
    # Загрузить индексаторы
    for name in ["BenchGame", "UserDefined1", "UserDefined2"]:
        indexer = load_indexer(name)
        if indexer != None:
            indexers.append(indexer(con))

    # Последовательно загрузить входные данные для всех индексаторов
    for it in indexers:
        it.fetch()

    # Загрузить анализатор
    anl = load_analyzer("Compare")(con)

    # Добавить все известные метрики в анализатор
    for it in indexers:
        it.export_metrics(anl)

    metrics = [
        "rust",
        "ghc",
        "swift",
        "dart",
        "gpp",
        "julia",
        "ruby",
        "go",
        "java",
    ]

    # Рассчитать и отсортировать итоговые значения для каждого языка
    for idx in range(0, len(metrics)):
        lang = normalize_lang(metrics[idx])
        metrics[idx] = (lang, anl.lang_value(lang))

    metrics = sorted(metrics, key=lambda tup: tup[1], reverse=True)
    for row in metrics:
        print(f"| {row[0]:^10} | {row[1]:^10.4f} |")

    con.close()
