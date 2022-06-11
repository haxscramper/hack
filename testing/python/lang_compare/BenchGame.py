#!/usr/bin/env python

from common import *
from BaseAnalyzer import *
import csv


class BenchGame(BaseAnalyzer):
    # Получение информации о скорости работы языков на наборе задач путем разбора метрик в формате csv
    def __init__(self, bd):
        super(BenchGame, self).__init__(bd)

    def fetch(self):
        # Прочитать файл bench.csv разобрать каждую строку последовательно
        # и создать необходимые таблицы и поля.
        with open("bench.csv") as c:
            for row in csv.DictReader(c):
                lang = normalize_lang(row["lang"])
                bench = row["name"]
                self.add_table(bench, ["speed", "mem"])
                sql = f"insert into {bench} (language, speed, mem) values (?, ?, ?)"
                self.bd.execute(
                    sql, (lang, float(row["cpu(s)"]), float(row["mem(KB)"]))
                )
