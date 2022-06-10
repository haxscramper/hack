#!/usr/bin/env python

from common import *
from BaseAnalyzer import *
import csv


class BenchGame(BaseAnalyzer):
    def __init__(self, bd):
        super(BenchGame, self).__init__(bd)

    def fetch(self):
        with open("bench.csv") as c:
            for row in csv.DictReader(c):
                lang = normalize_lang(row["lang"])
                bench = row["name"]
                self.add_table(bench, ["speed"])
                sql = f"insert into {bench} (language, speed) values (?, ?)"
                self.bd.execute(sql, (lang, float(row["cpu(s)"])))
