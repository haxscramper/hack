#!/usr/bin/env python


class BaseAnalyzer:
    def __init__(self, bd):
        self.bd = bd
        self.tables = []

    def add_table(self, table, fields):
        fs = ", ".join(fields)
        sql = f"create table if not exists {table}(language, {fs})"
        self.bd.execute(sql)
        self.tables.append((table, fields))

    def export_metrics(self, analyzer):
        for table in self.tables:
            for field in table[1]:
                analyzer.define_metric(table[0], field)
