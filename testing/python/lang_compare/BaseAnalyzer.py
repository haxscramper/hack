#!/usr/bin/env python


class BaseAnalyzer:
    # Базовый класс для получения метрик из различных источников
    def __init__(self, bd):
        # Сохранить подключение к базе данных в поле
        self.bd = bd
        self.tables = []

    def add_table(self, table, fields):
        # Создать новую таблицу, используя имя и список дополнительных
        # полей. Поле с названием языка добавляется автоматически
        fs = ", ".join(fields)
        if (table, fields) not in self.tables:
            sql = f"create table {table}(language text, {fs})"
            print(sql)
            self.bd.execute(sql)
            self.tables.append((table, fields))

    def export_metrics(self, analyzer):
        # Проинформировать конечный анализатор о наличии таблиц и метрик
        for table in self.tables:
            print(table)
            for field in table[1]:
                analyzer.define_metric(table[0], field)
