#!/usr/bin/env python


class CompareBase:
    def __init__(self, bd):
        self.bd = bd

    def define_metric(self, table, field):
        def method_impl(self, lang: str):
            sql = f"select {field} from {table} where language = '{lang}'"
            for row in self.bd.execute(sql):
                return row[0]

            return 0

        name = f"{table}_{field}"
        setattr(CompareBase, name, method_impl)


class Compare(CompareBase):
    def __init__(self, bd):
        super(Compare, self).__init__(bd)

    def lang_value(self, lang: str) -> float:
        return self.user1_users(lang) * 0.3 + self.spectralnorm_speed(lang) * 1.2
