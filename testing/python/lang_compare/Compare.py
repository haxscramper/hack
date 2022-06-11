#!/usr/bin/env python


class CompareBase:
    # Базовый класс для реализации частных решений для сравнения метрик для
    # различных языков
    def __init__(self, bd):
        self.bd = bd

    def define_metric(self, table, field):
        # Динамически добавить метод в класс. Имя метода формируется из
        # имени таблицы и строки как `table_field`
        def method_impl(self, lang: str):
            # Реализация нового метода, которая будет добавлена в класс
            sql = f"select {field} from {table} where language = '{lang}'"
            for row in self.bd.execute(sql):
                return row[0]

            return 0

        name = f"{table}_{field}"
        # Добавить новый метод в класс
        setattr(CompareBase, name, method_impl)


class Compare(CompareBase):
    # Частная реализация сравнительного анализа языков, использующая
    # метрику скорости работы языка на наборе бенчмарков
    def __init__(self, bd):
        super(Compare, self).__init__(bd)

    def lang_value(self, lang: str) -> float:
        speed = (
            self.spectralnorm_speed(lang)
            + self.binarytrees_speed(lang)
            + self.fasta_speed(lang)
            + self.mandelbrot_speed(lang)
            + self.nbody_speed(lang)
        )

        mem = (
            self.spectralnorm_mem(lang)
            + self.binarytrees_mem(lang)
            + self.fasta_mem(lang)
            + self.mandelbrot_mem(lang)
            + self.nbody_mem(lang)
        )

        result = 0

        if speed != 0:
            # Итоговое значение обратно пропорционально времени работы -
            # языки, требущие много времени для выполнения задачи получают
            # меньше значения
            p_mem = 1 / (mem / 10000)
            p_speed = 1 / speed
            result += p_speed * 2 + p_mem

        return result
