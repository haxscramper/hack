#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from  nltk.parse.corenlp  import CoreNLPParser

parser = CoreNLPParser()
parse = next(parser.raw_parse("I put the book in the box on the table."))
parse.pretty_print()