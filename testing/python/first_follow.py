#!/usr/bin/env python
# Credit: https://medium.com/100-days-of-algorithms/day-93-first-follow-cfe283998e3e

from pprint import pprint

def first_and_follow(grammar):
    # first & follow sets, epsilon-productions
    first = {i: set() for i in grammar.nonterminals}
    first.update((i, {i}) for i in grammar.terminals)
    follow = {i: set() for i in grammar.nonterminals}
    epsilon = set()

    while True:
        updated = False

        for nt, expression in grammar.rules:
            # FIRST set w.r.t epsilon-productions
            for symbol in expression:
                updated |= union(first[nt], first[symbol])
                if symbol not in epsilon:
                    break
            else:
                updated |= union(epsilon, {nt})

            # FOLLOW set w.r.t epsilon-productions
            aux = follow[nt]
            for symbol in reversed(expression):
                if symbol in follow:
                    updated |= union(follow[symbol], aux)
                if symbol in epsilon:
                    aux = aux.union(first[symbol])
                else:
                    aux = first[symbol]

        if not updated:
            return first, follow, epsilon

def union(first, begins):
    n = len(first)
    first |= begins
    return len(first) != n

class Grammar:
    def __init__(self, *rules):
        self.rules = tuple(self._parse(rule) for rule in rules)
        print("Creating grammar from rules:")
        pprint(self.rules)

    def _parse(self, rule):
        return tuple(rule.replace(' ', '').split('::='))

    def __getitem__(self, nonterminal):
        yield from [rule for rule in self.rules if rule[0] == nonterminal]

    @staticmethod
    def is_nonterminal(symbol):
        return symbol.isalpha() and symbol.isupper()

    @property
    def nonterminals(self):
        return set(nt for nt, _ in self.rules)

    @property
    def terminals(self):
        return set(
            symbol
            for _, expression in self.rules
            for symbol in expression
            if not self.is_nonterminal(symbol)
        )

# https://lambda.uta.edu/cse5317/notes/node15.html
# 1)   E ::= T E' $
# 2)   E' ::= + T E'
# 3)        | - T E'
# 4)        |
# 5)   T ::= F T'
# 6)   T' ::= * F T'
# 7)        | / F T'
# 8)        |
# 9)   F ::= num
# 10)      | id

# Change `E'` to `Z` and `T'` `X`
# Num -> `1`, `id` -> `2`

first, follow, epsilon = first_and_follow(Grammar(
    '^ ::= E $',
    'E ::= TZ',
    'Z ::= +TZ',
    'Z ::= -TZ',
    'Z ::= ',
    'T ::= FX',
    'X ::= *FX',
    'X ::= /FX',
    'F ::= 1',
    'F ::= 2'
))

print("First set")
pprint(first)

print("Follow set")
pprint(follow)

print("Nullable terminals")
pprint(epsilon)

#    FIRST    FOLLOW
# E  {num,id} {$}
# E' {+,-}    {$}
# T  {num,id} {+,-,$}
# T' {*,/}    {+,-,$}
# F  {num,id} {+,-,*,/,$}
