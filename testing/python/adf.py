#!/usr/bin/env python

lit = """
    = {QChar(U'A'),
       QChar(U'Ɐ'),
       QChar(U'𝔄'),
       QChar(U'𝕬'),
       QChar(U'𝔸'),
       QChar(U'𝐀'),
       QChar(U'𝐴'),
       QChar(U'𝑨'),
       QChar(U'𝒜'),
       QChar(U'𝓐'),
       QChar(U'𝖠'),
       QChar(U'𝗔'),
       QChar(U'𝘈'),
       QChar(U'𝘼'),
       QChar(U'𝙰')},
"""

print(lit.encode('utf-8'))
