#!/usr/bin/env python

def get_abc(bias, key):
    a = ord('a')
    abc = ''.join([chr(i) for i in range(a,a+26)])
    for sym in key:
        abc = abc.replace(sym, '')
    abc = key + abc
    for i in range(bias):
        abc = abc[-1] + abc[:-1]
    return abc


inp = "input text"
bias = 2
key = "key"

abc = get_abc(bias, key)
message1 = ''
for sym in inp:
    if sym.isalpha():
        message1 += abc[(ord(sym) - 97 + bias) % len(abc)]
    else:
        message1 += sym
print(message1)



message2 = ''
c = 0
for sym in inp:
    if sym.isalpha():
        message2 += chr(((ord(sym)-97) + (ord(key[c])-97)) % 26 + 97)
        c = (c + 1) % len(key)
    else:
        message2 += sym
print(message2)

print(abc)
