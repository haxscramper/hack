#!/usr/bin/env python

import math

def hash_func(a,b,c):
    return int(math.exp(a)*10**15) + int(math.log(b,c)*10**15) + int(math.log(c,b)*10**15)

hash_func(54, 76, 45)

hashes = set()
for a_i in range(1000):
    for b_i in range(1000):
        for c_i in range(1000):
            hsh = hash_func(a_i, b_i, c_i)
            if hash_func(a_i, b_i, c_i) in hashes:
                print("FAIL")
                exit(0)
            hashes.add(hsh)

a = 1
b = 2
c = 3

print(int(math.exp(a)*10**15) + math.log())
print(int(math.exp(a)*10**15) + int(math.log(b,c)*10**15) + int(math.log(c,b)*10**15))

hash_func(1488)
