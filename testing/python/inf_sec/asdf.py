#!/usr/bin/env python

def hash_func(num):
    num = (num ^ 37) ^ (num >> 32)
    num = num + (num << 4)
    num = num ^ (num >> 32)
    return num

hashes = {}
test_count = 1000_000
collisions = []
for num in range(test_count):
    hash = hash_func(num)
    if hash in hashes:
        collisions.append([num, hashes[hash], hash])

    hashes[hash] = num

if len(collisions) == 0:
  print("No collisions found for a hash function")

else:
  print(f"Found {len(collisions)} collisions for a hash function - {len(collisions) / test_count:3.2%}")
  for i in range(min(10, len(collisions))):
    c = collisions[i]
    print(f"hash({c[0]}) ==  hash({c[1]}) = {c[2]}")

