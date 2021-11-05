#!/usr/bin/env python

import random

n = 1_000_000

numbers = [i for i in range(n + 1)]
is_prime = [True for i in range(n + 1)]

# print(numbers)

i = 2
while i < len(numbers):
    a = i**2
    while a <= n:
        is_prime[a] = False
            #numbers.pop(numbers.index(a))
        a += numbers[i]
    i += 1

primes = []
for i in range(n+1):
    if is_prime[i]:
        primes.append(numbers[i])

# print(primes)

print(len(primes))

def get_random_prime(n):
    numbers = [i for i in range(n+1)]
    is_prime = [True for i in range(n+1)]

    i = 2
    while i < len(numbers):
        a = i**2
        while a <= n:
            is_prime[a] = False
            a += numbers[i]
        i += 1

    primes = []
    for i in range(n+1):
        if is_prime[i]:
            primes.append(numbers[i])
    return primes[random.randint(0,len(primes)-1)]

g = random.randint(100, 1000000)
print("Alice got g:", g)
print("Bob got g:", g)
print("Eva got g:", g)



p = random.randint(100, 1000000)
print("Alice got p:", p)
print("Bob got p:", p)
print("Eva got p:", p)

a = get_random_prime(1000000)
print("Alice generated a:", a)
A = g**a % p
print("Alice sent A:", A)
print("Bob got A:", A)
print("Eva got A:", A)

b = get_random_prime(1000000)
print("Bob generated b:", b)
B = g**b % p
print("Bob sent B:", B)
print("Alice got B:", B)
print("Eva got B:", B)

print("Alice generated", B**a % p)
print("Bob generated", A**b % p)
print("Eva got nothing, lol")
