#!/usr/bin/env python

import random

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

def euler_func(p, q):
    return (p - 1) * (q - 1)


prime_iterations = 400

def get_keys(user: str):
    p = get_random_prime(prime_iterations)
    q = get_random_prime(prime_iterations)
    print(user, '\tgenerated prime numbers', p, q)

    m = p * q
    print("\tgot module", m)

    while True:
        e = get_random_prime(euler_func(p,q))
        if euler_func(p,q) % e != 0:
            break

    print("\tgenerated e", e)


    while True:
        d = random.randint(2, prime_iterations ** 2)
        if d * e % euler_func(p,q) == 1:
            break

    print('\tgenerated d', d)
    return ((e, m), (d, m))


if __name__ == '__main__':
    (A_ok, A_pk) = get_keys("Alice")
    (B_ok, B_pk) = get_keys("Bob")

    input_message = [ch for ch in "Input message"]
    print(f"sending  : {input_message}")

    encrypt_mes = [ord(x)**B_ok[0] % B_ok[1] for x in input_message]
    print(f"ecnrypted: {encrypt_mes}")

    decrypted_mes = [chr(x**B_pk[0] % B_pk[1]) for x in encrypt_mes]
    print(f"decrypted: {decrypted_mes}")

    assert input_message == decrypted_mes
