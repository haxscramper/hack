#!/usr/bin/env python

import random, math

class User(object):
    def __init__(self, bitlen):
        self.public_alice = get_prime(bitlen)
        self.public_bob = get_prime(bitlen)
        self.private_key = get_prime(bitlen)
        self.full_key = None

    def generate_partial_key(self):
        return self.public_alice ** self.private_key % self.public_bob

    def generate_full_key(self, partial_key_r):
        self.full_key = partial_key_r ** self.private_key % self.public_bob

    def encode(self, encrypted_message):
        return "".join([chr(ord(c) ^ self.full_key) for c in encrypted_message])

def is_prime(n):
    if n > 1:
        for i in range(2, int(n/2)+1):
            if (n % i) == 0:
                return False

        else:
            return True

def get_prime(n):
    while True:
        range_start = int("1" + "0" * (n), 2)
        range_end = int("1" + "1" * (n), 2)
        number = random.randint(range_start, range_end)
        if is_prime(number): 
            return number

def exchange_private(alice, bob: User):
    alice.public_bob = bob.public_bob
    bob.public_alice = alice.public_alice

def generate_full(alice, bob: User):
    a_partial = alice.generate_partial_key()
    b_partial = bob.generate_partial_key()

    alice.generate_full_key(b_partial)
    bob.generate_full_key(a_partial)

message = "Приеду завтра в полдень"
bitlen = int(math.log2(max([ord(c) for c in message])))

alice = User(bitlen)
bob = User(bitlen)

exchange_private(alice, bob)
generate_full(alice, bob)

b_encrypted = bob.encode(message)
print(b_encrypted)

desc_message = alice.encode(b_encrypted)
print(desc_message)
