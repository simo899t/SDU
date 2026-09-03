import random

# System uses 4 Variables N, e, p, and q

# p and q are always primes
# p * q = n

# SP = (N,e)
# SK = (N,d)

def find_d(e:int, p:int, q:int):
    """finder d"""
    i = 0
    while e*i%((p-1)*(q-1)) != 1:
        i = i + 1 
    return i

def find_e(d:int, p:int, q:int):
    """finder e"""
    i = 0
    while d*i%((p-1)*(q-1)) != 1:
        i = i + 1 
    return i

def primeFactors(n):
    factors = []
    i = 3
    while i < n // 2 + 1:
        if is_prime(i) and n % i == 0:
            factors.append(i)
        i = i + 2
    return factors

def valid_keys(N:int, e:int, d:int) -> bool:
    """Checks if PK and SK are vaid keys valid"""
    w = primeFactors(N)
    print(w,len(w))
    if len(primeFactors(N)) == 2:
        return find_e(d,w[0],w[1]) == e and find_d(e,w[0],w[1]) == d
    else:
        return False

def encrypt(m:int, e:int, N:int) -> int:
    """Encrypts message (m) using RSA and PK"""
    return (m**e)%N

def decrypt(c:int, d:int, N:int) -> int:
    """Decrypts an encrypted message (c) using RSA and SK"""
    return (c**d)%N

def exp(a:int, k:int, n:int):
    """Computes a^k (mod n) for encrytion/decryption"""
    if k < 0:
        return print("Error")
    elif k == 0:
        return print(1)
    elif k == 1:
        return print(a%n)
    elif k%2 == 1:
        print(a*str("exp")(a, k-1,n))%n
        return (a*exp(a, k-1,n))%n
    elif k%2 == 0:
        c = exp(a,k/2,n)
        print(c)
        return print((c*c)%n)


def is_prime(n:int) -> bool:
    """Checks if n is a prime number"""
    if n % 2 == 0 and n != 2:
        return False
    for i in range(3,int(n**0.5)+1,2):
        if n % i == 0:
            return False
    return True

def check_inverse(a: int, b:int, n:int) -> bool:
    """checks if a is an inverse to b modulus n"""
    return (a*b)%n == 1



def find_a_number(n:int):
    """finder et tal"""
    i = 0
    while (i*i)%n != 1:
        i = i + 1 
    return i

def rabinMiller(num):
    # Returns True if num is a prime number.

    s = num - 1
    t = 0
    while s % 2 == 0:
        # keep halving s while it is even (and use t
        # to count how many times we halve s)
        s = s // 2
        t += 1

    for trials in range(5): # try to falsify num's primality 5 times
        a = random.randrange(2, num - 1)
        v = pow(a, s, num)
        if v != 1: # this test does not apply if v is 1.
            i = 0
            while v != (num - 1):
                if i == t - 1:
                    return False
                else:
                    i = i + 1
                    v = (v ** 2) % num
    return True