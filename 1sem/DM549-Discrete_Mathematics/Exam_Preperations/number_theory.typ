#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Discrete Mathematics notes",
  subtitle:      "Number Theory",
  course:        "DM549 - Discrete Mathematics",
  author:        "Simon Holm",
  date:          "Fall 2024",
  outline:       true,
  outline-depth: 2,
)

= Divisibility
#definition(title: "Definition: Divisibility")[
  We say that a number $a$ divides $b$ if there exists a number $c$  such that $a c = b$.

  This is written as $a mid b$.

  We call $a$ a factor or divisor of $b$ and we call $b$ a multiple of $a$
]

#definition(title: "Definition: Quotient and remainder")[
  Let $a$ be an integer and $d$ a positive integer. Then there exist unique integers $q$ and $r$, with $0 lt.eq r < d$, such that
  $ a = d q + r $

  We call $d$ the *divisor*, $a$ the *dividend*, $q = a "div" d$ the *quotient*, and $r = a mod d$ the *remainder*.
]

= Modular inverse
#definition(title: "Definition: Modular inverse")[
  Modular inverse, is the concept that for any integer $a$, The modulo is an integer $m$, where there exists $x$ such that:
  $ a dot x eq.triple 1 mod m $

  This exists on the condition that $a$ and $m$ are *coprime* 
]

#definition(title: "Definition: Coprime")[
  Two integers $a,m$ are coprime if and only if
  $ gcd(a,m) = 1 $
]

#example(title: "Example: Find the modular inverse")[
  Given the integers $a=3$ and $m=14$

  To calculate the modular inverse, it essentially means that we want to express 1 as a combination of 3 and 14.  First we will first calculate their gcd.   

  $ gcd(3,14) \ 
  14 &= 4 dot 3 + 2 \ 
  3 &= 1 dot 2 + 1 \ 
  2 &= 2 dot 1 + 0 $

  Since the last coefficient before achieving 0 was 1. Their GCD is 1

  
  We then use the Extended Euclidean Algorithm by making our way back from  3=1·2+1.

  $ 1 &= 3 − 2 dot 1 where 2=14−4 dot 3 \ 
  1 &= 3 − 1 dot (14 − 4 dot 3) \
  1 &= 3 − 14 + 4 dot 3 \
  1 &= 5 dot 3−14 $

  This means that $ 1=bluemath(5) dot 3−1·14 $

  $x = 5$
]
Here's the python #emoji.snake implementation
#codly(header: align(center,[*Inverse Mod*]))
```py
def inverse_mod(a, b):
    x = a
    y = b
    oldolds = 1
    olds = 0
    oldoldt = 0
    oldt = 1
    while y != 0:
        q = x // y
        r = x % y
        x = y
        y = r
        s = oldolds - q * olds
        t = oldoldt - q * oldt
        oldolds = olds
        oldoldt = oldt
        olds = s
        oldt = t
    return oldolds
```




= GCD
#definition(title: "Definition: GCD")[
  The greatest common divisor is the greatest integer that divides both a and b.

  This can be found by integers $a,b$ such that when finding the modular inverse of $a,b$. the last nonzero answer will their GCD 
]
#example(title: "Example: GCD")[
  FInd $gcd(10,36)$

  This is done by definition "Definition: GCD"
  
  $ 36 mod 10 eq.triple 6 \
  10 mod 6 eq.triple 4 \
  6 mod 4 eq.triple 2 \
  4 mod 2 eq.triple 0 $

  Thus, $gcd(10,36) = 2$
] 

#pagebreak()


= LCM
#definition(title: "Definition: LCM")[
  The Least Common Multiple of the positive integers $a,b$
  is the smallest positive integer that is divisible by both $a$ and $b$.
  The least common multiple of $a$ and $b$ is denoted by $lcm(a,b)$

  Let $a = p_1^(a_1) dot p_2^(a_2) dot dots.c dot p_n^(a_n)$ and
  $b = p_1^(b_1) dot p_2^(b_2) dot dots.c dot p_n^(b_n)$ be the prime
  factorizations of $a$ and $b$, where $p_1, p_2, dots.c, p_n$ are the
  distinct primes dividing $a$ or $b$, and each exponent $a_i, b_i gt.eq 0$.

  Then $lcm(a,b)$ is defined as
  $ lcm(a,b) = p_1^(max(a_1,b_1)) dot p_2^(max(a_2,b_2)) dot dots.c dot p_n^(max(a_n,b_n)) $
]

#definition(title: "Definition: Prime factorization")[
    1. Start with the smallest prime number ($p_i$) and check if it divides the given number. Repeat until it's no longer divisible by $p_i$.
    
    2. Move to next prime ($p_iplus$), and repeat with the remainder of step 1
    
    3. Express the result as a product of primes.
]

#example(title: "Example: Prime factorization")[
  Find the prime factorization of $60$.

  $ 60 / 2 = 30, quad 30 / 2 = 15 $

  $15$ is not divisible by $2$, so we divided by $2$ twice and continue with $15$.

  $ 15 / 3 = 5 $

  We divided by $3$ once. $5$ is already prime, so it counts once.

  Thus, $60$ can be written as $60 = 2^2 dot 3 dot 5$.
]

#pagebreak()

= Euclidean algorithm
#definition(title: "Definition: Euclidean algorithm")[
  The Euclidean algorithm is a method to find the greatest common divisor between two numbers.
  
  Let $a = b dot q + r$

  Where $a$ is the greater of the two numbers, 
  $b$ is the smaller of the two numbers, 
  $q$ is the quotient (how many times $b$ divides $a$, 
    $floor(frac(a,b,style:"skewed"))=q$), 
  and $r$ is the remainder of $a mod b$

  Repeat this sentence with the remainder, and stop when $r = 0$, the GCD will be the last nonzero remainder
]

Written recursively in python #emoji.snake here
#codly(header: align(center,[*Greatest Common Divider*]))
```py
def gcd(a, b):
    if b == 0:
        return a
    return gcd(b, a % b)
```



#example(title: "Example: Euclidean algorithm")[
  Find $gcd(18,12)$.

  $ 18 &= 12 dot 1 + 6 \
  12 &= 6 dot 2 + 0 $

  Since the remainder is now $0$, the GCD is the last nonzero remainder.

  Thus, $gcd(18,12) = 6$.
]

$ phi(x_i) = e^(-alpha(c_j^2-r_j^2)) $

#pagebreak()

= Chinese Remainder Theorem
#definition(title: "Definition: The Chinese Remainder Theorem")[
  The Chinese Remainder Theorem is a method used to solve simultaneous congruences with different moduli.

  Consider a system of simultaneous congruences
  $ x &eq.triple a_1 quad (mod n_1) \
    x &eq.triple a_2 quad (mod n_2) \
      &dots.v \
    x &eq.triple a_k quad (mod n_k) $

  Requirements for the Chinese Remainder Theorem:
  - $n_1, n_2, dots, n_k$ are pairwise coprime, i.e. $gcd(n_i,n_j) = 1$ for all $i eq.not j$.
  - $a_1, a_2, dots, a_k$ are known integers.

  When these requirements are met, the Chinese Remainder Theorem guarantees there is a unique solution for $x mod N$, where
  $ N = n_1 dot n_2 dot dots.c dot n_k $
]


#example(title: "Example: Chinese Remainder Theorem")[
  Solve the following system of congruences:
  $ x &eq.triple 2 quad (mod 3) \
    x &eq.triple 3 quad (mod 5) \
    x &eq.triple 1 quad (mod 7) $

  + *Compute $N$*, the product of the moduli.
    $ N = 3 dot 5 dot 7 = 105 $

  + *Compute the partial products* $N_i = N \/ n_i$.
    $ N_1 = 105 / 3 = 35, quad N_2 = 105 / 5 = 21, quad N_3 = 105 / 7 = 15 $

  + *Find the modular inverses* $y_i$ such that $N_i dot y_i eq.triple 1 quad (mod n_i)$.
    - $N_1 eq.triple 35 eq.triple 2 quad (mod 3)$, and $2 dot y_1 eq.triple 1 quad (mod 3)$ gives $y_1 = 2$.
    - $N_2 eq.triple 21 eq.triple 1 quad (mod 5)$, and $1 dot y_2 eq.triple 1 quad (mod 5)$ gives $y_2 = 1$.
    - $N_3 eq.triple 15 eq.triple 1 quad (mod 7)$, and $1 dot y_3 eq.triple 1 quad (mod 7)$ gives $y_3 = 1$.

  + *Construct the solution* $x = sum_i a_i dot N_i dot y_i$.
    $ x &= (2 dot 35 dot 2) + (3 dot 21 dot 1) + (1 dot 15 dot 1) \
        &= 140 + 63 + 15 \
        &= 218 $

  + *Reduce $x$ modulo $N$.*
    $ 218 mod 105 = 8 $

  Thus, the solution is $x eq.triple 8 quad (mod 105)$.
]

Chinese remainder theorem implemented in python #emoji.snake
#codly(header: align(center,[*Chinese Remainder Theorem*]))
```py
def chi_rem_thm(mn,an):
    m = 1
    Mn = []
    yn = []
    for k in range(0, len(mn)):
         m  = m * mn[k]
    for  k in range (0, len(mn)):
        Mk = m / mn[k]
        Mn.append(Mk)
        yk = inverse_mod(Mn[k],mn[k]) % mn[k]
        yn.append(yk)
    x = 0
    for  k in range (0, len(yn)):
        x = x + an[k] * Mn[k] * yn[k]
    while x >= m:
        x = x - m
    return x
```



