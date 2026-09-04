#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Lecture 1: Cybersecurity Fundamentals",
  course:        "AI510 - Cybersecurity and Innovation",
  author:        "Simon Holm",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 3,
)

= Briefest of Primers on Cryptography

#definition(title: "Definition: Cryptography")[
  The discipline that embodies the principles, means, and methods for
providing information security, including confidentiality, data integrity, non-
repudiation, and authenticity.

]

= The setup
- Confidentiality
#figure(
  image("assets/image-7.png"),
  caption: [Someone can read your info/message],
) <label>

- Integrity & Authenticity
#figure(
  image("assets/image-8.png"),
  caption: [Someone can corrupt your info/message],
) <label>

= Symmetric encryption
#figure(
  image("assets/image-9.png"),
  caption: [Use keys to encrypt and decrypt messages],
) <label>
== Caesar Cipher
Every character is replaced by another.

Given an interger $k$, which determines the rotation of encryottion
#example(title: "Example: encryption with " + $k = 3$)[
  Given the message "Hej"

  $ #[H] -> #[K], quad #[e] -> #[h], quad #[j] -> #[m] $
   So given $k = 3$

   $ #[Hej] -> #[Khm] $
]

#definition(title: "Definition: Kerckhoff’s Principle(s)")[
  Has six design rules, most importantly that "The key should be the only secret, not the mechanism"

  Secrecy based on the mechanism is called Security by Obscurity which is *bad*.
]

== One-Time Pad
Message $m$ and key $k$ are added using modular addition.
#example(title: "Example")[
  $m = 5$, $k = 15$, $mod 17$
  $ m+k teq  5+ 15 teq 20 = 3 mod 17 $
]

In practice we ise bit-wise XOR, modular addition with $mod 2$
#example(title: "Example")[
  $m = 1001$, $k = 1010$
  $ m+k teq  1001xor 1010 teq 20 = 0011 $
]

#example(title: "Example: A more complex example")[
  #figure(
    image("assets/image-10.png"),
    caption: [],
  ) <label>
  
]

== AES by NIST
Result of an international competition, which encrypts data in blocks by 128 bits. Keys can either be 128, 192 or 256 bits

Consists of 4 operations in 10-14 stages (depending on key length)

- Substitute Bytes: Uses a table called S-Box to perform a byte-by-byte substitution
- Shift Rows: A simple permutation that is performed row by row
- Mix Columns: A substitution that alters each byte in a column as a function of all the bytes in the column
- Add Round Key: A simple bitwise XOR of the current block with a portion of the expanded key

#figure(
  image("assets/image-11.png"),
  caption: [Example \ #text(red)[WILL NOT BE IN THE EXAM]],
) <label>

== Modes of Operation
These algorithms can be divided into two classes
- Stream-ciphers: Each bit of the data is encrypted and decrypted individually
- Block-ciphers: The data is divided into blocks of fixed length and each block is encrypted individually

Problem: Same plaintexts will result in same ciphertexts keeping the structure of the data intact.
#figure(
  image("assets/image-12.png"),
  caption: [],
) <label>

== Cipher Block Chaining
Cipher Block Chaining is just one, many likewise alternatives exists (Currently most advanced one is Galois/Counter Mode) 
#figure(
  image("assets/image-13.png"),
  caption: [Cipher Block Chaining, relies each encryption on the last. which is great for security but pretty bad for parallelization],
) <label>

== Hash Functions
The goal of all hash functions is to derive values of fixed length from inputs of arbitrary length. These are used for password storage.

=== Cryptographic hashes
- One-way function
Given output $h$ it is infeasible to find $x$ such that $H(x) = h$
- Collision-resistance
It is infeasible to find any pair $(x,y)$ such that $H(x) = H(y)$
- Efficient
Computation takes little resources

One should (as of 2024) use SHA-3 or SHA-2. Nothing else.

=== Hashes for Password Storage
More efficient that cryptographic hashes


Originally no dedicated hashes existed, and cryptographic hashes ware used mostly. Today we use specialized hashes for password storage. Best practice is "Argon2".

#figure(
  image("assets/image-14.png"),
  caption: [],
) <label>

#figure(
  image("assets/image-15.png"),
  caption: [],
) <label>

= Asymmetric Encryption
#figure(
  image("assets/image-16.png"),
  caption: [],
) <label>

= Hybrid Encryption
#figure(
  image("/AI510-Cybersecurity_and_Innovation/Exercises/ex1/assets/image.png"),
  caption: [],
) <label>


== RSA
- Encryption $c teq m^e mod n$
- Decryption $m teq c^d mod n [teq (m^e)^d mod n teq m^(e d)]$

$e$ is the public key, $d$ is the private key

#example(title: "Example: RSA")[
  1. Select $p = 17, q = 11$

  2. Calculate $n = p dot q = 17 dot 11 = 187$
  3. Calculate $phi(n) = (p-1) dot (q-1) = 160$
  4. 
]
