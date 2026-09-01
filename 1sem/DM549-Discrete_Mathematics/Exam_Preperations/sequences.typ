#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Discrete Mathematics notes",
  subtitle:      "Sequences",
  course:        "DM549 - Discrete Mathematics",
  author:        "Simon Holm",
  date:          "Fall 2024",
  outline:       true,
  outline-depth: 2,
)

= Sequences
#definition(title: "Definition 2.4.1")[
  A sequence (følge) is a function of $NN$ to some set.
]

*Remarks:*
- The domain of the function may be finite or infinite. Usually ${0,1,2,...}$ or ${1,2,3,...}$.
- Denote by $a_n$ the number that the function maps to, also called a *term* of the sequence. (Recall our notation for the Fibonacci sequence!)
- The sequence is then denoted by ${a_n}$. Do not confuse this with set notation!
- We can also list the terms in order, e.g., for the domain being $NN$:
  $ {a_n} = a_0, a_1, a_2, a_3, ... $
- You can also think of a sequence with domain $D$ as a $|D|$-tuple.

== Examples of sequences of numbers
- Fibonacci sequence: $f_n = f_(n-1) + f_(n-2) = {1,1,2,3,5,8,...}$
- All numbers for $1/n$ where $n$ is greater than $1$: ${1/n}_(n >= 1) = {0.5, 0.33, 0.25, ...}$


== Defining geometric and arithmetic sequences
#definition[
  An infinite *geometric sequence* (geometrisk følge) is a sequence of the form
  $ a_n = c dot r^n, quad n in NN $
  where $c in RR$ is the *initial term* (begyndelsesled) and $r in RR$ is the *common ratio* (fælles faktor). We obtain finite geometric sequences by stopping at some point.
]

#definition[
  An infinite *arithmetic sequence* (aritmetisk følge) is a sequence of the form
  $ a_n = b + n dot d, quad n in NN $
  where $b in RR$ is the *initial term* (begyndelsesled) and $d in RR$ is the *common difference* (fælles differens). We obtain finite arithmetic sequences by stopping at some point.
]

#pagebreak()

== Series (rækker, *not* "serier")
#definition(title: "Series")[
  Let
  $ a_m, a_(m+1), ..., a_n $
  be a sequence. Then there is an associated *series* (række), the sum of all terms in the sequence. It is denoted by
  $ sum_(i=m)^(n) a_i quad "or" quad sum_(m <= i <= n) a_i $
]

*Remarks:*
- If the domain of the sequence is $D$, we also write $sum_(i in D) a_i$.
- One could also talk about series that are the sum of infinitely many terms of an infinite sequence.
- Here, we focus on finite sequences.
- Otherwise, to be completely formal, we would need to talk about a concept from calculus called convergence.

=== Geometric series
#theorem(title: "Theorem 2.4.1")[
  For a finite *geometric series* (with $c=1$), the series corresponding to a finite geometric sequence, it holds that
  $ sum_(m <= i <= n) a_i = cases(
    (r^(n+1) - 1) / (r - 1) quad & "if" r in RR without {1},
    n+1 & "if" r = 1.
  ) $
]

*Note:* if $|r| < 1$ and we consider the *infinite* geometric series, the term $r^(n+1)$ vanishes as $n$ grows towards $∞$, so
$ sum_(i=0)^(∞) r^i = 1/(1-r) $

#pagebreak()

=== Arithmetic series
#theorem(title: "Theorem 2.4.1")[
  For a finite *arithmetic series*, the series corresponding to a finite arithmetic sequence, it holds that
  $ sum_(i=0)^(n) (b + i dot d) = b dot (n+1) + d dot (n dot (n+1))/2 $
]

Some useful sequences:

#table(
  columns: 2,
  [*$n$th term*], [*First 10 terms*],
  [$n^2$], [${1, 4, 9, 16, 25, 36, 49, 64, 81, 100, ...}$],
  [$n^3$], [${1, 8, 27, 64, 125, 216, 343, 512, 729, 1000, ...}$],
  [$n^4$], [${1, 16, 81, 256, 625, 1296, 2401, 4096, 6561, 10000, ...}$],
  [$2^n$], [${2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, ...}$],
  [$3^n$], [${3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, ...}$],
  [$n!$], [${1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, ...}$],
  [$F_n$ (fibonacci)], [${1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...}$],
)



== Proofs
Example of a geometric sequence:
$ 1, 1/2, 1/4, 1/8, ... $
and the corresponding geometric series would be:
$ 1 + 1/2 + 1/4 + 1/8 + ... $

Recall:
$ sum_(i=0)^(n) (1/2)^i = 2 - (1/2)^n $
and we have that:
$ sum_(i=0)^(∞) (1/2)^i = 1/(1 - 1/2) = 2 $
#QED

#pagebreak()

=== Proof of the finite geometric series formula
Let (geometric series definition):
$ S = sum_(i=0)^(n) r^i $

And observe that:
$ r dot S = sum_(i=0)^(n+1) r^i = sum_(i=0)^(n) r^i + r^(n+1) - 1 $

When solving for $S$, we get
$ S = (r^(n+1) - 1)/(r - 1) quad "when" r ≠ 1 $

For $r = 1$, it is clear that
$ S = sum_(i=0)^(n) r^i = n+1 $
#QED

=== Proof of the finite arithmetic series formula
$ sum_(i=0)^(n) (b + i dot d) &= sum_(i=0)^(n) b + sum_(i=0)^(n) i dot d \
  &= b dot (n+1) + d dot sum_(i=0)^(n) i \
  &= b dot (n+1) + d dot (n dot (n+1))/2. $
#QED

