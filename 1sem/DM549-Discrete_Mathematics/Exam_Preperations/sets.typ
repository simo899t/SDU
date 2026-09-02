#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Discrete Mathematics notes",
  subtitle:      "Sets",
  course:        "DM549 - Discrete Mathematics",
  author:        "Simon Holm",
  date:          "Fall 2024",
  outline:       true,
  outline-depth: 2,
)

= Introduction to sets

Relevant lecture handouts: Lecture 2 (slide 13), Lecture 6 (slides 5-13).

#definition(title: "Definition (Definition 2.1.1)")[
  A _set_ (mængde) is an unordered collection of different objects, called _elements_.
  If object $x$ is an element of set $A$, we write $x in A$; otherwise we write $x in.not A$.
]

- Explicitly a set looks like ${"element"_1, "element"_2, dots, "element"_k}$.
- A simple set $A = {1,2,3,4}$ just means $A$ contains the numbers $1,2,3,4$.
- $3 in A$ if the object is an element of the set, $5 in.not A$ if it is not.
  ($in$ and $in.not$ are written `\in` and `\notin` in LaTeX.)
- Called "set" in English, "sæt" or "mængder" in Danish.

== Important sets to know

#table(
  columns: (auto, 1fr),
  align: (left, left),
  table.header([*Set*], [*Meaning*]),
  [$ZZ = {dots, -2, -1, 0, 1, 2, dots}$],  [All integers.],
  [$ZZ^+ = {1, 2, 3, dots}$],              [All positive integers.],
  [$ZZ^- = {dots, -3, -2, -1}$],           [All negative integers.],
  [$NN = {0, 1, 2, 3, 4, dots}$],          [The natural numbers. Sometimes $0$ is excluded — exercises usually state whether $0$ is included.],
  [$QQ = {m/n | m in ZZ, n in ZZ^+}$],     [The rational numbers: integers in fraction form. Numerator from $ZZ$, denominator from $ZZ^+$.],
  [$RR$],                                  [The real numbers: all rationals + irrationals. Any number that can be placed on a linear number line. Contains e.g. $pi, sqrt(2), e$. Imaginary and complex numbers are _not_ included.],
  [$emptyset$ (`\emptyset`)],              [The empty set, with no elements. Also written ${}$.],
)

== Set-builder notation

Given a propositional / logical function, you can define a set from it where every element
of the set satisfies the function's condition.

#example(title: "Example: set-builder notation")[
  Take the positive integers $ZZ^+ = {1,2,3,4,5, dots}$ and keep only the even numbers.
  Let $P(x): "\"x is an even number\""$. Then
  $ A = {x in ZZ^+ | P(x)} = {2, 4, 6, 8, 10, dots}. $
  Intuition: include every element of $ZZ^+$ for which the proposition $P(x)$ is true.
]
#pagebreak()

= Subsets and supersets

Picture two sets $A$ and $B$ where $A$ lies entirely inside $B$.

#definition(title: "Definition: subset")[
  $A$ is a _subset_ of $B$, written $A subset B$, if ALL values in $A$ are also in $B$.
  Correspondingly, $B$ is a _superset_ of $A$.
]

- It is typically also true that a set is a subset of itself: $A subset A$, since every value in $A$ also lies in $A$.
- In everyday use we often mean a _proper subset_, written $A psubset B$ (without the bar underneath).
- The point of $psubset$ versus $subset$: for a proper subset we must have $A eq.not B$, so a set cannot be a proper subset of itself.
#pagebreak()

= Set operators

Set operators are the tools used to define new sets from existing ones. The examples use
$A = {1,2,3}$, $B = {2,3,4}$ and the universe $U = {1,2,3,4,5}$, where $U$ is every object
from every set — including those not in any defined set.

== Intersection

#definition(title: "Definition: intersection")[
  An _intersection_ is the objects that two sets have in common:
  $ A inter B = {x | x in A and x in B}. $
]
Here only $2$ and $3$ lie in both $A$ and $B$, so $A inter B = {2,3}$.

== Union

#definition(title: "Definition: union")[
  A _union_ is the collection of two sets together with the objects they share:
  $ A union B = {x | x in A or x in B}. $
]
All objects from $A$ and $B$ are gathered, so $A union B = {1,2,3,4,5}$.

== Difference

#definition(title: "Definition: difference")[
  $A without B$ is $A$ with every object that also lies in $B$ removed — only what is
  exclusive to $A$ is kept. Sometimes written with $-$ (minus) instead of $without$.
]
Here $A without B = {1}$, because $2$ and $3$ are removed (they are in both $A$ and $B$).

== Complement

#definition(title: "Definition: complement")[
  The complement is everything except the set itself: take the universe and remove the set.
  Denoted with a bar over the set, $overline(A)$, and found as the difference with the universe:
  $ overline(A) = U without A. $
]

== Disjoint

Two sets are _disjoint_ if their intersection is empty: $A inter B = emptyset$.
#pagebreak()

= Set intervals

A set interval specifies a range the set stays within. Main takeaway: _square brackets_
mean the endpoint is included, _round parentheses_ mean it is not.

#definition(title: [Definition: intervals for $a, b in RR$])[
  $
    [a, b] &= {x in RR | a <= x <= b} quad & "closed interval from " a "to " b \
    (a, b) &= {x in RR | a < x < b}   quad & "open interval from " a "to " b \
    (a, b] &= {x in RR | a < x <= b}  quad & \
    [a, b) &= {x in RR | a <= x < b}  quad &
  $
]

#example(title: "Example: intervals")[
  - $(1, 4] = {x in RR | 1 < x <= 4}$. All numbers greater than $1$ up to $4$: $1$ is not included, but $3.12$ and $4$ are.
  - $[3, 10) = {x in ZZ | 3 <= x < 10}$. Based on $ZZ$, so explicitly: ${3,4,5,6,7,8,9}$.
]

*Sidenote:* when an interval is based on $RR$ it is infinite as long as $a < b$.
#pagebreak()

= Cardinality

Relevant lecture handouts: Lecture 8 (slides 6-10), Lecture 9 (slide 2).

== Basics

For a finite set the _cardinality_ is the number of elements, written $abs(A)$.

#example(title: "Example: cardinality")[
  - $A = {1, 3, 5, 8, 23}$ gives $abs(A) = 5$.
  - $emptyset$ has cardinality $0$.
  - $B = {{1,2,3}, {4,5,6}, {7,8,9}, 10, 11}$ gives $abs(B) = 5$.
]

A set inside another set counts as only _one_ object, no matter how many internal objects
it has — cardinality counts only the outermost layer.

== Three types of sets

/ Countable: you can count the cardinality given enough time (e.g. $A$ and $B$ above).
/ Countably infinite: you can keep going one element at a time along the number line, but never reach the end. Applies to $ZZ, ZZ^+, ZZ^-, QQ$.
/ Uncountable: you cannot even step to the next number. $RR$ is uncountable — if you are at $1$, the next number is neither $1.1$ nor $1.01$, since you can always push the $1$ further right with infinitely many $0$ digits.

#definition(title: [Definition: $aleph_0$])[
  Countably infinite sets have cardinality $aleph_0$ (aleph-null) — the smallest kind of
  infinity. There are also $aleph_1, aleph_2, aleph_3, dots$ (larger infinities), but they
  are not relevant here according to the slides.
]

#theorem(title: "Theorem 9.30")[
  Let $A$ and $B$ be finite sets. Then $abs(A times B) = abs(A) dot abs(B)$.
]
#pagebreak()

= Misc.

Relevant lecture handouts: Lecture 7 (slides 5-7).

== De Morgan's laws for sets

#theorem(title: "Theorem (Example 2.2.10)")[
  For any two sets $A, B$ it holds that
  $ overline(A union B) = overline(A) inter overline(B) quad "and" quad overline(A inter B) = overline(A) union overline(B). $
]

*Remark:* note the correspondence between $union$ and $or$, $inter$ and $and$, and $overline(#h(0.4em))$ and $not$.

== Tuples vs. sets

#table(
  columns: (1fr, 1fr),
  table.header([*Sets*], [*Tuples*]),
  [Order of objects does not matter], [Order of objects matters],
  [All objects are unique],           [Objects may be identical],
  [Uses $in$],                        [No proper notation, so $in$ is not used],
)

== The Cartesian product

#definition(title: "Definition: Cartesian product")[
  The Cartesian product pairwise combines two sets in every possible way, with the symbol $times$:
  $ A times B = {(a, b) | a in A and b in B}. $
]

Since order matters, swapping the sets gives different objects:

#example(title: "Example: Cartesian product")[
  With $A = {1, 2}$ and $B = {3, 4}$:
  $
    A times B &= {(1,3), (1,4), (2,3), (2,4)} \
    B times A &= {(3,1), (3,2), (4,1), (4,2)}
  $
  You can picture the product as a table. With $A = {1,2}$ and $B = {x, y}$:
  #table(
    columns: 3,
    align: center,
    table.header([], [$x$], [$y$]),
    [$1$], [$(1,x)$], [$(1,y)$],
    [$2$], [$(2,x)$], [$(2,y)$],
  )
  so $A times B = {(1,x), (1,y), (2,x), (2,y)}$.
]
