#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Discrete Mathematics notes",
  subtitle:      "Counting",
  course:        "DM549 - Discrete Mathematics",
  author:        "Simon Holm",
  date:          "Fall 2024",
  outline:       true,
  outline-depth: 2,
)

= Counting Rules

The following rules are nice to remember for counting.

#definition(title: "Definition: The Division Rule")[
  Suppose A is a finite set  with $A = B_1 cup B_2 cup dots cup B_n$ where
  - $abs(B_i) = d quad forall i space$  and 
  - $B_i cap B_j = emptyset quad forall i,j where i!=j$
  Then $n = abs(A)\/ d$.
]


#definition(title: "Definition: The Product Rule")[
  For any finite sets $S_1, S_2, dots, S_n$
  $ underbrace(abs(times.big^n _(i=1)S_i),abs(S_i times S_2 times dots times S_n)) = underbrace(prod(n,i=1,abs(S_i)), abs(S_1) dot abs(S_2) dot dots dot abs(S_n)). $
]


#definition(title: "Definition: The Sum Rule (for two sets)")[
  For any finite sets $S_1, S_2$ with $S_1 cap S_2 = emptyset$, it holds that $abs(S_1 cup S_2) = abs(S_1) + abs(S_2)$.
]

#definition(title: "Definition: The Sum Rule")[
  For any finite sets $S_1, S_2$ with $S_1 cap S_2 = emptyset$, it holds that
  $ underbrace(abs(union.big^n _(i=1)S_i),abs(S_i cup S_2 cup dots cup S_n)) = underbrace(summ(n,i=1,abs(S_i)), abs(S_1) + abs(S_2) + dots + abs(S_n)). $
]

#definition(title: "Definition: The Subtraction Rule")[
  For any finite sets $S_1,S_2,dots,S_n$ it holds that $abs(S_1) union abs(S_2) = abs(S_1) + abs(S_2) - abs(S_1 cap S_2)$.
]

= Pigeonhole
#theorem(title: "The Pigeonhole Principle (Theorem 6.2.1 from book)")[
  Let $k>=1$ be an integer. When $k+1$ or more objects are placed into $k$ boxes, there exists at least one boz that contains at least two of the objects.
]
- Intuition: If there exists 10 holes, but 11 pigeons, at least one hole must contain 2 pigeons.

= Permutations ans combinations
Permutations count ordered selections of $r$ elements from a set $S$ of $n$ elements (order matters).
#theorem(title: "Permutations 1")[
  If $n>0 in ZZ$ and $r in ZZ$ such that $1<=r<=n$, then there are
  $ P(n,r) = n(n-1)(n-2)dots(n-r+1) $
  $r$-permutations of a set with $n$ distinct elements.
]

#theorem(title: "Permutations 2")[
  If $n,r in ZZ$ such that $0<=r<=n$, then 
  $ P(n,r) = (n!)/((n-r)!) $
]
 

#theorem(title: "Combinations")[
  The number of $r$-combinations of a set with $n$ elements, where $n>0 in ZZ$ and $r in ZZ$ such that $0 <= r<=n$, equals
  $ C(n,r) = (n!)/(r!(n-r)!) $ 
]

Note that $C(n,r)$ can also be written as $mat(n;r)$ ($r$ choose $r$), known as the binomial coefficient #emoji.face.wink.

#let nc(x,y) = $mat(#x;#y)$

= Binomial Coefficient
#theorem(title: "The Binomial Theorem")[
  Let $x$ and $y$ be variables, and let $n>0 in ZZ$
  
  Then. $ (x+y)^n = summ(n,j=0,mat(n;j) x^(n-j) y^j) = nc(n,0)x^n + nc(n,1)x^(n-1) y + dots + nc(n,n-1)x y^(n-1) + nc(n,n)y^n. $

  Where $nc(n,j) = C(n,j) = (n!)/(j!(n-j)!)$
]

= Repetition and indistinguishably
#theorem(title: "Theorem: Repeating combinations")[
  There are $C(n+r-1,r) = C(n+r-1,n-1)$ $r$-combinations from a set with $n$ elements when repetition is a allowed
]



#figure(
  table(
    columns: (1fr, 1fr, 1.5fr),
    align: center + horizon,
    inset: 12pt,
    text(size: 1.1em)[$quad$], text(size: 1.1em)[$k$-distinguishable boxes], text(size: 1.1em)[$k$-indistinguishable boxes],
    text(size: 1.1em)[$n$-distinguishable objects], text(size: 1.4em)[$(n!)/(n_1 ! dot n_2 ! dot dots dot n_k !)$], text(size: 1.1em)[$summ(k,j=1,1/(j!)) summ(j-1,i=0,(-1)î nc(j,i)(j-i)^n)$],
    text(size: 1.1em)[$n$-indistinguishable objects], text(size: 1.2em)[$C(n+1-1,k)$], text(size: 1.3em)[No formula]
  ),
  caption: [Good to remember #emoji.face.smirk],
) <label>

#figure(
  table(
    columns: (0.5fr, 1fr, 1.1fr),
    align: center + horizon,
    inset: 12pt,
    text(size: 1.1em)[$quad$], text(size: 1.1em)[number of \ $r$-permutations of $S$], text(size: 1.1em)[number of \ $r$-combinations of $S$],
    text(size: 1.1em)[without repetition], text(size: 1.3em)[$P(n,r)$], text(size: 1.3em)[$C(n,r)$],
    text(size: 1.1em)[with \ repetition], text(size: 1.3em)[$n^r$], text(size: 1.3em)[$C(n+r-1,r)$]
  ),
  caption: [Good to remember 2 #emoji.face.smirk],
) <label>
