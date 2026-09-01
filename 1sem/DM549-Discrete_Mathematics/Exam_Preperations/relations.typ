#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Discrete Mathematics notes",
  subtitle:      "Relations",
  course:        "DM549 - Discrete Mathematics",
  author:        "Simon Holm",
  date:          "Fall 2024",
  outline:       true,
  outline-depth: 2,
)

= Relations

#definition(title: "Definition")[
  Let $A,B$ be sets. A (binary) relation from $A$ to $B$ is a subset of $A times B$
]

A relation can be seen as a generalization of a function.
- A _Function_ can assign exactly 1 element from $B$ to any element in $A$
- A _Relation_ can assign any element in $B$ to any element in $A$

== Visualize relations
Let $A = {1,2,3}$ be a set, and $x R y$ be a relation from $A$ to $A$.

- Sets
  - $ R = {(1,2),(2,3),(1,3)} $
- Graph
#graph(
  nodes: (
    (pos: (0,0), label: $1$),
    (pos: (1,1), label: $2$),
    (pos: (2,0), label: $3$),
  ),
  edges: (
    ((0,0), (1,1),"->"),
    ((1,1), (2,0),"->"),
    ((0,0), (2,0),"->"),
  ),
  caption: [Graph of $R = {(1,2),(2,3),(1,3)}$],
)

- Matrix
$ R = mat(0,1,1;0,0,1;0,0,0) $


Remember that there exists relations that are neither reflexive or irreflexive, but never both.

Remember that there exists relations that are neither symmetric or anti-symmetric but also both symmetric and anti-symmetric.
#pagebreak()

== Reflexivity

#definition(title: "Definition: Reflexive")[
  A relation $R$ on a set $A$ is called *reflexive* if $(a,a) in R$ for all $a in A$.
]
- Graph $#sym.arrow.r.long$ self loops on every vertex
- Matrix $#sym.arrow.r.long$ diagonal is all $1$

#definition(title: "Definition: Irreflexive")[
  A relation $R$ on a set $A$ is called *irreflexive* if $(a,a) in.not R$ for all $a in A$.
]
- Graph $-->$ no self loops
- Matrix $-->$ diagonal is all $0$

== Symmetry

#definition(title: "Definition: Symmetric")[
  A relation $R$ on a set $A$ is called *symmetric* if $(a,b) in R ==> (b,a) in R$ for all $a,b in A$.
]
- Graph $-->$ every edge that goes one way also goes the other way (edges are undirected)
- Matrix $-->$ the matrix equals its own transpose, $R = R^T$

#definition(title: "Definition: Anti-symmetric")[
  A relation $R$ on a set $A$ is called *antisymmetric* if $(a,b) in R and (b,a) in R ==> a=b$, for all $a,b in A$.
]
- Graph $-->$ no pair of distinct vertices has edges going both ways
- Matrix $-->$ if $R_(i j) = 1$ and $R_(j i) = 1$ then $i = j$


== Transitivity

#definition(title: "Definition (Definition 9.1.5)")[
  A relation $R$ on a set $A$ is called *transitive* if whenever $(a,b) in R$ and $(b,c) in R$, then $(a,c) in R$, for all $a,b,c in A$.
]
- Graph $-->$ whenever there is a path $a -> b -> c$, there is also a direct edge $a -> c$


= Closures
#definition(title: "Definition: Closure")[
  Let $R$ be a relation on set $A$ and let $P$ be a property of relations. Then, the _closure_ of $R space wrt space P$ is (if it exists) the relation $C$ on $A$ such that
  + $R psubset C$,
  + $C$ fulfills property $P$,
  + $C psubset S$ for every $S$ that fulfills 1. and 2, (in place of $C$)
]

For some property (like transitive), the closure $C$ is the "minimal" extension" of $R$ such that $R$ fulfills property $P$. That being said, ig $R$ fulfills $P$, then $C = P$

*Note:* anti-symmetric or irreflexive closures does not exists. One can not make an anti-symmetric or irreflexive closure by adding elements.

#definition(title: "Definition: Reflexive Closure")[
  The _reflexive closure_ of a relation $R$ on a set $A$ is $ r(R) ) R cup {(a,a) | a in A} $
]

#definition(title: "Definition: Symmetric Closure")[
  The _symmetric closure_ of a relation $R$ on a set $A$ is $ s(R) ) R cup {(b,a) | (a,b) in R} $
]

#definition(title: "Definition: Transitive Closure")[
  The _transitive closure_ of a relation $R$ on a set $A$ is 

  $ t(R) = R^* = underbrace(union.big_(i=1)^oo R^i.,R cup R^2 cup R^3 cup dots) $
]
_in other words_
When $(a,b),(b,c) in R$ but $(a,c) in.not R$. Add $(a,c)$ to $R$ until we can no longer find such $a,b,c$.
#pagebreak()

= Combining relations
#definition(title: "Definition: Composition of two relations")[ 
  Let $A,B,C$ be sets, $R$ a relation from $A$ to $B$ and $S$ A relations from $B$ to $C$.

  Then $ S compose R = {(a,c) | exists b : (a,b) in R and (b,c) in S}. $
  If $A = B$, then $R^2$ denotes $R compose R$, $R^3$ denotes $R compose R compose R$, etc.
]

$(a,b) in R^k$ if and only if one can walk from $a$ to $b$ in $k$ steps along edges (as in a graph)

#example(title: "Example: composition of two relations")[
  Given tree sets $ A= {1,2,3},space B = {1,2,3,4}, space C={0,1,2} $
  
  And relations $ R={(1,1),(1,4),(2,3),(3,1),(3,4)}, space S = {(1,0),(2,0),(3,1),(3,2),(4,1)} $,

  Then combination / composition $R compose S = {(1,0),(1,1),(2,1),(2,2),(3,0),(3,1)}$

  #figure(
    image("assets/image.png"),
    caption: [Visual example of composition of two relations],
  ) <label>
  
]
#pagebreak()

= Equivalence relation and class
#definition(title: "Definition: Equivalence relations")[  
  A relation $R$on a set is called an equivalence relation if it is
  - reflexive,
  - symmetric, and
  - transitive

  If this is the case, then for $(a,b) in R$, $a$ and $b$ are called equitant.
]

#definition(title: "Definition: Equivalence class")[
  Let $R$ be an equivalence relation on a set $A$. For $a in A$.

  Then $ [a]_R = {b | (a,b) in R} $

  Is the equivalence class of $a space wrt space R$. 
]

One can see this as 'all elements which "equivalent" to $a$'

= Ordering

== Partial order
#definition(title: "Definition: Partial order")[
  A relation $R$ on a set $A$ is called a _partial order_ if it is
  - reflexive,
  - anti-symmetric, and
  - transitive.

  If this is the case, $(A,R)$ os called a partially ordered set (or a poset)
] 
Partial ordering is usually seen as $<=$ or $prec.eq$

== Total order
#definition(title: "Definition: Total order")[
  Let $(A, prec.eq)$ be a poset, if $a,b in A$ are comparable, we cal $prec.eq$ a total order
]

== Lexicographic order
#definition(title: "Definition: Lexicographic order")[
  Let $(A_1, prec.eq_1),(A_2, prec.eq_2), dots, (A_n, prec.eq_n)$ be partial orders. Then we can define a _lexicographic order_, a partial order, $prec.eq$ on $A_1 times A_2 times dots times A_n$ as follows.
  For different elements $(a_1,a_2, dots,a_n), (b_1,b_2, dots,b_n)$ of $A_1 times A_2 times dots times A_n$ that are not equal, $(a_1,a_2, dots,a_n) prec.eq (b_1,b_2, dots,b_n)$ holds if and only if
  - $a_1 prec b_1$, or
  - there exists an $i>0$ such that $a_1 = b_1$,$a_2 = b_2, dots, a_i = b_i$ and $a_(iplus) prec.eq_iplus b_iplus$. 
]
#example(title: "Example: Lexicographic order")[
  Words (sets of letters) are lexicographically ordered in most dictionaries.
  $ #[hate] prec.eq #[hope] prec.eq #[love] $ 
]



