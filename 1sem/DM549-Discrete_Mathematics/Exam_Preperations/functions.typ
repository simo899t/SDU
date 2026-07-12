#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Discrete Mathematics notes",
  subtitle:      "Functions",
  course:        "DM549 - Discrete Mathematics",
  author:        "Simon Holm",
  date:          "Fall 2024",
  outline:       true,
  outline-depth: 2,
)

= Functions
#definition(title: "Definition")[
  Let $A$ and $B$ be non-empty sets. A function $f$ from $A$ to $B$, for each $x in A$, assigns *precisely* one element $f(x) in B$ to $x$.

  $ f: A -> B $
  Where $A$ is the domain and $B$ is the co domain
]

#definition(title: "Definition: Image (range)")[
  Let $f: A -> B$ be a function. The _image_ or _range_ of $f$ is

  $ "im"(f) = {f(x) | x in A} = {y in B | exists x in A : f(x) = y} $
]

== Injective, Surjective, Bijective
#definition(title: "Definition: Injective function")[
  A function $f : A -> B$ is called injective or _one-to-one_ if
  $ forall x_1, x_2 in A : (f(x_1) = f(x_2) iimp x_1 = x_2), $
  that is $f$ assigns ant value $y in B$ to at most one $x in A$
]
 
#definition(title: "Definition: Surjective function")[
  A function $f : A -> B$ is called surjective or _onto_ if
  $ y in B | exists x in A : f(x) = y $
  that is, $"Im"(f) = B$
]

#definition(title: "Definition: Bijective function")[
  A function $f : A -> B$ is called bijective or _one-to-one correspondence_ if ot is both injective and surjective.
]

== Combining functions

#definition(title: "Definition: Addition and multiplication of two functions")[
  Let $f : A -> B$ and $g : A to B$ be functions. Then $(f+g):A to B$ and $(f dot g) : A to B$ are functions with
  $ (f+g)(x) &= f(x) + g(x) / (f dot g)(x) &= f(x) dot g(x) $
  $forall x in A$
]
#definition(title: "Definition: Composition of two functions")[
  Let $f : A -> B$ and $g : A to B$ be functions. Then the _composition_ of $g$ and $f$, $(f comp g) : A to B$ is a functions with
  $ (g comp f) = g(f(x)) $
  $forall x in A$
]
Note that for composition the codomain of $f$ has to match the domain of $g$

== Increasing, Decreasing, and more

#definition(title: "Definition")[
  Let $f : A to B$ If, for all $x_1,x_2 in A$ with $x_1 < x_2$, it holds that
  - $f(x_1) <= f(x_2)$, $f$ is called _increasing_
  - $f(x_1) < f(x_2)$, $f$ is called _strictly increasing_
  - $f(x_1) >= f(x_2)$, $f$ is called _decreasing_
  - $f(x_1) > f(x_2)$, $f$ is called _strictly decreasing_
  If $f$ is increasing or decreasing, it is called monotone.
]

Observe that if a function $f$ is either strictly increasing or strictly decreasing, it must also be injective.

== Cardinality
#definition(title: "Definition: Cardinality")[
  Two sets $A,B$ have the same cardinality ($abs(A) = abs(B)$), if there exists a bijection function from $A to B$
]




