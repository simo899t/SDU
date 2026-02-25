#import "../../../../../../temp.typ": *

#assignment(
  title: "Exercise sheet 2",
  course: "AI504 — Knowledge Representation",
  author: "Simon Holm",
  date: "February, 2026",
)

= Problem 1 
An _interval_ is an ordered pair of real numbers $(a,b)$ such that $a<b$. We say that the interval _interleaves_ the interval $(c,d)$ in case $a<c<b<d$. Is the "interleaves" relation transitive? If not, what is it transitive closure?

== Solution

The interleaves relation is defined as:

$ (a,b)bold(R)(c,d) = a<c<b<d $

We can easily counter example this with the fact that

$ (1,2)bold(R) (3,4) = 1<3<2<4 quad bold("FALSE") $

The relation is therfore *not transitive*

We need additional information (the closure) to say that the interleave is transitive.

Because of this we add
$ R^+ = {((a,b),(c,b))|a<c "and" c<b} $

Now $ a<b, a<c, c<b, c<d $

This all shows that
$ a<c<b<d $
#pagebreak()

= Problem 2
Let $x$ and $Y$ be sets, let $R$ and $S$ be binary relations of type $X times Y$, and let $P$ and $Q$ be subsets of $X$. 
Recal that the _forward image_ $R(P)$ is the subset of $Y$ obtained by following all $R$-arrows forwards out of $P$. 
(And similarly for all the other forwars images in this problem.) 
For each of the following identities, either prove correct or provide a counterexample. 
_All proofs are short and all counterexamples are tiny_
 
#abc[
  $R(P cup Q) = R(P)cup R(Q)$
][
  $R(P cap Q) = R(P)cap R(Q)$
][
  $(R cup S) (P) = R(P)cup S(P)$
][
  $(R cap S) (P) = R(P)cap S(P)$
]

== Solution
For this problem $ R = {(x,y) | "some relation"} $

=== (a)
We wish to show that $ R(P cup Q) = R(P)cup R(Q) $

We know that
$ forall y in R(P cup Q), exists x in P cup Q | (x,y) = R $

This just means that for every $y$ from the relation on the union of the 
subsets $P$ and $Q$ there must be some $x$ where the relation $R$ holds.

Then we can show that the $y$ can either come from $R(P)$ or $R(Q)$.

$ forall y in R(P) cup R(Q): y in R(P) "or" y in R(Q) $
This is the same as $R(P cup Q) = R(P)cup R(Q)$ #QED

=== (b)

We can try the same technique on $R(P cap Q) = R(P)cap R(Q)$

The same applies when
$ forall y in R(P cap Q), exists x in P cap Q | (x,y) = R $

again the $y$ must come from an $x$ mapped $R$

But when we do

$ forall y in R(P) cap R(Q): y in R(P) "and" y in R(Q) $

We realise that for $y$ to exist $P$ and $Q$ must share an element that both 
maps to the same $y$ if they dont then *this does not hold*. #QED


