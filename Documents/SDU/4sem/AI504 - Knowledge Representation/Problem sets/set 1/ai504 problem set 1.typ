#import "../../../../../../temp.typ": *

#assignment(
  title: "Problem set 1",
  course: "AI504 — Knowledge Representation",
  author: "Simon Holm",
  date: "February, 2026",
)

= Problem 1 
An _interval_ is an ordered pair of real numbers $(a,b)$ such that $a<b$. We say that the interval _interleaves_ the interval $(c,d)$ in case $a<c<b<d$. Is the "interleaves" relation transitive? If not, what is it transitive closure?

== Solution

The relation definition is that 
$ "if" (x,y) in R "then" x bold(R)y. $

The interleaves relation is defined as:

$ (a,b)bold(R)(c,d) = a<c<b<d. $



For transitivity
$ (a,b)bold(R)(c,d) and (c,d)bold(R)(e,f) => (a,b)bold(R)(e,f). $

By the interleaves definition

$ (a,b)bold(R)(c,d) = a<c<b<d $

$ (c,d)bold(R)(e,f) = c<e<d<f $

We wish to prove that $a<e and b<f and e<b$

We can then prove that

$ a<c and c<e => a<e $
and 
$ b<d and d<f => b<f $

since we only know that
$ c<e<d and c<b<d $
We cannot say anything about weather $e<b$ is true

So take a counter example

that $ c<e<d and c<b<d => e=b or e<b or e>b $


We can denote the relation closure $R^+$ as
$ R^+ = {(e,b)} $




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
$ forall y in R(P cup Q), exists x in P cup Q | (x,y) in R $

This just means that for every $y$ from the relation on the union of the 
subsets $P$ and $Q$ there must be some $x$ where the relation $R$ holds.

Then we can show that the $y$ can either come from $R(P)$ or $R(Q)$.

$ forall y in R(P) cup R(Q): y in R(P) or y in R(Q) $
This is the same as $R(P cup Q) = R(P)cup R(Q)$ #QED

=== (b)

We can apply on $R(P cap Q) = R(P)cap R(Q)$

The same applies for
$ forall y in R(P cap Q), exists x in P cap Q | (x,y) in R $

again the $y$ must come from an $x$ mapped $R$

But when we do

$ forall y in R(P) cap R(Q): y in R(P) and y in R(Q) $

We realise that for $y$ to exist $P$ and $Q$ must share an element that both 
maps to the same $y$ if they dont then *this does not hold*. 

Counter exmaple: 
#figure(
  image("/assets/IMG_7680.jpeg", width: 30em),
  caption: [look at this idiot]
)

#QED
#pagebreak()

=== (c)

For $ (R cup S)(P) = R(P) cup S(P) $

We can say that any $y$ must come from an $x$ in $P$ applied by  $R$  $ S$
$ forall y in (R cup S)(P), exists x in P | (x,y) in R "or" S $

Then we can describe $y in R(P) cup S(P)$

$ forall y in R(P) cup S(P), exists x in P| (x,y) in R "or" S $

=== (d)

For $ (R cap S)(P) = R(P) cap S(P) $

any $y$ must have an $x$ from $P$ which is applied by $R cap S$
$ forall y in (R cap S)(P), exists x in P | (x,y) in R cap S $

Then showing that $x$ can only be 
$ forall y in R(P) cap S(P), exists x in P |(x,y) in R and S $




$ #image("/assets/IMG_7682.jpeg", width: 30em) $


