#import "../../../../../../temp.typ": *

#assignment(
  title: "Problem set 1",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm", "Johannes Rothe", "Shuagib Ibrahim", "Anne Sofie Høj"),
  date: "March, 2026",
  outline-depth: 1
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

Then we wish to prove that 
$ (a,b)bold(R)(e,f) = a<e<b<f$

For this there is the following requirements
$ a<c<e<b<d<f $

We can then prove the following

$ a<c "and" c<e => a<e $
and 
$ b<d "and" d<f => b<f $

since we only know that
$ c<e<d "and" c<b<d $
We cannot say anything about weather $e<b$ is true.

#pagebreak()

Lets do a *counterexmple.*

Let $ A = [1,3] quad B = [2,6] quad C =[5,8] $

If we apply $ A bold(R)B and B bold(R)C => A bold(R)C $

This would imply that $[1,3]$ interleaves $[5,8]$

this is false because it would imply that $ 1<5<3<8 $

For this to work we need the relation closure $R^+$

Lets argue that fro the closure, we need

$ (a,b) bold(R) (c,d) "and" (c,d) bold(R) (e,f ) => (a,b)bold(R)(e,f) $

Where 

$ (a,b)bold(R)(e,f) = a<e<b<f $

Since by definition we already know $a<e$ and $b<f$

The closure needs to be

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

This just means that for every $y$ in the relation on the union of the 
subsets $P$ and $Q$, there must be some $x$ where the relation $R$ holds.

Then we can show that the $y$ can either come from $R(P)$ or $R(Q)$, so

$ forall y in R(P) cup R(Q): y in R(P) "or"  y in R(Q). $
This is the same as $R(P cup Q) = R(P)cup R(Q).$ #QED
#pagebreak()

=== (b)

We can apply on $R(P cap Q) = R(P)cap R(Q)$

The same applies for
$ forall y in R(P cap Q), exists x in P cap Q | (x,y) in R $

again the $y$ must come from an $x$ mapped $R$

But when we do

$ forall y in R(P) cap R(Q): y in R(P) "and" y in R(Q) $

We realise that for $y$ to exist $P$ and $Q$ must share an element that both 
maps to the same $y$ if they dont then *this does not hold*. 

Let's do a *counterexample* to proerly disprove this

Let set $X$ be a set, and $Y$ be the forward image of relations $R_1$, $S_1$ and $R_2$, $S_2$

Then let $P$ be a subset of $X$

Lets check if $ R(P cap Q) = R(P) cap R(Q) $

When 

$ X={1,2} "and" Y={1,2} "and" P={1} superset X "and" Q={2} superset X $


Now let
$ R= {(1,2), (2,2)} $

now we can check
$ R(P cap Q) = {(1,2), (2,2)}({1} cap {2}) = emptyset $
and
$ R(P) cap R(Q) = {(1,2), (2,2)}({1}) cap {(1,2), (2,2)}({2}) = {2} cap {2} = {2} $

Since $emptyset != {2}$ this *counters* the statement. #QED
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


Let's do a *counterexample* to disprove this

Let $X$ be a set, and $Y$ be the forward image of relations $R$, $$

Then let $P$ be a subset of $X$

Then i want to see if $ (R cap S)(P) = R(P)) cap S(P) $

$ X={1,2} "and" Y={1,2} "and" P={1,2} superset X $


Now let
$ R = {(1,2), (1,3), (3,1)} quad S = {(1,3), (2,1), (2,2), (3,3)} $

then 
$ R cap S = {(1,3)} $

now we can check
$ (R cap S)(P) = {(1,3)}({1,2}) = {3} $
and
$ R(P)) cap S(P) = {(1,2), (1,3), (3,1)}({1,2})) cap {(1,3), (2,1), (2,2), (3,3)}({1,2})\ 
= {1,2,3} cap {1,2,3} = {1,2,3} $

Since ${3} != {1,2,3}$ this *counters* the statement. #QED

#pagebreak()

= Problem 3
A _full binary_ tree is a nonempty binary tree where each node is a leaf or has exactly two children.
Define a function $dotless.j$ from full binary trees to rational numbers by recursion as follows:

$
  dotless.j(T) := mycases(
    1, T "is a leaf",
    1 + (dotless.j(T_0) + dotless.j(T_1))/4, "otherwise, where" T_0 "and" T_1 "are the two subtrees",
    word: "if"
  )
$

Calculate the value of $dotless.j$ on lots of small examples. Formulate a conjecture about how big it can get, and prove it.

== Solution

Lets start of by doing a few examples

=== Example 2
$ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
  - $T_0$

] $

Here $dotless.j(T) = 1 $

=== Example 2
$ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
  - $T_0$
    - $ T_1 $
    - $ T_2 $
] $

Here $dotless.j(T) =1+(1+1)/4 = 1 + 1/2 = 1.5 $


=== Example 3
$ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
  - $T_0$
    - $ T_1 $
      - $ T_3 $
      - $ T_4 $
    - $ T_2 $
] $

Here $dotless.j(T) =1+((1+(1+1)/4)+1)/4 = 1 + (2+ 1/2)/4 = 1.625 $
#pagebreak()

=== Example 4
$ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
  - $T_0$
    - $ T_1 $
      - $ T_3 $
      - $ T_4 $
    - $ T_2 $
      - $ T_5 $
      - $ T_6 $
] $

Here $dotless.j(T) =1+((1+(1+1)/4)+(1+(1+1)/4))/4 = 1 + (3)/4 = 1.75 $

=== Example 5
$ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
   - $T_0$
    - $ T_1 $
      - $ T_3 $
        - $ T_7 $
        - $ T_8 $
      - $ T_4 $
    - $ T_2 $
      - $ T_5 $
        - $ T_9 $
        - $ T_10 $
      - $ T_6 $
] $

Here $dotless.j(T) =1+((1+((1+(1+1)/4) + 1)/4)+(1+((1+(1+1)/4)+1)/4))/4 = 1 + (13)/16 = 1.8125 $

#pagebreak()

=== Conjecture

From the examples above, it seems that $dotless.j(T_("size "k))->2$ as $k -> oo$.

Lets try and prove this


We know that for a full endless binary tree, then for any $dotless.j (T)$ 
$ dotless.j (T) = 1+(dotless.j (T)+dotless.j (T))/4 = 1+1/2 dotless.j (T). $
Then by recursion

$ dotless.j (T) = 1+1/2 dotless.j (T) = 1+1/2 + 1/4 dotless.j (T) $

That means that as the tree becomes infinitly large, that is $abs(T) -> oo$. It means that $ dotless.j (T) = summ(i=0,oo,(1/2)^i) $

We know that geometric series where $r<1$, then $summ(i=0,oo,r^i) = 1/(1-r)$

$ dotless.j (T) = summ(i=0,oo,(1/2)^i) = 1/(1-frac(1,2)) = 1/frac(1,2,style: "skewed") = 2 $




// #pseudo[
// *Proof by structured induction*
// + *$underline("Base case")$*
//   + if $T$ is a single leaf (a node with no children) then $dotless.j (T)=1$
// + *$underline("Inductive step")$*
//   + For any tree with two subtrees (full tree) $ dotless.j (T) = 1+ (dotless.j(T_("subtree 1")) + dotless.j(T_("subtree 2")))/4 $ 
//   + *$underline("inductive hypothosis")$*
//     + Since $T$ is a full infinite tree, any subtree is also a full tree
//     + $ dotless.j (T) = 1+ (2dotless.j(T))/4 = 1+1/2 dotless.j(T) $
//     + Then by recursion $ dotless.j (T) = 1+1/2 dotless.j (T) = 1+1/2 + 1/4 dotless.j (T) $
//     + This is a geometric series such that
//     + $ dotless.j (T) = 1+ 1/2 + 1/4 + 1/8 +1/16 +dots $
//     + $ dotless.j (T) = summ(i=0,oo,(1/2)^i) = 1/(1-frac(1,2,style: "skewed")) = 1/frac(1,2,style: "skewed") = 2 $
// 
//     + 
//   +
//   
// ]

#pagebreak()

= Problem 4
Consider a directed graph $G$ with $12$ vertices that satisfies the following two properties:
- $G$ is _strongly connected_, i.e., between any two distinct vertices $u$ and $v$, there is a directed path from $u$ to $v$ as well as a directed path from $v$ to $u$, and
- there is at most one directed edge coming out of each vertex.

Draw $G$, and ague why your drawing is the _only possible_ solution (up to isomorphism).

== Solution

Since each vertex has *at most one* directed edge going out, and $G$ must be strongly connected, every vertex must have *exactly one* outgoing as well as *exactly one* ingoing edge as every node must also be reachable. This creates a 12-cycle:

#figure(
  dirgraph("
#scl: 0.7;
1.12;
1>2; 2>3; 3>4; 4>5; 5>6; 6>7;
7>8; 8>9; 9>10; 10>11; 11>12; 12>1;
"),
caption: [$G:$ A strongly connected 12-cycle.]
) <fig:cycle>


*Isomophic property* between two structures means that they share the same exact same properties. For this 12-cycle, because its vertices have exactly one ingoing and exactly one outgoing edge, it means, that any vertex is traversable from any other vertex. This property is true regardless of the order of vertices. Because of this, @fig:cycle depicts the _only possible_ solution (up to isomorphism).











