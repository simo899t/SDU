#import "../../../../../../temp.typ": *

#assignment(
  title: "Problem set 1",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm", "Johannes Rothe", "Shuagib Ibrahim", "Anne Sofie Høj", "Daniel Nissen"),
  date: "March, 2026",
  outline-depth: 3
)

= Problem 1 
An _interval_ is an ordered pair of real numbers $(a,b)$ such that $a<b$. We say that the interval _interleaves_ the interval $(c,d)$ in case $a<c<b<d$. Is the "interleaves" relation transitive? If not, what is it transitive closure?

== Solution

The relation definition is that 
$ "if" (x,y) in R "then" x bold(R)y. $

The interleaves relation is defined as:

$ (a,b)bold(R)(c,d) = a<c<b<d. $

For transitivity, it is required that
$ (a,b)bold(R)(c,d) "and" (c,d)bold(R)(e,f) => (a,b)bold(R)(e,f). $

By the interleaves definition, we know that

$ (a,b)bold(R)(c,d) = a<c<b<d $

$ (c,d)bold(R)(e,f) = c<e<d<f $

we can call these our premises.

Now we wish to prove that 
$ (a,b)bold(R)(e,f) = a<e<b<f. $

For this there is the following requirements
$ a<c<e<b<d<f $

then by our premises we can prove the following

$ a<c "and" c<e => a<e $
and 
$ b<d "and" d<f => b<f. $

Since we only know that
$ c<e<d "and" c<b<d $
we cannot say anything about whether $e<b$ is true.

Therefore, the relation is *not transitive*.

#pagebreak()

Let's do a *counterexample.*

Let $ A = [1,3] quad B = [2,6] quad C =[5,8]. $

If we apply $ A bold(R)B "and" B bold(R)C => A bold(R)C. $

This would imply that $[1,3]$ interleaves $[5,8]$.

This is false because it would imply that $ 1<bold(5<3)<8. $

For this to work we need the relation closure $R^+$.

Let's argue that for the closure, we need

$ (a,b) bold(R) (c,d) "and" (c,d) bold(R) (e,f ) => (a,b)bold(R)(e,f) $

where 

$ (a,b)bold(R)(e,f) = a<e<b<f $

Since by definition we already know $a<e$ and $b<f.$

The closure needs to be

$ R^+ = {(e,b)}. $

#pagebreak()

= Problem 2
Let $x$ and $Y$ be sets, let $R$ and $S$ be binary relations of type $X times Y$, and let $P$ and $Q$ be subsets of $X$. 
Recall that the _forward image_ $R(P)$ is the subset of $Y$ obtained by following all $R$-arrows forwards out of $P$. 
(And similarly for all the other forward images in this problem.) 
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
A relation is a relationship between sets of values such that for a set $X$ 
$ R psubset {(x,y) | x,y in X} $

=== (a)
We wish to show that $ R(P cup Q) = R(P)cup R(Q) $

We know that
$ forall y in R(P cup Q), exists x in P cup Q | (x,y) in R $

This means that for every $y$ in the relation on the union of the 
subsets $P$ and $Q$, there must be some $x$ where the relation $R$ holds.

Then we can show that the $y$ can either come from $R(P)$ or $R(Q)$, so

$ forall y in R(P) cup R(Q): y in R(P) "or"  y in R(Q). $
This is the same as $R(P cup Q) = R(P)cup R(Q).$ 

#QED

#pagebreak()

=== (b)

We wish to show that $ R(P cap Q) = R(P)cap R(Q). $

The same applies for
$ forall y in R(P cap Q), exists x in P cap Q | (x,y) in R $

again the $y$ must come from an $x$ mapped $R$.

But when we do

$ forall y in R(P) cap R(Q): y in R(P) "and" y in R(Q). $

We realise that for $y$ to exist, $P$ and $Q$ must share an element that both 
maps to the same $y$, if they don't then *this does not hold*. 

Let's do a *counterexample* to properly disprove this.

Let's check if $ R(P cap Q) = R(P) cap R(Q) $

when 

$ X={1,2} "and" Y={1,2} "and" P={1} psubset X "and" Q={2} psubset X. $


Now let
$ R= {(1,2), (2,2)} $

then
$ R(P cap Q) = {(1,2), (2,2)}({1} cap {2}) = emptyset $
and
$ R(P) cap R(Q) = {(1,2), (2,2)}({1}) cap {(1,2), (2,2)}({2}) = {2} cap {2} = {2}. $

Since $emptyset != {2}$ this *counters* the statement. 

#QED

#pagebreak()

=== (c)

We wish to show that $ (R cup S)(P) = R(P) cup S(P) $

we can say that any $y$ must come from an $x$ in $P$ applied by  $R$ or $ S$
$ forall y in (R cup S)(P), exists x in P | (x,y) in R "or" S. $

Then we can describe $y in R(P) cup S(P)$

$ forall y in R(P) cup S(P), exists x in P| (x,y) in R "or" S. $

Since these both describe the same relation from $x->y$ they are *equal*.

#QED

=== (d)

We wish to show that $ (R cap S)(P) = R(P) cap S(P) $

any $y$ must have an $x$ from $P$ which is applied by $R cap S$
$ forall y in (R cap S)(P), exists x in P | (x,y) in R cap S. $

Then showing that $x$ can only be 
$ forall y in R(P) cap S(P), exists x in P |(x,y) in R "and" S. $

Much like in (b) we realise that for $y$ to exist, $R$ and $S$ must share an element that both maps the same $x$ to the same $y$, if they don't then *this does not hold*. 

Let's do a *counterexample* to disprove this.



Let's check if $ (R cap S)(P) = R(P)) cap S(P) $

$ X={1,2,3} "and" Y={1,2,3} "and" P={1,2} psubset X. $


Now let
$ R = {(1,2), (1,3), (3,1)} quad S = {(1,3), (2,1), (2,2), (3,3)} $

then 
$ R cap S = {(1,3)}. $

Now we can check
$ (R cap S)(P) = {(1,3)}({1,2}) = {3} $
and
$ R(P)) cap S(P) = {(1,2), (1,3), (3,1)}({1,2})) cap {(1,3), (2,1), (2,2), (3,3)}({1,2})\ 
= {2,3} cap {1,2,3} = {2,3}. $

Since ${3} != {2,3}$ this *counters* the statement. #QED

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

Let's start of by doing a few examples

=== Example 1
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

Therefore, for any finite tree $T$, it seems that $dotless.j$ grows infinitely close to 2. Therefore: $ dotless.j(T) <2 $

Let's try and prove that

#pseudo[
*Proof by structural induction*
- We want to prove that $dotless.j(T)<2$
+ *$underline("Base case")$*
  + if $T_0$ is a single leaf (a node with no children) then $dotless.j (T)=1$
  + $ dotless.j (T_0) < 2. $
+ *$underline("Inductive hypothesis")$*
  
 + Assume that 
 + $ dotless.j(T_("subtree 1"))<2 "and" dotless.j(T_("subtree 2"))<2 $
+ *$underline("Inductive step")$*
  + For any tree with two subtrees (full tree) 
  + $ dotless.j (T) = 1+ (dotless.j(T_("subtree 1")) + dotless.j(T_("subtree 2")))/4 . $ 
  + Since, by #IH 
  + $ dotless.j(T_("subtree 1"))+dotless.j(T_("subtree 2")) <4. $
  + A new tree, composed of two subtrees, becomes 
  + $ dotless.j (T)=  1+ (dotless.j(T_("subtree 1")) + dotless.j(T_("subtree 2")))/4. $

  + Since  $(dotless.j(T_("subtree 1")) + dotless.j(T_("subtree 2")))/4 <1 $
  + $ dotless.j (T)=  1+ (dotless.j(T_("subtree 1")) + dotless.j(T_("subtree 2")))/4 <2 $
]

#pagebreak()

= Problem 4
Consider a directed graph $G$ with $12$ vertices that satisfies the following two properties:
- $G$ is _strongly connected_, i.e., between any two distinct vertices $u$ and $v$, there is a directed path from $u$ to $v$ as well as a directed path from $v$ to $u$, and
- there is at most one directed edge coming out of each vertex.

Draw $G$, and argue why your drawing is the _only possible_ solution (up to isomorphism).

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
) <fig:12cycle>


To say that @fig:12cycle is the _only possible solution_ (up to isomorphism). We need to argue that @fig:12cycle is isomorphic.

By definition, two directed graphs $G_1=(V_1,E_1)$ and $G_2=(V_2,E_2)$ are *isomorphic* if and only if there exists a one-to-one and onto function $f:V_1->V_2$ such that any two vertices $(a,b) in V_1$ where $(a,b) in E_1$ and $(f(a),f(b)) in V_2$ where $(f(a),f(b)) in E_2$. @DescMath

For a function to be one-to-one and onto on two sets, it is at least required that they have the same amount of elements. For this 12-cycle, any _other solution_ would, by definition, have exactly 12 vertices as well. 

For this 12-cycle, because its vertices have exactly one ingoing and exactly one outgoing edge, the adjacency property of isomorphism *can* be satisfied. Any two vertices in two in a strongly conected cycle like this, will hold this property. The only way this protery would *not* hold, is that a vertex in the graph has either 2 outgoing edges or 2 ingoing edges.

#pagebreak()

This can be showed with an example of 2 isomorphic graphs:

#figure(
  grid(
    columns: 3,
    column-gutter: 1em,
    align: center,
    [
      #dirgraph("
#scl: 0.5;
1.12;
1>3; 2>4; 3>5; 4>6; 5>7; 6>8;
8>9; 7>10; 9>11; 10>12; 11>1; 12>2;
")
    ],[
      #align(center + horizon)[
        #stack(
          dir: ttb,
          spacing: 0.4em,
          $bold(f)$,
          scale(x: 400%,y:300% , reflow: true)[#sym.arrow
          ],
        )
      ]
    ],
    [
      #dirgraph("
#scl: 0.5;
1.12;
1: [6]; 2: [12]; 3: [7]; 4: [1]; 5: [8]; 6: [2];
7: [9]; 8: [3]; 9: [4]; 10: [10]; 11: [5]; 12: [11];
1>3; 2>4; 3>5; 4>6; 5>7; 6>8;
8>9; 7>10; 9>11; 10>12; 11>1; 12>2;
")
    ],
  ),
  caption: [$G:$ Two isomorphic directed graphs due to a function $f$]
) <fig:example1>

Here, the left graph fullfills the same requirements as directed graph G on @fig:12cycle, though it has different relations. By applying the following one-to-one and onto function $f$ to the graph:
$ f(12) =11, f(2)=12, f(4)=1,f(6)=2,f(8)=3,f(9)=4,\ f(11)=5,f(1)=6,f(3)=7,f(5)=8,f(7)=9,f(10)=10 $
yields the same relations as the original graph $G$ on @fig:12cycle.



Because of this, @fig:12cycle depicts the *_only possible_* solution (up to isomorphism).

#pagebreak()

#bibliography("references.bib")
 