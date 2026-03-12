#import "../../../../../../temp.typ": *

#assignment(
  title: "Problem set 2",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm", "Johannes Rothe", "Shuagib Ibrahim", "Anne Sofie Høj"),
  date: "March, 2026",
  outline-depth: 1
)

= Problem 1
Work over the set ${a, b, c, d}$ of nouns. Consider the set of sentences

$ Gamma = {"All" a "are" b, "All" a "are" c, "All" b "are" d, "All" c "are" d}. $

Write down two different proof trees verifying $Gamma prov$ All $a$ are $d$. Write your proof trees carefully and as legibly as possible.

== Solution

$  $

#figure(
  ptree(
  "all a are c",
  r($$, "all a are d"),
  r($$, "all b are c")),
  caption: [Proof tree 1]
)<proof-tree-1>

@proof-tree-1 shows one of the two different proof trees verifying that $Gamma prov "All "a" are "d""$ using the 'axioms' "All $a$ are $b$" and" All $b$ are $d$".

#figure($ #tree(
  shape: "rectangle",
  spacing: (40pt, 40pt),
  node-inset: 12pt
)[
   - All $bold(a)$ are $bold(d)$
    - $underbrace("All "bold(a)" are "bold(c)"","axiom")$
    - $underbrace("All "bold(c)" are "bold(d)"","axiom")$
  
] $,
caption: [Proof tree 1]
)<proof-tree-2>
@proof-tree-2 shows the second of the two different proof trees verifying that $Gam prov "All "a" are "d""$ using the 'axioms' "All $a$ are $c$" and" All $c$ are $d$".


#pagebreak()

= Problem 2

Work over the set ${a, b, c, d}$ of nouns. In this question I want you to find *two different models of the same size*. Each of your models should satisfy all of the following sentences

$ "All" a "are" b, "All" a "are" c, "All" b "are" d, "All" c "are" d $
and falsify both of the following sentences
$ "All" b "are" c, "All" c "are" b. $
_Note_. We have to be a little careful in saying what it means for two models to be different. But basically
if you label the elements of your model $1, 2, dots, n$ you should not be able to relabel (i.e., permute) the elements of one of your models to get the other one.

== Solution 2

Let $cal(m)$ be a model m = {1,2,3,4,5}.

=== Model 1
$[|a|] = emptyset$

$[|b|] = {1}$

$[|c|] = {2,3}$

$[|d|] = {1,2,3}$


"all a are b" $a psubset b = emptyset psubset {1}$ Which is true

"all a are c" $a psubset c = emptyset psubset {2,3}$ Which is true

"all b are d" $b psubset d = {1} psubset {1,2,3}$ Which is true

"all c are d"  $a psubset c = {2,3 psubset {1,2,3 $ Which is true

Falsify both:

"all b are c" $b psubset c = {1} psubset {2,3}$  Which is false

"all c are b" $c psubset b = {2,3} psubset {1}$ Which is false 


=== Model 2 
Let $cal(m)$ be a model m = {7,8,9,10,11}.


$[|a|] = {7}$

$[|b|] = {7,8}$

$[|c|] = {7,9}$

$[|d|] = {7,8,9,10,11}$


"all a are b" $a psubset b = {7} psubset {7,8}$ Which is true

"all a are c" $a psubset c = {7} psubset {7,9}$ Which is true

"all b are d" $b psubset d = {7,8} psubset {7,8,9,10,11}$ Which is true

"all c are d"  $a psubset c = {7,9} psubset {7,8,9,10,11}$ Which is true

Falsify both:

"all b are c" $b psubset c = {7,8} psubset {7,9}$  Which is false

"all c are b" $c psubset b = {7,9} psubset {7,8}$ Which is false 


#pagebreak()



= Problem 3
The _dual_ of a sentence is obtained by switching its two nouns. For example the dual of All $p$ are $q$ is All $q$ are $p$. We denote the dual of a sentence $phi$ by $phi^dag$. If $Gamma$ is a set of sentences, by $Gamma^dag$ we mean the
obvious thing, i.e., “take the dual of each sentence in $Gamma$.”

Suppose $Gamma prov phi$. Prove that $Gamma^dag prov phi^dag$ by induction on proof trees. Be very careful and deliberate, so that
I know you understand what you're doing.


= Solution 3

#pseudo[
  *#u("Proof by induction")*

  *Goal:*  if $cal(T)$ is a proof $phi$ from $Gam$ then there exist a proof $s.t. Gam^dag prov phi^dag.$
  - *#u("Base step")* (T is a single leaf)
  + Suppose T is a proof of $phi$ from $Gam$, T is just $phi$, and since all leaves come from $Gam$, so $phi in Gamma$.
  + The dual $phi$ to $phi^dag$, is a proof tree $cal(T)$ and all leaves are in $Gamma^+$, it follows that $phi^dag in Gam^dag$, 
  + therefor $ Gam^dag prov phi^dag. $
  - *#u("Inductive hypothesis")*
  + For all proofs $cal(T)$, $Gam$, and $phi$, if $cal(T)$  is a proof of $phi$ from $Gam$ ,then there exist a proof T, such that $ Gam^dag prov phi^dag. $
  - *#u("Inductive step")*
  + Suppose T is not a leaf, decompose T as follows.
  + Let's assume that $cal(T)$ is not a single leaf $phi$ and $phi^+$, but $cal(T)$ is decomposed of two trees, left and right trees labeled $cal(T_0) $ and $cal(T_1)$ and its dual:
  + #figure(
  image("assets/image.png", width: 60%),
  caption: [
    1 function
  ],
)




+ The proof tree can then be restructued, so it satisfies the $phi^dag$ dual will then satisfy

+ #figure(
  image("assets/image2.png", width: 60%),
  caption: [
    Then the dual...
  ],
)
]

#pagebreak()


#ptree(
  $C$,
  r($T_0$, $phi_1$),
  r($T_1$, $phi_2$,
    r($$, $phi_3$),
    r($$, $phi_4$)),
)




#pagebreak()

= Problem 4
Continuing the previous problem, there is also a notion of a dual model. So the idea is that given a
model $cal(M)$, we should be able to define its “dual” $cal(M)^dag$, which has the property that $cal(M)^dag ent phi^dag$ if and only if $cal(M) ent phi$, for each sentence $phi$. Define the dual and prove that it enjoys this property.

(Hint. Start out with some explicit small models, see if you can find their duals by trial and error, and
then see what the common pattern is.)
#pagebreak()

= Problem 5

The point of this exercise is to show you that models can be built out of anything, as long as they
have the correct type. Work over the set $N = {0, 1, 2, 3}$ of nouns. Now this set of nouns makes for
weird-looking sentences like All $3$ are $1$. But once we construct an $N$-model, it makes perfect sense to
say that such sentences are satisfied or falsified.

Define the $N$-model $cal(R)$ as follows. Its domain is $RR$, the set of real numbers, and for each $b in N$, define

$ [|b|] = {x in RR: x^b-x < 0} $

List all of the $N$-sentences that $cal(R)$ satisfies, and argue that your answer is correct. (For the purposes of this exercise, assume $0^0 = 1$.)

== Solution 5

To show this, we can construct all the nouns $[|b|]$ from $N$ that follows $cal(R)$:
- $b=0 ==> {x in RR: x>1}$
- $b=1 ==> {emptyset}$
- $b=2 ==> {x in RR: x>1}$
- $b=3 ==> {x in RR: x<(-1)}$

Therefore the set of sentences that can be satisfied are:
- All of 0 are 2
- All of 2 are 0
- All of 1 are 0
- All of 1 are 2
- All of 1 are 3
#QED
