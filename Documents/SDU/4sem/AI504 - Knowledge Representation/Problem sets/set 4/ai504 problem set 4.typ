#import "../../../../../../temp.typ": *

#assignment(
  title: "Problem set 4",
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

#figure(
  $ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
   - All $bold(a)$ are $bold(d)$
    - $underbrace("All "bold(a)" are "bold(b)"","axiom")$
    - $underbrace("All "bold(b)" are "bold(d)"","axiom")$
  
] $,
caption: [Proof tree 1]

)<proof-tree-1>

@proof-tree-1 shows one of the two different proof trees verifying that $Gamma prov "All "a" are "d""$ using the 'axioms' "All $a$ are $b$" and" All $b$ are $d$".

#figure($ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
   - All $bold(a)$ are $bold(d)$
    - $underbrace("All "bold(a)" are "bold(c)"","axiom")$
    - $underbrace("All "bold(c)" are "bold(d)"","axiom")$
  
] $,
caption: [Proof tree 1]
)<proof-tree-2>
@proof-tree-2 shows the second of the two different proof trees verifying that $Gamma prov "All "a" are "d""$ using the 'axioms' "All $a$ are $c$" and" All $c$ are $d$".


#pagebreak()

= Problem 2

Work over the set ${a, b, c, d}$ of nouns. In this question I want you to find *two different models of the same size*. Each of your models should satisfy all of the following sentences

$ "All"a "are" b, "All" a "are" c, "All" b "are" d, "All" c "are" d $

and falsify both of the following sentences

$ "All" b "are" c, "All" c "are" b. $

_Note_. We have to be a little careful in saying what it means for two models to be different. But basically
if you label the elements of your model $1, 2, dots, n$ you should not be able to relabel (i.e., permute) the elements of one of your models to get the other one.

== Solution



#pagebreak()

= Problem 3
The _dual_ of a sentence is obtained by switching its two nouns. For example the dual of All $p$ are $q$ is All $q$ are $p$. We denote the dual of a sentence $phi$ by $phi^dag$. If $Gamma$ is a set of sentences, by $Gamma^dag$ we mean the
obvious thing, i.e., “take the dual of each sentence in $Gamma$.”

Suppose $Gamma prov phi$. Prove that $Gamma^dag prov.not phi^dag$ by induction on proof trees. Be very careful and deliberate, so that
I know you understand what you're doing.
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