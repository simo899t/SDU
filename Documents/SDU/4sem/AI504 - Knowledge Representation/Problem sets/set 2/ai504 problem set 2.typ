#import "../../../../../../temp.typ": *

#assignment(
  title: "Problem set 2",
  course: "AI504 — Knowledge Representation",
  author: "Simon Holm",
  date: "March, 2026",
  outline-depth: 1
)

= Problem 1
Work over the set ${a, b, c, d}$ of nouns. Consider the set of sentences

$ Gamma = {"All" a "are" b, "All" a "are" c, "All" b "are" d, "All" c "are" d}. $

Write down two different proof trees verifying $Gamma prov$ All $a$ are $d$. Write your proof trees carefully and as legibly as possible.

== Solution

$ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
   - all $bold(a)$ are $bold(d)$
    - $underbrace("all "bold(a)" are "bold(b)"","axiom")$
    - $underbrace("all "bold(b)" are "bold(d)"","axiom")$
  
] $

$ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
   - all $bold(a)$ are $bold(d)$
    - $underbrace("all "bold(a)" are "bold(c)"","axiom")$
    - $underbrace("all "bold(c)" are "bold(d)"","axiom")$
  
] $

= Problem 2

Work over the set ${a, b, c, d}$ of nouns. In this question I want you to find *two different models of the same size*. Each of your models should satisfy all of the following sentences

$ "All"a "are" b, "All" a "are" c, "All" b "are" d, "All" c "are" d $

and falsify both of the following sentences

$ "All" b "are" c, "All" c "are" b. $

_Note_. We have to be a little careful in saying what it means for two models to be different. But basically
if you label the elements of your model $1, 2, dots, n$ you should not be able to relabel (i.e., permute) the elements of one of your models to get the other one.

= Problem 3
The _dual_ of a sentence is obtained by switching its two nouns. For example the dual of All $p$ are $q$ is All $q$ are $p$. We denote the dual of a sentence $phi$ by $phi^dag$. If $Gamma$ is a set of sentences, by $Gamma^dag$ we mean the
obvious thing, i.e., “take the dual of each sentence in $Gamma$.”
Suppose $Gamma prov phi$. Prove that $Gamma^dag prov.not phi^dag$ by induction on proof trees. Be very careful and deliberate, so that
I know you understand what you’re doing.

= Problem 4

= Problem 5