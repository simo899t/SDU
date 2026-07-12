#import "@local/tempst:0.1.0": *
#show: exam.with(
  title:         "Take Home Exam 2024",
  subtitle: "Group 15",
  course:        "DM500 — Studyintroduction to Artificial Intelligence and Computer Science",
  author:        ("Simon Holm - sihol24", "Johannes Rothe - jorot24", "Anne Sofie Høj - anho024"),
  date:          "November 17th 2024",
  university:    "University of Southern Denmark",
  outline:       true,
  outline-depth: 2,
)

= Introduction
The objective of this assignment is the get to know #typst as a tool for assignment writing. The following presents some exercises written in #typst

= Reexam february 2015
== Task 1
Let $U = {1,2,3,4 dots, 15}$ be the universe
Then let $ A = {2n | n in S} quad B 0 {3n + 2|n in S} $
Where $S = {1,2,3,4}$

Declare all elements in the following sets

#set enum(numbering: "a)")
+ $A$
+ $B$
+ $A cap B$
+ $A cup B$
+ $A - B$
+ $obar(A)$

=== Solution
+ ${2,4,6,8}$
+ ${5,8,11,14}$
+ ${8}$
+ ${2,4,6,5,8,11,14}$
+ ${2,4,6}$
+ $obar(A) = U^(without A) = {1,3,5,7,9,10,11,12,13,14,15}$

#pagebreak()

== Task 2

1. Which of the following statements are true?
  #set enum(numbering: "1.")
  + $forall x in NN: exists y in NN: x<y$   
  + $forall x in NN: exists! y in NN: x<y$
  + $exists y in NN: forall x in NN: x<y$
  #set enum(numbering: "a)")
1. Negate proposition 1 from question $a)$, without the use of $not$
=== Solution

1. #set enum(numbering: "1.")
  + $forall x in NN: exists y in NN: x<y to top$   
  + $forall x in NN: exists! y in NN: x<y to bot$
  + $exists y in NN: forall x in NN: x<y to bot$
  #set enum(numbering: "a)")

1. Negate proposition 1 from question $a)$, without the use of $(not)$
$ exists x in NN: forall y in NN: x>=y $  

#pagebreak()

== Task 3
Let $R, S$ and $T$ be binary relations on the set ${1,2,3,4}$

#set enum(numbering: "a)")
+ Let $R = {(1,1), (2,1), (2,2),(2,4),(3,1),(3,3),(3,4),(4,1),(4,4)}$.
  
  Is $R$ a partial ordering?

+ Let $S = {(1,2),(2,3),(2,4),(4,2)}$.

  Compute the transitive closure of $S$.

+ Let $T = {(1,1),(1,3),(2,2),(2,4),(3,1),(3,3),(4,2),4,4}$. Notice that $T$ is an equivalence relation.

  Compute $T$'s equivalence classes


=== Solution
#set enum(numbering: "a)")
+ $R$ is reflexive since $(1,1),(2,2),(3,3),(4,4) in R$.

  $R$ is antisymmetric: for every pair $(a,b) in R$ with $a eq.not b$, $(b,a) in.not R$ (checking $(2,1),(2,4),(3,1),(3,4),(4,1)$, none of their reverses appear).

  $R$ is transitive: e.g. $(2,4),(4,1) in R arrow.r (2,1) in R$ and $(3,4),(4,1) in R arrow.r (3,1) in R$; all other compositions already lie in $R$.

  Since $R$ is reflexive, antisymmetric and transitive, $R$ *is* a partial ordering.

+ Computing the reachability closure of $S = {(1,2),(2,3),(2,4),(4,2)}$:

  $ S^+ = {(1,2),(1,3),(1,4),(2,2),(2,3),(2,4),(4,2),(4,3),(4,4)} $

+ Grouping elements related by $T$:

  $1$ and $3$: $(1,1),(1,3),(3,1),(3,3) in T$, so ${1,3}$ is a class.

  $2$ and $4$: $(2,2),(2,4),(4,2),(4,4) in T$, so ${2,4}$ is a class.

  Equivalence classes: $ {1,3} quad {2,4} $

Task