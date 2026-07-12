#let title = "Lecture 1: Introduction"
#let course = "AI504 - Knowledge Represntation"
#let date = "23/02/2026"

#import "@local/tempst:0.1.0": *

#note(
  title: title,
  course: course,
  date: date
)

#pagebreak()
// content starts here

= Introduction

Teacher - Siddharth Bhaskar (bhaskar\@imada.sdu.dk)

This course is about *declarative* (based on facts) knowledge and inference

+ Deductive inference
  + If I know something, what else can i conclude
  + Shoud be exact, i have to be sure that $a->b$
+ Inductive inference 
  + If I know something, then what explains it?
  + Is not as exact, a distribution might sorta explain some data, but we cannot decleratively conclude anything by it.

This course mainly focuses on deductive inference, by the study of different logics.

For each of these we then study the inference/consequence problem: "When does $Y$ follow from $X$" We thereofre study study the relationship between the model- & Proof-theoretic approaches. We wish to justlify that these are the same.

This course also implements solutions to the inference problem using ```hs Haskell``` #emoji.face.sad

= Deductive inference
How can I justify that $Y$ is a consequence of $X$? (follows from)

We commonly use 2 methods

== 1. Proof-theoretic
- I have a chain of reasoning starting with $X$ and concluding with $Y$

== 2. Model-theoretic
- There is no world where world where $X$ is true but $Y$ isnt

- You would generally need multiple assumptions for this to hold.