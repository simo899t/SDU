#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 13: Exam preparation",
  course: "AI504 - Knowledge Representation",
  date: "May/2026"
)
// content starts here

/*
Come by Siddarth's office or via email
*/

/*
A B C
C A B
B C A
*/

= Latin squares

*types*:
- Set of cells $G = x_1, dots x_9$
- Set of rows $R$
- set of columns $C$
- Set of characters $A$ (alphabet)

One would need 3 functions
```hs
r :: Eq => G -> R
c :: Eq => G -> C
a :: Eq => G -> A
```
A model $model$ in this signature would be called a configuration.

$ forall x in R, forall y in A, exists z in G : r(z) = x and a(z) =y $
$ forall x in C, forall y in A, exists z in G : c(z) = x and a(z) =y $

How do we then express #u("uniqueness")?

$ forall z_1,z_2 in G : ((r(z_1) = r(z_2)  or c(z_1) = c(z_2) )and a(z_1) = a(z_2) imp z_1 = z_2) $

= First-order logic
Signatures:  in full generality these can consist of a set of #u("types") plus a set of #u("typed") function adn #u([relation symbols])

#example(title: [Example: Latin Squares],[
4 types $R,C,A,G$

plus function symbols
```hs
r :: Eq => G -> R
c :: Eq => G -> C
a :: Eq => G -> A
```
])

#pagebreak()


Model: I hav a domain or universe for each type, plus an interpretation of function & relation symbols, by actual functions and relations

#example(title: [Example],[
This signature has two types: types $a: X to bool,b: X times Y to bool$
])

#example(title: [Example],[
This signature has two types: types $a: X to bool,b: X times Y to bool$
])