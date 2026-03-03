#let title = "Lecture 2: "
#let course = "AI504 - Knowledge Represntation"
#let date = "26/02/2026"

#import "../../../../../../temp.typ": *

#note(
  title: title,
  course: course,
  date: date
)

#pagebreak()

// 13-15 3. marts
// 9:30-11:30 4. marts

// content starts here

= The logic of "all $bold(p)$ are $bold(q)$"

- Fix a set (e.g. of nouns), e.g. ${"groundhogs, warthogs,hedgehos"}$
- once we fix a set of nouns, we can form sentences of the form "all $"___"$ are $"___"$"

- These sentences are neither true nor false until we give them meaning (we dont necessarily know what a groundhog or a hedgehog is), until we inteprete them with *model*.

== Model
 - A set of all underlying elements (_nouns_) in the univers/domain
 - Then we can map each element to a ground in the set.

$cal(M)$ usually refers to the model, while $M$ refers to its domain

Lowercase greek letters like $phi, psi$ usually refers to sentences

$Gamma$ is a set of senteces

Then $cal(M) ent phi $, means that the model $cal(M)$ $underline("satisfies")$ the sentece $phi$. 

Note that $ent$ is polymophic where

$ forall psi in Gamma | cal(M) ent psi, "if" Gamma ent phi => cal(M) ent phi    $

if any sentece in $Gamma$ satisfies the model $cal(M)$ then if $Gamma ent phi$, then $cal(M) ent phi$

#definition(
  [For $Gamma prov phi$, means that there is a proof of $phi$ fromn $Gamma$],
  title: [Definition of $prov$]
)


== Example (_noun example_)
Let $bold(P)$ be the underlying set of nouns 
A $bold(P)$ consists of of:
1. A set of $X$ (the domain or universe)

2. A function $P -> cal(P)(X)$ where $cal(P)$ is the powerset of $X$

Now let
- all $g$ are $w$
- all $w$ are $h$
- all $g$ are $h$

Note that these the transitive property that $g in w "and" w in h => g in h$

Because of this the following statement is not possible
$ g ent w, quad w ent h, quad g ent.not h $
#pagebreak()

let $Gamma = {("all" a "are" b), ("all" b "are" d), ("all" b "are" c), ("all" c "are" b), ("all" d "are" c), ("all" d "are" e)}$

Show that there are a "chain of reasoning" that uses three assumptions and deduces "all a are e"

$ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
   - all $bold(a)$ are $bold(e)$
    - all $bold(a)$ are $bold(d)$
      - all $bold(a)$ are $bold(b)$
        - $underbrace("all "bold(a)" are "bold(c),"axiom")$
        - $underbrace("all "bold(c)" are "bold(b)"","axiom")$
      - $underbrace("all "bold(b)" are "bold(d)"","axiom")$
    - $underbrace("all "bold(d)" are "bold(e)"","axiom")$
  
] $


== Induction on proof trees.

$ #tree(
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
   - all $bold(a)$ are $bold(e)$
    - $underbrace("all "bold(a)" are "bold(c)"","axiom")$
    - $underbrace("all "bold(c)" are "bold(d)"","axiom")$
    - $underbrace("all "bold(b)" are "bold(c)"","axiom")$
  
] $

Is there a proof of "all $a$ are $e$" from only these 3 axioms?

No, since none of the axioms contain $e$

If a element appears in the conclusion of a proof, it must appear in one of the leaves.

We use Induction on proof trees when i want to prove a statement about "all ..."

=== Proof

"Any element occuring in the roots of a tree must occur in some leaf."

#pseudo[
  *Proof by infuction*
  + *$underline("Base case")$*
    + tree is a single leaf
    + the root of the tree #underline[is] the whole tree, so it must occur in (the only) leaf
  + *#underline("Inductive step")*
    + Assume that for any statement $a R b$ 
    + then $a$ must come from $*$ (the left subtree) and $b$ must come from $+$ (the right subtree)
]
