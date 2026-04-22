#let title = "Lecture 5: "
#let course = "AI504 - Knowledge Represntation"
#let date = "26/02/2026"

#import "../../../../temp/temp.typ": *

#note(
  title: title,
  course: course,
  date: date
)
// content starts here


/*
office hours:
Tuesday 13.00-15.00
Thursday afternoon (look out for announcements) 
*/

= Recall

Recall the logic of "all $p$ are $q$"

Now fix an underlying set $P$
- a $P$-sentence is just a phrase "all $underbrace("all" p "are" q, (p,q)"-mathematically")$, where $p,q in P$"
- a $P$-model is a set $M$ and a function $P->cal(P)(M)$, if $p in P$, them $[|p|]psubset P$

== Terminology
$P$ is the #underline("signature") of a $P$-model or $P$-sentence

Suppose  $cal(M)$ is a $P$-model and "all $p$ are $q$" is $P$-sentense

Then $cal(M) ent "all" p "are" q$ means $[|p|] psubset [|q|]$

Suppose $Gam$ is a subset of senteces and $phi$ is a single sentences, them $Gam ent phi$ mean that for all models $cal(M)$, if $cal(M) ent Gam$, then $M ent phi$

= Proofs

A #underline("Proof") is a binary tree labeled by senteces such that at each non-leaf node
$ #tree(
  shape: "rectangle",
  reverse: true,
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
   - all $bluemath(bold(a))$ are $redmath(bold(c))$
    - all $bluemath(bold(a))$ are $greenmath(bold(b))$
    - all $greenmath(bold(b))$ are $redmath(bold(c))$
] $

Suppose that $T$ is aproof tree, then $T$ is a proof of $phi$ from $Gam$ if 
+ $phi$ labels the root of $T$
+ every leaf of $T$ either is in $Gam$ or has the form $underbrace("all _ are _", "blanks are equal")$

#definition(
  [$Gam prov phi$ if there is a proof tree of $phi$ from $Gam$],
  title: [Definition on $Gam prov phi$]
)

During this class and next class we want to prove $ Gam ent phi  <==> Gam prov phi $

Where $ underbrace(Gam ent phi ==> Gam prov phi, "Soundness "(prov "implies" ent)) $
and $ underbrace(Gam ent phi <== Gam prov phi, "Completeness "(ent "implies" prov)) $

= Soundness
#definition(
  [For any (set of senteces) $Gam$ and (sentence) $phi$, if $Gam prov phi$ then $Gam ent phi$],
  title: [Definition: Soundness]
)
For all $Gam, phi$ if there is exists a proof $phi$ from $Gam$, then for every model $cal(M)$ ig $cal(M) ent Gam$ then $M ent phi$

#theorem(
  [for all proofs $T, Gam$ and $phi$.
  
  If $T$ is a proof of $phi$ from $Gam$ then for every model $cal(M)$, 
  
  if $M ent Gam$ then $Gam ent phi$],
  title: [Message]
)

Now we can do induction to prove this, where #u("goal")

if $T$ is a proof of $phi$ from $Gam$ and $cal(M)$ is any model satisfying $Gam$ them $cal(M) ent phi$ too
#pagebreak()

#pseudo[
  *Proof by induction*

  - *#u("Base Case)") (Assume proof $T$ is a single leaf)*
    + Assume proof $T$ is a single leaf
    + Then $phi$ is the only thing in $T$, since $T$ is a proof from $phi$.
    + Notice that all the leaves from $T$ fomrs from $Gam$
    + Let $cal(M)$ be any model that satisfies (all sentences in) $Gam$
    + since $phi in Gam$, *$cal(M) ent phi$*
  + 
  - *#u("Inductive step")*
    + Suppose that $T$ is not a leaf
    + Decompose $T$ as follows. Say that $phi$ is "all $p$ are $q$"
  + Then there exists an $r$ s.t $T$ looks like
    - $ #tree(  
  shape: "rectangle",
  reverse: true,
  spacing: (40pt, 40pt),
  node-inset: 4pt
)[
   - all $bluemath(bold(p))$ are $redmath(bold(q))$
    - all $bluemath(bold(p))$ are $greenmath(bold(r))$
    - all $greenmath(bold(r))$ are $redmath(bold(q))$
] $
    - then let $T_0, T_1$ be the left & right havles of $T$ 
    - $T_0$ is a proof of "$"all" p "are" r$" from $Gam$ ($IH$ but explain better)
    - $T_1$ is a proof of "$"all" r "are" q$" from $Gam$ ($IH$ but explain better)
    - Suppose that $model ent Gam$.
    - By $IH$ applied to $T_0$, $model ent "all" p "are" r $
    - By $IH$ applied to $T_0$, $model ent "all" p "are" r $
    - Since $model ent "all" p "are" r$
    - $[|p|] psubset [|r|]$
    - Since $model ent "all" r "are" q$
    - $[|r|] psubset [|q|]$
    - Therefore, $ [|p|] psubset [|q|], "so" model ent "all" p "are" q $

]