#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 10: ",
  course: "AI504 - Knowledge Represntation",
  date: "April/2026"
)
// content starts here


/*
Come by Siddarth's office or email him
*/

= Propositional logic

Remember...
- a signature is just a set ("sentence letters")
- if $P$ is a signature, a $P$-sentence is genereated by recursion as follows
$ (p or not q) <=> (r and p) $
These can be broken into
$ p, q, r $

#u("Base case"): a single

- letter $in P$ is a sentence.
- Booleam constants $(T,F)\/(top,bot)$


#u("Additionally")
- if $phi$ is a sentence then so is $not phi$
- if $phi$ and $psi$ are senteces, so are
  $ phi and psi, phi or psi, phi imp psi, phi bi psi  $

#definition(title: [A $P$-model],[
  A $P$-model is a function. $P-> {top,bot}$
  (sometimes called a truth assignment/valuation)
  $ model = (P, P -> {top, bot}) $
])

if $model$ is a model, and $phi$ is a sentence in the same signature, define in the same signature ($P$), $ip(phi) in {top,bot}$ as follows.

If $phi$ is a single letter $eg space p$, then $ip(phi)$ is defined by recursion on construction of $phi$ (where the base case $p in P$ is given by $model$)
== example
for $P = {p,q,r}$ and $model = {(p,top), (q,top), (r,bot)}$
where for $phi = (r and not p) or q$

then $ ip(phi) = ip(((r and not p) or q))  = ip(r and not p) or ip(q) = top $

= Tautologies
For "Does $emptyset ent (p or q) and (p or r) imp p or (q and r)$"

A general fact applies

Let $and.big Gam$ be the conjunction of all sentences in $Gam$ 

Then $Gam ent phi$ #u("is the same as") $and.big Gam imp phi$ being a tautology

= Contradictions
Similarly ${psi} ent F$

This means that every models satifying $psi$ must satisfy $F$ (contradiction)

= Logical equivalance
$phi$ and $psi$ are logically equivalant if for all models $model$, 
$ phi eq.triple psi $
$model ent phi bi model ent psi$
== equivalant formulations
  - $phi bi psi$ is a tautology
  - ${phi}ent psi "and" {psi} ent phi$

= Good to Remember
$ p or not p eq.triple top $
$ p and not p eq.triple bot $
$ not not phi eq.triple phi $
$ not (psi and phi) eq.triple (not psi) or (not phi) $
$ not (psi or phi) eq.triple (not psi) and (not phi) $