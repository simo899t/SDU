#import "../../../../../../temp.typ": *
#note(
  title: "Lecture 6: ",
  course: "AI504 - Knowledge Represntation",
  date: "16/03/2026"
)
// content starts here


/*
office hours:
Thursday afternoon
Chech itsLearning
*/

= Canonical models
For each set of sentences $Gam$ we have an associated cononical model $model_Gam$

Take $Gam= {(a,c), (b,d), (b,c), (c,b), (d,c), (d,e)}$

Where $P = {a,b,c,d,e}$

Lets construct a new model where the domain is the signature $P$. (the signature is the entire universe)

$ model_Gam = (P,[|" "|], P -> pow(P)) $

In this model $[|p|] = {x : Gam prov (x,p)}$

For this example then.

#align($
[|a|] &= {x: Gam prov "All" x "are" a} = {a}
\
[|b|] &= {x: Gam prov "All" x "are" b} = {a,b,c,d}
\
[|c|] &= {x: Gam prov "All" x "are" c} = {a,b,c,d}
\
[|d|] &= {x: Gam prov "All" x "are" d} = {a,b,c,d}
\
[|e|] &= {x: Gam prov "All" x "are" e} = {a,b,c,d,e}
$)

== Properties of canonical models
The canonical model only satisfy senteces, which $Gam$ proves. I.e., for any sentence $phi$, $ Gam prov phi <==> model_Gam ent phi $

e.g. In other model $cal(N)$, if $cal(N) ent Gam$ then i must satisfy any sentece that $Gam$ proves (by soundness).

#u("But") $cal(N)$ could also satisfy  other sentences too, not $model_Gam$. This #u("only") satisfies the senteces whcih $Gam$ proves.


== Example

$ Gam = {"All" a "are" b} $
#figure(
  grid(
    columns: 3,
    column-gutter: 1em,
    align: center + horizon,
    figure(
  venn(
  scale: 1cm,
  domain: $P$,
  ($a$, ("a",)),
  ($[|b|]$, ("a", "b")),
), caption: $model$
),
    text(size: 2em)[$"and"$],
    figure(
  venn(
  scale: 1cm,
  domain: $P$,
  ($a,b$, ("a,b",))
), caption: $"other model" cal(N)$
)
,
  )
)<venn-diag-1>

Here the model $cal(N)$ does satisfy $Gam$, while $Gam$ does not prove #u("any") sentence from $cal(N)$

Take the example $"All" b "are" a$. This is not proven by $Gam$

#pagebreak()

= Proof of completeness
Assume $Gam ent phi$.

This means that every model satisfying $Gam$ must sastify $phi$. Since $model_Gam$ satisfyes $Gam$ by definition. Then $ model_Gam ent phi $

We know that $model_Gam$ only satisfies senteces which $Gam$ proves. Since $model_Gam ent Gam$, then $ Gam prov phi $
#QED

= Soundness & Completeness
Says $prov$ and $ent$ agrees. I.e., for any $Gam, phi$ then $ Gam prov phi <==> Gam ent phi $


== Explanation
For any $Gam$ and $phi$, either $Gam prov phi$ (there exists a proof), or $Gam ent.not phi$ (there exists a counter-model. i.e a model which satisfies $Gam$ but falsifies $phi$)

== Algorithmic question
Given $Gam$ and $phi$, either
- find a proof of $phi$ from $Gam$

*or*

- find a model satisfying $Gam$ but not $phi$