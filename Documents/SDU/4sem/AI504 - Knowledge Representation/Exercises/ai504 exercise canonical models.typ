#import "../../../../../temp.typ": *

#show: exercise.with(
  title: "AI 504 activity: Canonical models",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm"),
  date: "March, 2026",
  outline-depth: 1
)

= Problem 1
Work over the set of nouns $P = {p, q, r}$. Let $Gam = {"All" p "are" q, "All" p "are" r}$. Compute each of the following three sets:
- ${x in P: Gam prov "All" x "are" p}$
- ${x in P: Gam prov "All" x "are" q}$
- ${x in P: Gam prov "All" x "are" r}$

== Solution

Let set A,B,B be

$ A= {"All" p "are" p, "All" q "are" p, "All" r "are" p} $

The elements in $P$ that satisfies this is ${p}$.


$ B= {"All" p "are" q, "All" q "are" q, "All" r "are" q} $

The elements in $P$ that satisfies this is ${p,q}$.

$ C= {"All" p "are" r, "All" q "are" r, "All" r "are" r} $

The elements in $P$ that satisfies this is ${p,r}$.

= Problem 2
Now construct a $P$-model whose domain is also $P$, and where the interpretation of $p, q, r$ are the three sets above respectively. (Caution: $P$ plays two roles here: it is both the domain as well as the underlying signature. While this may be momentarily confusing, there is nothing wrong with it!) Draw your answer below.

== Solution
$model = (P,[||]:P->cal(P)(P))$

$ [|p|] := {p} $
$ [|q|] := {p,q} $
$ [|r|] := {p,r} $

#figure(
  venn(
  scale: 1.1cm,
  domain: $P$,
  ($A$, ("p",)),
  ($B$, ("p", "q")),
  ($C$, ("p", "r")),
), caption: $model$
)

= Problem 3
Look at your model. What sentences does it satisfy? Write them all below. Does that set contain

== Solution

We want to check if $forall x,y in {p,q,r} | model ent "All" x "are" y$

$model ent "All" p "are" p quad checkmark$

$model ent "All" p "are" q quad checkmark$

$model ent "All" p "are" r quad checkmark$

$model ent "All" q "are" p quad crossmark$

$model ent "All" q "are" q quad checkmark$

$model ent "All" q "are" r quad crossmark$

$model ent "All" r "are" p quad crossmark$

$model ent "All" r "are" q quad crossmark$

$model ent "All" r "are" r quad checkmark$

= Problem 4
Now go exactly through the same process, but for the set $Sigma = {"All" p "are" q, "All" r "are" p}$. Draw the
resulting model below.

$model = (P,[||]:P->cal(P)(P))$

$ [|p|] := {p,r} $
$ [|q|] := {p,q,r} $
$ [|r|] := {r} $

#figure(
  venn(
  scale: 1.2cm,
  domain: $P$,
  ($[|p|]$, ("p",)),
  ($[|q|]$, ("p", "q")),
  ($[|r|]$, ("p", "r")),
), caption: $model$
)




= Problem 5
Again, write the set of sentences that your model satisfies. Does it contain Σ as a subset?

== Solution

$model ent "All" p "are" p quad yes$

$model ent "All" p "are" q quad yes$

$model ent "All" p "are" r quad no$

$model ent "All" q "are" p quad no$

$model ent "All" q "are" q quad yes$

$model ent "All" q "are" r quad no$

$model ent "All" r "are" p quad yes$

$model ent "All" r "are" q quad yes$

$model ent "All" r "are" r quad yes$

= Problem 6
This construction is called the canonical model construction. It allows us to construct a model out of a set of sentences. Write another set of sentences below, and make it a little more complicated than the
previous examples—maybe 3-5 sentences over a signature of 4-5 symbols.

== Solution

Let $P ={p,q,r,t}, "and" Gam = {("All "p "are" q),("All" t "are" r),("All" r "are" q)} $

= Problem 7
Draw the canonical model for the set of sentences you just created. Once you do this, check whether
the model just created satisfies each one of those sentences.

== Solution

$model = (P,[||]:P->cal(P)(P))$

$ [|p|] := {p} $
$ [|q|] := {p,q,r,t} $
$ [|r|] := {r,t} $
$ [|t|] := {t} $

#figure(
  venn(
  scale: 1cm,
  domain: $P$,
  ($[|p|]$, ("p",)),
  ($[|q|]$, ("p", "q","r","t")),
  ($[|r|]$, ("r", "t")),
  ($[|t|]$, ("t")),
), caption: $model$
)

$model ent "All" p "are" p quad yes$

$model ent "All" p "are" q quad yes$

$model ent "All" p "are" r quad no$

$model ent "All" p "are" t quad no$

$model ent "All" q "are" p quad no$

$model ent "All" q "are" q quad yes$

$model ent "All" q "are" r quad no$

$model ent "All" q "are" t quad no$

$model ent "All" r "are" p quad no$

$model ent "All" r "are" q quad yes$

$model ent "All" r "are" r quad yes$

$model ent "All" r "are" t quad no$

$model ent "All" t "are" r quad yes$

$model ent "All" t "are" p quad no$

$model ent "All" t "are" q quad yes$

$model ent "All" t "are" t quad yes$

= Problem 8
Now let's move away from concrete examples and work abstractly. Suppose $P$ is any signature and $Gam$
is any set of $P$-sentences. Can you _define_ the canonical model $model_Gam$? Remember that to define a model
you need to specify a domain and, for each symbol in the signature, an interpretation of that symbol
as a subset of the domain.

== Solution

$  forall a in P, [|a|] :={x in P:Gam prov "All" x "are" a} $

= Problem 9
gain, let $P$ be any signature and $Gam$ be any set of $P$-sentences. Using your definition of the canonical
model $model_Gam$ from the previous problem, rigorously prove that $model_Gam ent Gam$; i.e., that $model_Gam$ satisfies every
sentence in $Gam$.

== Solution

$ model_Gam ent Gam (forall phi in Gam model_Gam ent phi) $
Let $phi in Gam$ so $exists p,q in P$. $phi = "All" p "are" q$

So then $[|p|] psubset [|q]$.

Let $ a in [|p|] = {x in P:Gam prov "All" x "are" a} $
So $a in P$ and $exists$ derication $cal(D)$ of All $a$ are $q$

= Solution 10
Let $P$ be any signature, $Gam$ be any set of $P$-sentences, and let $phi$ be any arbitrary $P$-sentence. Suppose that $model_Gam ent phi$. Prove that $Gam prov phi$.

== Solution

This is completeness


We know that
$ model_Gam ent phi $

This would contain somthing like

$ [|p|] psubset [|q|] $

where $ [|q|] = {x in P : Gam prov underbrace("All" x "are" q, phi)} $

This then mean thats $Gam prov phi$
