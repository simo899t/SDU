#import "../../../../temp/temp.typ": *

#show: assignment.with(
  title: "Problem set 3 - written part",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm", "Johannes Rothe", "Shuagib Ibrahim", "Anne Sofie Høj", "Daniel Nissen"),
  date: "March, 2026",
  outline-depth: 1
)

= Exercise 1.8
Here is a set $Gam$:

$ Gam = {all(a,b), all(c,d), all(a,c), all(a,e), all(c,e)} $

Then $Gam ent.not all(d,b)$. The point of this problem is to give two models of Γ where All d are b is false.
+ Find the canonical model of $Gam$, and check that $all(d,b)$ is false in that model.
+ Find a model $model$ with just one element such that $model ent Gam$ but $model ent all(d,b)$ [Hint: You can do this by modifying the model in Exercise $1.8$. That is, you use a model $model$ with $M = {*}$, and with the interpretation function given by something like $(1.8)$. The only difference is that we don't want $y$ on the right, we want $dots$. You can also get a one-point model this by using Exercise $1.9$ just below. On the other hand, some people might find Exercise 1.9 easier to think about after working on this exercise.]

== Solution
1. Let $M ={a,b,c,d,e}, "and" Gam = {all(a,b), all(c,d), all(a,c), all(a,e), all(c,e)} $
  
  
  Then $model = (M,[||]:M->cal(P)(M))$

  $ [|a|] := {a}       $
  $ [|b|] := {a,b}     $
  $ [|c|] := {a,c}     $
  $ [|d|] := {a,c,d}   $
  $ [|e|] := {a,c,d,e} $

  #figure(
  venn(
  scale: 1cm,
  domain: $P$,
  ($[|a|]$, ("a",)),
  ($[|b|]$, ("a","b")),
  ($[|c|]$, ("a", "c")),
  ($[|d|]$, ("a","c","d")),
  ($[|d|]$, ("a","c","e")),
  ), caption: [canonical $model$]
  )

#figure(
  euler-diagram(
    (
      ("P", $P$),        // universe — listed first, auto-contains everything
      ("a", $[|a|]$),
      ("b", $[|b|]$),
      ("c", $[|c|]$),
      ("d", $[|d|]$),
      ("e", $[|e|]$),
    ),
    elements: (
      // (display, (set-keys...), (x, y))
      // Blobs for every listed set automatically expand to contain this element.
      ($a$, ("a", "b", "c", "d", "e"),  ( 0.5,  0.8)),  // in all sets → central overlap
      ($b$, ("b",),                       ( 2.4,  0.3)),  // only in b
      ($c$, ("c", "d", "e"),              ( 0.3, -1.5)),  // in c ∩ d ∩ e
      ($d$, ("d",),                       (-1.5, -2.3)),  // only in d
      ($e$, ("e",),                       (-1.8,  0.4)),  // only in e
    ),
  ),
)







#pagebreak()


  $ model ent all(a,a) quad yes qquad model ent all(b,a) quad no $

  $ model ent all(c,a) quad no qquad model ent all(d,a) quad no $

  $ model ent all(e,a) quad no qquad model ent all(a,b) quad yes $

  $ model ent all(b,b) quad yes qquad model ent all(c,b) quad no $

  $ underline(model ent all(d,b) quad no) qquad model ent all(e,b) quad no $

  $ model ent all(a,c) quad yes qquad model ent all(b,c) quad no $

  $ model ent all(c,c) quad yes qquad model ent all(d,c) quad no $

  $ model ent all(e,c) quad no qquad model ent all(a,d) quad yes $

  $ model ent all(b,d) quad no qquad model ent all(c,d) quad yes $

  $ model ent all(d,d) quad yes qquad model ent all(e,d) quad no $

  $ model ent all(a,e) quad yes qquad model ent all(b,e) quad no $

  $ model ent all(c,e) quad yes qquad model ent all(d,e) quad yes $

  $ model ent all(e,e) quad yes $

2. 
