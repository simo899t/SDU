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

$ Gam = {allare(a,b), allare(c,d), allare(a,c), allare(a,e), allare(c,e)} $

Then $Gam ent.not allare(d,b)$. The point of this problem is to give two models of Γ where All d are b is false.
+ Find the canonical model of $Gam$, and check that $allare(d,b)$ is false in that model.
+ Find a model $model$ with just one element such that $model ent Gam$ but $model ent allare(d,b)$ [Hint: You can do this by modifying the model in Exercise $1.8$. That is, you use a model $model$ with $M = {*}$, and with the interpretation function given by something like $(1.8)$. The only difference is that we don't want $y$ on the right, we want $dots$. You can also get a one-point model this by using Exercise $1.9$ just below. On the other hand, some people might find Exercise 1.9 easier to think about after working on this exercise.]

== Solution
1. Let $M ={a,b,c,d,e}, "and" Gam = {allare(a,b), allare(c,d), allare(a,c), allare(a,e), allare(c,e)} $
  
  
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


  $ model ent allare(a,a) quad yes qquad model ent allare(b,a) quad no $
  $ model ent allare(c,a) quad no qquad model ent allare(d,a) quad no $
  $ model ent allare(e,a) quad no qquad model ent allare(a,b) quad yes $
  $ model ent allare(b,b) quad yes qquad model ent allare(c,b) quad no $
  $ underline(model ent allare(d,b) quad no) qquad model ent allare(e,b) quad no $

  $ model ent allare(a,c) quad yes qquad model ent allare(b,c) quad no $
  $ model ent allare(c,c) quad yes qquad model ent allare(d,c) quad no $
  $ model ent allare(e,c) quad no qquad model ent allare(a,d) quad yes $
  $ model ent allare(b,d) quad no qquad model ent allare(c,d) quad yes $
  $ model ent allare(d,d) quad yes qquad model ent allare(e,d) quad no $
  $ model ent allare(a,e) quad yes qquad model ent allare(b,e) quad no $
  $ model ent allare(c,e) quad yes qquad model ent allare(d,e) quad yes $
  $ model ent allare(e,e) quad yes $

2. 
