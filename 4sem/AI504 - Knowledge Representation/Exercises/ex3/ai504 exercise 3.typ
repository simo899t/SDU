#import "../../../../temp/temp.typ": *

#show: exercise.with(
  title: "AI 504 activity: Proving",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm"),
  date: "March, 2026",
  outline-depth: 1
)

= How to use  $ent$
- $underbrace(Gam ent phi, "entails"):= forall model. model ent Gam -> model ent phi$
- $underbrace(model ent phi, "satisfies"):= [|p|] psubset [|q|], ie forall x in [|p|], x in [|s|]$
  
- $underbrace(model ent Gam, "satisfies all"):= forall phi in Gam. model ent phi$


= Exercise 1.9 from book

See describtion from book (logic_from_language)

1. Show that $cal(N) ent Gam$

$ H_1: Gam ent term(p,q) = cal(N) ent Gam prov forall (term(r,s)) in Gam. cal(N) ent term(r,s) $
Let $term(r,s) in Gam (k)$. We show $cal(N) ent term(r,s), ie [|r|] psubset [|s|], ie forall x in [|r|], x in [|s|]$

$ H_2: Gam ent term(p,q), K = term(r,s) in Gam prov forall x in ip(r). x in ip(s) $

Let $x in [|r|], ie [|r|] = {*}, ie Gam prov term(p,r), ie exists cal(D) in Der_(Gam, term(p,r)) (L)$. 

We show $x in ip(s), ie ip(s) = {*}, ie Gam prov term(p,s), ie exists cal(D)' in Der_(Gam, term(q,s)) $


$ H_3: Gam ent term(p,q), K: term(r,s) in Gam, L: exists cal(D) in Der_(Gam,term(p,r)) prov exists cal(D)' in Der_(Gam, term(p,s)) $
$"We choose" cal(D)':= #figure(
  grid(
    columns: 2,
    align: horizon, 
    gutter: 1mm, 
    ptree(
      "all p are s",
      r($$, "all p are r "),
      r($$, "all r are s" )
    ),
    [BARBARA.]
  )
)$
$#figure(
  grid(
    columns: 2,
    align: horizon, 
    gutter: 1mm, 
    ptree(
      "all p are s",
      r($$, "all p are r "),
      r($$, "all r are s" )
    ),
    [BARBARA.]
  )
) in Der_(Gam,term(p,s)),\ ie term(p,r) in Der_(Gam, term(p,r)) "and" term(r,s) in Der_(Gam,term(r,s)).$

Obvious by $K$ and $L$.

2. Show that $cal(N) ent term(p,q)$ [Hint: Use part (1).]

We want to show that $cal(N) ent Gam => cal(N) ent term(p,q) $

$ H_4: Gam ent term(p,q) prov cal(N) ent Gam => cal(N) ent term(p,q) $

To prove $cal(N) ent Gam -> cal(N) ent term(p,q)$, we show that $cal(N) in "Models and" forall model in "Models"$.

$cal(N) ent Gam -> cal(N) ent term(p,q).$ Obvious by H
#pagebreak()

3. Use part (2) to show that $Gam prov term(p,q)$

$ H_5: Gam ent term(p,q) prov cal(N) prov term(p,q) \ =Gam ent term(p,q) prov ip(q) = {*} $
We prove that $ip(p) psubset ip(q) and ip(p) = {*}$:
- $ip(p) psubset ip(q)$, we know this by definition of satifiability
- $ip(p) = {*},ie Gam prov term(p,p)$ This is obvious by AXIOM.


