#import "@local/tempst:0.1.0": *

#show: exercise.with(
  title: "AI 504 exam practice",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm"),
  date: "May, 2026",
  outline-depth: 1
)
#set heading(numbering: none)

= Multiple choice questions
== 1
+ $yes$
+ $yes$
+ $yes$
+ $yes$
+ $no$

== 2
+ $yes$
+ $no$
+ $no$
+ $no$
+ $no$
+ $no$

== 3
+ $yes$
+ $yes$
+ $yes$
+ $yes$

== 4
+ $yes$
+ $no$

== 5
+ $no$
+ $no$
+ $yes$
+ $yes$

== 6
+ $no$
+ $yes$
+ $no$
+ $no$

== 7
+ $no$
+ $no$
+ $no$
+ $yes$

== 8
+ $yes$
+ $yes$
+ $yes$
+ $no$

== 9
+ $yes$
+ $yes$
+ $no$
+ $no$

== 10
+ $no$
+ $no$
+ $no$
+ $yes$

== 11
+ $no$ 
+ $yes$
+ $no$
+ $no$

== 12
// not (n<2: t) = n < 2 : not t
+ $no$
+ $no$
+ $no$
+ $yes$

#pagebreak()
= Short-answer questions
== 1 
Let $phi$ be a contradiction such that  $phi = p and not p$

Then $ model ent not (p and not p) = not p or q quad top $
$ model^star ent p and not p quad bot $

$ model ent not phi bi model^star ent phi quad bot $ 
== 2
#let tr = $#text(font: "PT Sans")[tree]$

Let $Gam, phi$ be arbitrary. Prove $Gam prov phi$ them $Gamd prov phid$

Let $Gam prov phi$
This means that there exists a prooftree that proves $phi$ by $Gam$. 
$ exists cal(D) in tr_(sans(T),phi) : Gamd prov phid $

$ (forall cal(D) in tr_(sans(T),phi)). space  Gam prov phi imp Gamd prov phid $

== 3
No 

given that $model_1 ent (p,q) $ and $model_2 ent (q,not p)$
Then $ model_3 ent.not (p,not p) $

== 4
$p imp q$

== 5
#let see = $#[see]$
P = {p,q}
$ Gam = {sent(p,q), sent(q, (term(see, p)))} $
$       ip(p) = {p}       $
$      ip(q) = {q,p}      $
$ ip(see) = {(q,p), (p,)} $


#pagebreak()
== 6
if $t = p$, then $ f(t) = t(x) $
if $t = term(v, t)$, then$ F(t) = (forall y) (f(T) -> r(x,y)) $

#pseudo[
  *Proof by induction* 
  - *Goal:* For any term $t: ip(t) = ip(f(t))$. 
  + *$underline("Base case")$*
    + Let $t = p$
    + Since $ip(p(x))$ is the set of all element that satisfy $p(x)$,
    + then $ ip(p) = ip(f(p)) =  ip(p(x)) $
  + *$underline("Inductive hypothesis")$*
    + Assume that for any subterm $t^prime$ $ip(t^prime) = ip(f(t^prime))$
  + *$underline("Inductive step")$*
    + Let t be an arbitrary term on the form $t= term(r,t^prime)$, then
    + $ ip(t)) = {x in M: forall y in ip(t^prime). space (x,y) in ip(r)} $
    + $ ip(f(t)) = {x in M: forall y in ip(f(t^prime)). space (x,y) in ip(r)} $
    + By $IH$ 
    + $ ip(t) = ip(f(t)) $
  ]


== 7
$ ip(z) = emptyset $

$ ip(term(ell,z)) = ZZ $

$ ip(term(ell, term(ell,z))) = {x in ZZ : forall y in ip(term(ell,z)). space (x,y) in ip(r)} = emptyset $
It flips

#pagebreak()
== 8
#definition(title: [Definition of tableau rules for $imp$ and $bi$],[
  
  #figure(  // large figure for the rules of tableau expansion
    grid(columns: 2,
  rows: 2,
  column-gutter: 2em,
  row-gutter: 2em,
tree(
  shape: "rectangle",
  spacing: (40pt, 20pt),
  node-inset: 4pt
)[
  - $T:alpha imp beta$
    - $T:not alpha or beta$
      - $T: not alpha$
        - $F: alpha$
      - $T: beta$
],  
tree(
  shape: "rectangle",
  spacing: (40pt, 20pt),
  node-inset: 4pt
)[
  - $T: alpha bi beta$
    - $T:alpha imp beta and beta imp alpha$
      - $T:alpha imp beta, beta imp alpha$
        - $T: not alpha or beta, not beta or alpha$

]
  ), gap: 1.5em, caption: [Rules for tableau expansion (propositional logic)]
  )

])

== 9
$ ((p imp q) imp p) imp p $
#figure(
  tree(
  shape: "rectangle",
  spacing: (40pt, 20pt),
  node-inset: 4pt
)[
  - $T:((p imp q) imp p) imp p$
    - $T:not ((p imp q) imp p) or p$
      - $T: not ((p imp q) imp p)$
        - $T: not (not (p imp q) or p)$
          - $T: (p imp q) and not p$
            - $T:not p or q , not p$
              - $T: not p quad$
      - $T: p$
]
)
This mean that for $((p imp q) imp p) imp p$ to be true then $p or not p$ this is a tautology




== 10
If $(forall x,y) (L(x,y) imp L(y,x))$, then $(forall x,y) (L(y,x) imp L(x,y))$

That means $ (forall x,y) (L(x,y) bi L(y,x)) $ 


== 11
$ model = (emptyset , ip(space)) $

== 12
#let test = "test"
$ exists n in NN, forall x in {y in NN | y > n}. test(x) $


== 13
$ x,y,z in RR. exists x in NN. x<y<z $

$ forall x,y,z in ZZ. exists n,m,k in ZZ. 
#align($(x + 360n) &- (z + 360k) < 360 and \
        (z + 360k) &< (y + 360m) and \
        (y + 360m) &< (x + 360n) and \ $) $

$ (forall x,y in ZZ) (exists z in ZZ) : (x mod 360) <= (z mod 360) <= (y mod 360) $

$ (forall x,y in ZZ) (exists z,n,m,k in ZZ) : (x +n 360) <= (z + m 360) <= (y + k 360) $

= Build your own logic

== 1
#let isa(a,b) = $#a "is a" #b$

(a) $model = {Gam, ip(space)}$
$ forall p in P: ip(p)_model subset M $
$ forall n in N: ip(n)_model subset M $
Let $ model ent phi $
$ phi = cases(ip(p)_model subset ip(q)_model iff phi = sent(p,q),
              ip(n)_model in ip(p)_model iff phi = isa(n,p)) $

(b)
#figure(
  prooftree(
    rule(
      name: "ISA",
      $isa(n,p) $, $sent(p, q)$,
      $isa(n, q)$,
    )
  )
)
- Soundness
Given the definition of a sentence $phi$


$ ip(n)_model in ip(p)_model subset ip(q)_model \
  ip(n)_model in ip(q)_model $
== 2

== 3

== 4
#set enum(numbering: "1.")
(a) 
1. _fewer things_, since $ent^s$ is smaller
2. 
$ sent(p, term(r,q)) <=> sent(q, term(r,p)) $