#import "../../../temp/temp.typ": *

#show: exercise.with(
  title: "AI 504 exam practice",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm"),
  date: "May, 2026",
  outline-depth: 1
)
#set heading(numbering: none)
= 1
#let AtSen = "AtSen"
For atomic sentences
$ M^* = v^* : AtSen -> BB | v^*(x) = cases(top iff v(x) in AtSen = bot,
                                           bot iff v(x) in AtSen = top)  $
this means that $ v(x) = v^*(not x) $

But for all sentences
Let $phi = p xor q$

For this
$ v ent.not not(p xor q) bi v^* ent p xor q qquad bot $

= 7
#align($
(a) & quad (exists x,y) (x = y) quad bot \
(b) & quad (forall x,y) (x != y) quad bot \
(c) & quad (forall x)(exists y) (x != y) quad bot \
(d) & quad (exists x,y) (x != y) quad top
$)

= 8
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

= 12
#let test = "test"
$ exists n in NN, forall x in {y in NN | y > n}. test(x) $
$ exists n in NN, forall x in <n test(x) $

= 13
$ x,y,z in RR. exists x in NN. x<y<z $

$ forall x,y,z in ZZ. exists n,m,k in ZZ. 
#align($(x + 360n) &- (z + 360k) < 360 and \
        (z + 360k) &< (y + 360m) and \
        (y + 360m) &< (x + 360n) and \ $) $

