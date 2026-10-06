#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         ("Lecture 10: Routing cont."),
  author:        "Simon Holm",
  course:        "AI508 — Algorithmic Game Theory",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)
#let cost = "Cost"
= Recall
$ alpha(G) = sup_(c in G) sup_(r>0) max_(0<=x<=r) (r dot c(r))/(x dot c(x) + (r-x) dot c(x)) $

$ #comment[$<=r$ is actually irrelevant since the fraction \ becomes tiny when $x>>r$ me] $

where $c:$ is continuous and nonnegative

#theorem(title: "Key Theorem")[
  For every set $G$ of cost functions & for every selfish routing network with $ c_i in G quad forall i $
  Then $ "PoA" <= alpha(G) #comment[despite $alpha$ only defined on Pigov networks] $
]

= Paths and traffic
#graph(
  arrow: "->",
  nodes: (
    (pos: (0,0), label: $o$),
    (pos: (1,1), label: $w$),
    (pos: (2,0), label: $d$),
    (pos: (1,-1), label: $v$)
  ),
  edges: (
    ((0,0), (1,1)),
    ((1,1), (2,0)),
    ((0,0), (1,-1)),
    ((1,-1), (1,1)),
    ((1,-1), (2,0))
  ),
  caption: [A modified traffic system \ $ "Directed": (V,E) $],
)


And 
$ o to v &: c(x)=x \ 
  o to w &: c(x)=1 \ 
  v to d &: c(x)=1 \ 
  w to d &: c(x)=x \
  v to w &: c(x)=0 $

With $r$ unit for total demand going from $o to d$
#pagebreak()

For every given path $p$, $f_p$ denotes the fraction of traffic using path $p$, where
$ sum_(p in cal(P)) f_p = r $

Given an edge $e$ then, $ f_e = sum_(p in cal(P) : e in p) f_p $

#example(title: "Example: computing "+$f_e$)[
  Given $g:$
  #graph(
  arrow: "->",
  nodes: (
    (pos: (0,0), label: $o$),
    (pos: (1,1), label: $w$),
    (pos: (2,0), label: $d$),
    (pos: (1,-1), label: $v$)
  ),
  edges: (
    ((0,0), (1,1)),
    ((1,1), (2,0)),
    ((0,0), (1,-1)),
    ((1,-1), (1,1)),
    ((1,-1), (2,0))
  ),
  caption: [A modified traffic system \ $ g: (V,E) $],
)

  Where $ o to v to d &: 1\/4 \ 
  o to w to d &: 1\/4 \
  o to v to w to d &: 1\/2 \ 
  $

  Then $ f_((o, v)) = sum_(p in cal(P) : e in p) f_p = 3/4 $
]
#pagebreak()
= Equilibrium flow 
#definition(title: "Definition: Equilibrium flow")[
  $A$ flow $f$ is said to be an equilibrium if $ f_hat(p)^("eq") > 0 iimp hat(p) in arg min_(p in cal(P))space {sum_(e in p) c_e (f_e)} $
]

#example(title: "Example: Optimal cost")[
  Given a Braess network $g$
  #graph(
  arrow: "->",
  nodes: (
    (pos: (0,0), label: $o$),
    (pos: (1,1), label: $w$),
    (pos: (2,0), label: $d$),
    (pos: (1,-1), label: $v$)
  ),
  edges: (
    ((0,0), (1,1)),
    ((1,1), (2,0)),
    ((0,0), (1,-1)),
    ((1,-1), (1,1)),
    ((1,-1), (2,0))
  ),
  caption: [A modified traffic system \ $ g: (V,E) $],
)

  Where $ o to v &: c(x)=x \ 
  o to w &: c(x)=1 \ 
  v to d &: c(x)=1 \ 
  w to d &: c(x)=x \
  v to w &: c(x)=0 $

  Is the flow shown above an equilibrium flow?

  Then 
  $ c_(o,v,d) (f) &= c_(o,v) (3/4)  + c_(w,d) (3/4) = 3/4 + 3/4 = 3/2 \
    c_(0,w,d) (f) &= c_(o,w) (1/4)  + c_(w,d) (3/4) = 1+3/4 $

  Since $3\/2 != 1+3\/2$, the flow is *not* an equilibrium.
]

#pagebreak()
= Optimal Flow
For any flow $f$
$ "Cost"(f) = underbrace(sum_(p in cal(P)) f_p dot c_p (f), "via paths") = underbrace(sum_(e in E) f_e dot c_e (f_e), "via edges") $

The optimal flow $f^*$ is then defined as
$ f^* in arg min_(f" feasible") "Cost"(f) $

and
$ "PoA" = cost(f^"eq")/cost(f^*) <= alpha(G) $

Take the equilibrium flow $f^"eq"$ and let $g$ be any feasible flow. The following inequality holds.
$ sum_(e in E) (g_e - f_e^"eq") dot c_e (f_e^"eq") >= 0 $

Then by the definition of $alpha(G)$,
$ alpha(G) >= (r dot c(r))/(x dot c(x) + (r-x) c(r)) $
and therefore $ x dot c(x) >= 1/alpha(G) dot r dot c(r) + (x-r) c(r) $
Then let $ c = c_e, quad r = f_e^"eq", quad x=f_e^* quad forall e in E $
then
$ f^*_e c_e (f_e^*) >= 1/alpha(G) f_e^"eq" c_e (f_e^"eq") + (f_e^* - f_e^"eq") c_e (f_e^"eq") \
cost(f^*) >= 1/alpha(G) cost(f^"eq") + underbrace(sum_(e in E) (f_e^* - f_e^"eq") dot c_e (f_e^"eq"), >=0) $
Since the sum on $r.h.s.$ is nonzero, it cannot be a contributing factor to the fact that
$ cost(f^*) >= 1/alpha(G) cost(f^"eq") $
#pagebreak()

= Over provisioned
Given a cost function $c$
$ c(x) = cases(1/(u_e -x) &iif x < u_e,
               +oo & ow) $
where $u_e > 0$ is the total capacity of an edgy

#definition(title: "Definition: Over provisioned")[
  Fix $beta in (0,1)$ A routing network is $beta$-over provisioned if at the equilibrium
  $ f_e^"eq" <= (1-beta) dot v_e quad forall e in E $
  Also saying. A network is overprovisioned if too much capacity is _unused_.
]

#example(title: "Example: Pigov example")[
  Let $g$ be a Pigov network
  #graph(
  arrow: "->",
  nodes: (
    (pos: (0,0), label: $o$),
    (pos: (1,1), label: $w$),
    (pos: (2,0), label: $d$),
    (pos: (1,-1), label: $v$)
  ),
  edges: (
    ((0,0), (1,1)),
    ((1,1), (2,0)),
    ((0,0), (1,-1)),

    ((1,-1), (2,0))
  ),
  caption: [A Pigov network for example],
  )

  Where
  $ o to v to d &: c(x)=1/(u-r) \ 
  o to w to d &: c(x)=1/(u-x) $

  Lets set  $r = (1-beta) dot u$, and bound PoA as following
  $ "PoA" <= 1/2 (1+1/sqrt(beta)) $

  As $beta to 1$ then $"PoA" to 1$

  As $beta to 0$ then $"PoA" to oo$
]



