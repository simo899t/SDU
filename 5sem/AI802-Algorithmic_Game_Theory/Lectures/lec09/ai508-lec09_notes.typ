#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         ("Lecture 9: Routing"),
  author:        "Simon Holm",
  course:        "AI508 — Algorithmic Game Theory",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

= Contrast to auctions
While actions were mostly basted on system design, routing is morebasted on selfish decisions.

= Braess paradox

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
  caption: [A traffic system],
)
Given that 
$ o to v &: c(x)=x \ 
  o to w &: c(x)=1 \ 
  v to d &: c(x)=1 \ 
  w to d &: c(x)=x $
where $x$ is the fraction of total users on the road. so $x_"ov" + x_"ow" = 1$

The split should be $50\/50$ given that going up and down, results in the same cost of $c_T = 1.5$.

#pagebreak()

Now modify
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
  caption: [A modified traffic system],
)


And 
$ o to v &: c(x)=x \ 
  o to w &: c(x)=1 \ 
  v to d &: c(x)=1 \ 
  w to d &: c(x)=x \
  v to w &: c(x)=0 $

Then $ 2x <=1+x $
Given you've already paid $c = x$ the worst you can do is pay $x$ again, which cannot be worse than $1$

#definition(title: "Theorem: Price of Anarchy")[
  $ "PoA"= "Cost of the system at an equilibrium"/"Best/lowest system performance cost" $
]

#pagebreak()
= Pigov Network

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
  caption: [A _very_ complicated network],
)

Where
$ o to v to d &: c(x)=1 \ 
  o to w to d &: c(x)=x $

At equilibrium $x_"owd" = 1$, and total cost $=1$

Then total cost given $x_"split" = x_"owd" dot x_"owd" + (1-x_"owd") dot 1$

So $ dd(x_"owd") x_"split" = 0 iimp x^*_"owd" = 1\/2 $
Then the best cost for the system is $ 2 dot 1\/2 + (1- 1\/2) dot 1 = 3\/4 $
Then the PoA $= 1/(3\/4) = 4\/3$.

= supremum notation btw

Consider $c_p =x^p$, where $0<= p<= 1$
Then $ "PoA" = 1/(1-p/(p+1)^(1+1\/p)) $
As $p to 1$, then $"PoA" to 4\/3$

Assume that i want to maximize PoA. Since the max is not defined as $p<1$. We use $ sup "PoA" $ instead.
#pagebreak()
= Generalized Pigov
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
  caption: [Generalized Pigov],
)

with $ o to v to d &: c(r) #comment[fixed] \ 
  o to w to d &: c(x)#comment[function] $

Total traffic flow rate $r > 0$, then $ 0 <= x <= r$. Also by design $c$ as continuous and non decreasing, so $c(x) <= c(r)$.

Define
$ G = {"class of cost functions"} where c in G $
Where

$ alpha(G) &= sup_(c in G) sup_(r>0) (r dot c(r))/(display(min_(0<=x<=r) {x dot c(x) + (r-x) dot c(x)})) \
&= sup_(c in G) sup_(r>0) max_(0<=x<=r) (r dot c(r))/(x dot c(x) + (r-x) dot c(x)) $

Consider $ G_"quad" = {a x^2 + b x + d : a,b,d >= 0} $

Let $c(x) = x^2$, and $r=1$. Then

$ alpha(G) = sup_(c in G) sup_(r>0) 1/display(min_(0>=x>=1) x dot (x^2-1)+1) $

given that $arg min_(0>=x>=1) x dot (x^2-1)+1 iimp x^* 1/sqrt(3) $

$ alpha(G) = sup_(c in G) sup_(r>0) 1/(-2\/3sqrt(3)) = sup_(c in G) sup_(r>0) (3sqrt(3))/(-2) $

#pagebreak()


But we wish to show this for a generic quadratic cost function

$ alpha(G) = sup_(c in G) sup_(r>0) (r dot (a r^2 + b r + d))/display(min_(0>=x>=r) x dot (a x^2 + b x + d) + (r-x) dot (a x^2 + b x + d)) $

#theorem(title: "Key Theorem")[
  For every set $G$ of cost functions & for every selfish routing network with $ c_i in G quad forall i $
  Then $ "PoA" <= alpha(G) #comment[despite $alpha$ only defined on Pigov networks] $
]
