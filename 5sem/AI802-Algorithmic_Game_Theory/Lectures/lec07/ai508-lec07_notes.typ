#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         ("Lecture 7: Auction Activity"),
  author:        "Simon Holm",
  course:        "AI508 — Algorithmic Game Theory",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)
#let rev = "Rev"
= Recall
Revenue maximization
$ rev(r) = r(1-F(r)) $
$ dd(r) rev(r) = 1-r f(x) - F(x) iimp r^* =(1-f(r^*))/F(r^*) $

We then derived $phi$ $st$
$ phi(v) = v - (1-F(v))/f(v) = PP(V >= v)/f(v) qquad #comment[(virtual welfare)] $
This means that $ phi(r^*) = 0, " and " phi(v) <=v $

= cont. Revenue maximization
By the definition of $phi$:
$ dd(r) rev(r) &= -f(r)phi(r)\
  integral_(r^*)^(macron(v)) dd(r) rev(r) dif r &= -integral_(r^*)^(macron(v)) f(r)phi(r) dif r \
  rev(macron(v)) - rev(r^*) &= -integral_(r^*)^(macron(v)) f(r)phi(r) dif r \
  rev(r^*) &=  integral_(r^*)^(macron(v)) f(r)phi(r) dif r \
  integral_(0)^(macron(v)) phi(r) bb(1){v <=r^*} f(r) dif r &= EE[phi(r) bb(1){v <=r^*}] qquad #comment[(Zeroes the integral below $r^*$)]
   $


Given the allocation rule
$ X_(r^*) (v) = cases(1 &iif v >= r^*, 0 &ow) $
Then $ rev(r^*) = EE_(v tilde F) [phi(v) X_(r^*) (v)] $

Take $(x,p)$ from Myerson's Lemma

$ EE_(v_i tilde F_i) [p_i (arrow(v))] = EE_(v_i tilde F_i) [phi_i (v_i) X_i (arrow(v))] quad forall i space forall v_(-i) $
#pagebreak()
Then,
$ EE_(arrow(v) tilde F) [sum_(i=1)^(n) p_i (arrow(v))] = EE_(arrow(v) tilde F) [sum_(i=1)^(n) phi_i (v_i) X_i (arrow(v))] $

We now have that 
$ "Revenue Maximization" = arg max_x EE_(v tilde F) [sum phi_i (v_i) x_i (arrow(v))] $

This only work when $phi(v)>=0$ and when $phi$ is monotone.
Add the following constraint.

Given that $v_1 < v_2$
$ phi(v_1) <= phi(v_2) $
$ r^* = inv(phi)(0) $
#pagebreak()

= Exercises
#question(title: "1")[
Consider a single-item auction with at least three bidders. The highest bidder receives the item but pays the _third_-highest bid. Show that truthful bidding, is not a dominant strategy. 
]
#answer[

]

#question(title: "2")[
A seller has $k$ identical items and $n>k$ bidders. Each bidder wants at most one item. Design a generalization of the second-price auction for this setting. Specify its allocation and payment rules, and prove that truthful bidding, is a dominant strategy
]
#answer[
  For allocation
$ X_i = cases(1 &iif b_i in "topk"(B),
              0 & ow) $
  then for $b_1 >= dots b_k >= b_kplus$, all $k$ players pay $b_kplus$.
]

#question(title: "")[

]
#answer[
For allocation
$ X_i = cases(1 &iif b_i <= b_(-i),
              0 & ow) $

then for $b_1 <= dots b_k$, client pay $b_2$.
]