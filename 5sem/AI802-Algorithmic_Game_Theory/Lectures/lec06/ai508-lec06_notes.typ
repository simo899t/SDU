#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         ("Lecture 6: Auction Activity"),
  author:        "Simon Holm",
  course:        "AI508 — Algorithmic Game Theory",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

= Revenue Maximization
Recall welfare and utility:
$ "welfare" = sum_(i=1)^(n) v_i x_i, quad "utility" = v_i x_i - p_i $

Now note sellers revenue
$ "sellers revenue" = sum_(i=1)^(n) p_i $

And that for _total utility_ (including seller) $ sum_(i=1)^(n) v_i x_i - p_i - sum_(i=1)^(n) p_i = sum_(i=1)^(n) v_i x_i $

*How can one maximize seller revenue?*
== Simplest case
Assume 1 item and 1 bidder.

Let the single bidders valuation $v from F$ (continuous on has a density $f$)

$ supp(F) = [0<=v<=obar(v)] $

We will sell the item if and only if $v >= r$ and the bidder pays $r$.

$ PP("making a sale") = PP(v >= r) = 1-F(r) $
Where $F(x) = integral_(-oo)^(x) f(x) dif x$

Then the sellers revenue is $r(1-F(r))$

Then $ r^* = arg max_r r(1-F(r)) $
$ pp(r) r(1-F(r)) = 1-F(x) - r F'(x) = 0 \ ==> r = (1-F(r))/(F'(x)) = (1-F(r))/(f(x)) $
#pagebreak()

== Virtual welfare
Add some lower bound for the bets such that 

$ "Welfare" = sum_(i=1)^(n) phi(v_i) x_i $


Where $ phi def v-(1-F(v))/f(v) #comment[virtual welfare] $
$v^*$ is a point where $phi(v^*) = 0$ is the revenue price.
