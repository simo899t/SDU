#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         "Lecture 4: Sealed Bid Auctions",
  author:        "Simon Holm",
  course:        "AI508 — Algorithmic Game Theory",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

#let mi = $-i$

= Auctions
A typical auction is an "English auctions"

Today we will talk about "Sealed bid auctions"

#definition(title: "Definition: Sealed Bid Auctions")[
  An auction with 1 item, $n$ bidders

  Each individual bidder $i$ has a #underline[private] valuation $v_i$ of the item.
  
  At the time of bidding, each individual can at the same time bid on the item. After bidding a winner is chosen.
]

When designing a SBA, the following things should be considered
+ How should the design infer the amount the winning bidder pays
+ Simplicity of participation (strategic simplicity)
+ Computationally easy to run
+ $(b_1,b_2,dots,b_n) -> [n] where b_i = "bid by bidder" i$

== First Price Auction

The bidder with the highest bid gets the item by paying what they bid
$ u_i : (b_1,b_2,dots,b_n) -> RR $
$ u_i = cases(v_i -b_i &iif b_i>b_j quad forall j in [n] without {i}, 0 &ow) $

== Dominant strategies
Let $s$ be the strategy profile (set of strategies (e.g. bid) by each player)
$ s = {s_1,s_2,dots,s_n} $
Where $s_i$ be denotes the strategy of player $i$ and $s_(mi) = {s_(i), dots s_(i-1),s_(i+1),dots s_n}$ (all players besides $i$)

$ s_i in S_i; s_(mi) in S_(mi) = {S_1 times S_2 times S_(i-1) times S_(i+1) times S_n} $

$ u_i = S_1 times S_2 times S_m -> RR $

A strategy $s_i^*$ is a dominant $underbrace("(weakly)", "not "<)$ strategy for player $i$ if:
$ u_i (s_i^* , s_(mi)) >= u_i(s_i,s_(mi)) quad forall s_i in S_i, forall s_(mi) in S_mi $
#pagebreak()

== Second Price Auction
Highest bidder gets the item, but pays the amount of equal to the second highest bidder

$ B_i = max_(j != i) b_j $

$B$ would be the max bid, then utility is altered such that


$ u_i = cases(v_i - B_i &iif v_i > B_i,  0 &iif v_i <B_i) $

Then $ u_i (v_i,b_mi) >= u_j (b_i,b_mi) $
$v_i$ is a weakly dominant strategy for player $i$.