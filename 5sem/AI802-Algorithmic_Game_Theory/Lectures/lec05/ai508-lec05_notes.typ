#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         ("Lecture 5: Sealed Bid \n Auctions 2"),
  author:        "Simon Holm",
  course:        "AI508 — Algorithmic Game Theory",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

#let mi = $-i$

= Third Price Auction
Highest bidder gets the item, but pays the amount of equal to the third highest bidder

$ B_i def max_(j!=i) b_j $

where $ x_i (b_i) = cases(1 &iif b_i>B_i, 0 &iif b_i <B_i) $

#definition(title: "Definition: Welfare Maximization in sealed bid auctions")[
  Given 1 item, $x_i in {0,1}$ and $x$ is a valid allocation
  $ max_x sum_(i=1)^(n) v_i x_i $ 
]

= Sponsored Search (simplified)
Given $abs(J)$ ad slots ad $n$ bidders. Then
$ 1>=alpha_1>=dots>=alpha_abs(J)>=0 $
Each bidder has only 1 bid with value $v_i$ (value per click)

The $i^"th"$ bidder gets allocated $j^"th"$ slot 

$ x_i in {0,alpha_1,dots,alpha_abs(J)} $

#example(title: "Example: ")[
  given that $alpha_1 = 1, alpha_2 = frac(1,2,style: "skewed")$, and that other bids are $5$ and $8$. Then
  $ x_i(b_i) = cases(0 &iif b_i < 5, 
                     frac(1,2,style: "skewed") &iif 5 < b_i < 8, 
                     1 &iif b_i > 8) $
                     Here one can either get the best slot #emoji.face.stars, second best slot #emoji.face.neutral or no slot at all #emoji.face.sad.
]
#pagebreak()

= Mechanism
Given the utility function 

$ u_i =v_i x_i - p_i $

#definition(title: "Definition: Mechanism rules")[
  A mechanism in this sense is function that defines allocation rule $x$ and payment rules $p$.
  $ b |-> (x(b),p(b)) $

]
A mechanism is dominant strategy incentive compatible (DSIC) if 
$ underbrace(v_i x_i (v_i, b_(-i)) - p_i (v_i, b_(-i)),"truthful bidding") >= v_i x_i (b_i, b_(-i)) - p_i (b_i, b_(-i)) quad forall v_i, b_i, b_(-i) forall i $


#definition(title: "Definition: Monotone allocation")[
  For each player $i$, an allocation is monotone if for $b_i > b_i^prime$, it holds that
  $ x_i (b_i,b_mi) >= x_i (b_i^prime,b_mi) $
  If $b_i$ in increases you will have the _same or better_ allocation
]

#theorem(title: "Myerson's Lemma")[
  For $(x,p)$ DSIC $bii$ X is a monosome allocation
]

Then derived from Myersons' Lemma is:
$ p_i (b_i b_mi) = b_i x_i (b_i,b_mi) - integral_(0)^(b_i) x_i (z,b_mi) dif z $

Note that if you change the _true_ value DSIC still holds given that 

$ z x(z) - p(z) >=z x(y) - p(y) $
$ y x(y) - p(y) >=y x(z) - p(z) $