#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         "Lecture 2: Recap",
  author:        "Simon Holm",
  course:        "AI508 — Algorithmic Game Theory",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

= Probability
Serves to understand outcomes with some probability

#definition(title: "Definition: Sample space")[
  The space which an event can hold.
]

#example(title: "Example: Different sample spaces")[
  - Coins $Omega = {H,T} $
  - Die $Omega = {1,2,3,4,5,6}$
  - Real values $Omega = RR$
]

#definition(title: "Definition: Event")[
  An Event is a subset of the sampling space.
]
#example(title: "Example: Different events")[
  Given that $A$ is an event in $Omega$ such that $0 <=P(A) <= 1$, then $P(Omega) = 1$ and $P(phi) = 0$
]

Remember that 
$ P(A^C) = 1-P(A) $
$ P(A union B) = P(A) + P(B) - P(A inter B) $

#pagebreak()

== Random variables
#definition(title: "Definition: Random Variable")[
  The random variable $X$ is a function that maps
  $ cal(X) : Omega -> RR $

  $ EE[X] = sum_(x in cal(X)) x dot P(cal(X)=x) $

]

#example(title: "Example: Coin")[
  Two coins giving heads
  $ cal(X) = {0,1,2} $
]

#example(title: "Example: Discrete random variable")[
  Given that $Omega = [0,1]$, then $ X(omega) = cases(-1 quad & w <=1/2, 1  & w > 1/2) $
]

#definition(title: "Definition: Probability mass function")[
  Given $cal(X)$
  $ P(cal(X)=x) = p_x $
]


#definition(title: "Definition: Joined distribution")[
  Given $cal(X),cal(Y)$
  $ P(cal(X)=x,cal(Y)=y) = p_(x y) $
]

#definition(title: "Definition: Marginalization")[
  Given $cal(X),cal(Y)$
  $ P(cal(X)=x) = sum_(y in cal(Y)) P(cal(X)=x,cal(Y)=y) $
]
#definition(title: "Definition: Bernoulli")[
  Given that $cal(X) = cases(1 quad &p, 0 &1-p)$

  So that $EE[cal(X)] = 1p + 0(1-p) = p$
]

#definition(title: "Definition: Variance")[
  Variance captures the spread of value around the
  $ Var[cal(X)] = EE[(cal(X) - EE[cal(X)])^2] $
]

#example(title: "Example: Variance on bernoulli")[
  Given $X$,
  $ Var[cal(X)] = sum_(x in cal(X)) (x-mu)^2 P(cal(X)=x) $
]


#definition(title: "Definition: Covariance")[
  Given $cal(X), cal(Y)$,

  $ Cov[cal(X), cal(Y)] &= EE[(cal(X)- EE[cal(X)]) (cal(Y) - EE[cal(Y)])] ) \ 
    &= EE[cal(X) cal(Y) + EE[cal(X)] EE[cal(Y)] - cal(X)EE[cal(Y)] - cal(Y) EE[cal(X)]] \
    &= EE[cal(X)cal(Y)] + EE[cal(X)]EE[cal(Y)] - EE[cal(X)] EE[cal(Y)] - EE[cal(Y) EE[cal(X)]] \
    &= EE[cal(X)cal(Y)] - EE[cal(X)]EE[cal(Y)] = Cov[cal(X)] $
]
#pagebreak()

== Conditional probability
Assume an implicit sample space.

#definition(title: "Definition: Conditional probability")[
  Assume there are 2 events $A$ and $B$ and let $P(B)>0$
  
  Then the probability of event $A$ given $B$ is:
  $ P(A mid B) = P(A inter B)/P(B) $
]



#example(title: "Example: Conditional probability")[
  Given $B={x>3} = {4,5,6}$

  The probability that $A="EVEN" = {2,4,6}$ is:
  $ P(A inter B)/P(B) = (2/6)/(3/6) = 2/3 $
]

#definition(title: "Definition: Bayes Rule")[
  Given $A,B$
  $ P(A mid B) = (P(B mid A) P(A))/P(B) $
  Where $P(B mid A)$ is the _likelihood_, $P(A)$ is the _prior_ and $P(B)$ is the _marginal evidence_
]

#definition(title: "Definition: Independent events of a random variable")[
  Event $A$ and $B$ are independent if anf only if
  $ P(A inter B) = P(A)P(B) $
]

#example(title: "Example: Given that " + $B>0$)[
  Given that $B>0$ then
  $ P(A mid B) = (P(B mid A) P(A))/P(B) = (P(A)P(B))/P(B) = P(A) $
]
#pagebreak()

#definition(title: "Definition: Independence of 2 random variables")[
  2 R.V's $X,Y$ are independent iff
  $ P(cal(X)=x,cal(Y)=y) = P(cal(X)=x)P(cal(Y)=y) quad forall x,y in (x in cal(X), y in cal(Y))  $
]

#definition(title: "Definition: Uncorrelation of 2 random variables")[
  2 R.V's are uncorrelated iff $ Cov(cal(X),cal(Y)) = 0 iimp EE[cal(X)cal(Y)] = EE[cal(X)]EE[cal(Y)] $
]

#example(title: "Example: Independence and Uncorrelation")[
  Note that
  $ "Independence" iimp "Uncorrelation" $
]

= Conditional expectation
Note that conditional expectations are not scalars,

#definition(title: "Definition: Conditional expectation")[
  Given $cal(Y)$
  $ EE[cal(X) mid cal(Y) = y] = sum_(x in cal(X)) x dot P(cal(X) = x mid cal(Y) = y) $
]

#theorem(title: "Theorem: Law of Total Expectation")[
  Given $X$ and $Y$,
  $ EE[cal(X)] &= EE[EE[cal(X) mid cal(Y) = y]] \ &= sum_(y in cal(Y)) (sum_(x in cal(X) ) x dot P(cal(X) = x mid cal(Y) = y)) P(cal(Y) = y) $

]

#pagebreak()

= Inequalities
#definition(title: "Definition: Markov's inequality")[
  Let $cal(X)$ be a non negative random variable, then.
  $ P(X<=a) <= EE[cal(X)]/a $
]

#proof(title: "Proof of Markov's inequality")[
  Let 

  $ EE[cal(X)] &= sum_(x>=0) x dot P(cal(X)=x) \
  &= sum_(x=0)^a x dot P(cal(X)=x) + sum_(x>=a) x dot P(cal(X)=x) \
  &= sum_(x>=a) x dot P(cal(X)=x) >= a sum_(x>=a) P(cal(X)=x) = a EE[cal(X)]  
  
  $
]


