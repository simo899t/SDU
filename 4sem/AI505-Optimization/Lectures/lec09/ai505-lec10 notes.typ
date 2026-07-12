#import "@local/tempst:0.1.0": *
#show: note.with(
  title: "Lecture 10: Population Based Methods",
  author: "Simon Holm",
  date: "March - 2026"
)

// Your content starts here

= Sampling Plans

In all nonlinear non convex optimization, to generate good initial design points. With computationally costly functions, to create an initial set of design points from where to build a *surrogate model* to optimize in place of the original function

= Full Factorial Design
In the seach space $[a_1,a,2 .. b_1,b_2] (RR^2)$

Then we use uniform and evenly spaced samples across
domain (a grid) as shown on @fig:gridsearch

#figure(
  image("assets/image-9.png"),
  caption: [$a_i <= x_i <=b_i$ for each component $i$.\
  Grid with $m_i$ samples in the $i$th dimension]
)<fig:gridsearch>

In this space we can optimize over the points with the function $f(x) =a_0 + a_1 phi_1(x_1,x_2) + a_2 phi_2(x_1,x_2)$. This is known as *grid search*

- Sample count grows exponentially with dimension: $n^m$ and can be coarse and miss local features.

= Random Sampling
Uses pseudorandom number generator to define samples according to our desired distribution.

If variable bounds are known, a common choice is independent uniform distributions across domains of possible variable values $ [a_1,b_1] times dots times [a_n,b_n] $

Ideally, if enough points are sampled and the right distribution is chosen, the design space will
be covered

= Uniform Projection Plans
A uniform projection plan is a sampling plan over a discrete grid where the distribution over each dimension is uniform.
#figure(
  image("assets/image-10.png"),
  caption: [Example of a uniform projection plan where $p = [4,2,1,3,5]$]
)

= Stratified Sampling
Each point is sampled uniformly at random within each grid cell instead of the center

Cells are decided by Full Factorial or Uniform Projection Plans, and can capture details that regular-spaced samples might miss

#figure(
  image("assets/image-11.png"),
  caption: []
)

= Space Filling Metrics
A sampling plan may cover a search space fully, but still leave large areas unexplored

#figure(
  image("assets/image-12.png"),
  caption: []
)

== Discrepancy
*Discrepancy*: measure of ability of the sampling plan X to fill a hyper-rectangular design space

$ d(X) = supremum(cal(H)) abs((\#(X cap cal(H)))/(\#X)-lambda(cal(H))) $

#figure(
  image("assets/image-13.png"),
  caption: [$d$ for the purple rectangle is $>$ than $d$ for the blue rectangle]
)

== Pairwise Distances

$ d = root(q,x_(1,1)^q +dots) $

$ d = norm(arrow(x)_2 + arrow(x)_2) $

==  Morris-Mitchell Criterion
Alternative to previously suggested algorithm that simplifies the optimization problem

$ min_X max_(q in {1,2,3,10,20,50,100} Phi_q (X)) $

$ Phi_q(X) = (sum_i d_i^(-q))^(1/q) $

#figure(
  image("assets/image-14.png"),
  caption: [Uniform projection plans sorted from best to worst according to $Phi_1$]
)
= Space-Filling Subsets
Often, the set of possible sample points is constrained to be a subset of available choices

A space-filling metric for a subset S within a finite set X is the maximum distance between a point in X and the closest point in S, using a norm to measure distance
$ d_"max" (X,S) max_(x in X) min_(s in S) norm(s-x)_q $

#figure(
  image("assets/image-15.png"),
  caption: [Space-Filling Subsets]
)

= Quasi-Random Sequences
Also called *low-discrepancy sequences*, *quasi-random sequences* are deterministic sequences that systematically fill a space such that their integral over the space converges as fast as possible

- Used for fast convergence in Monte Carlo integration, which approximates an integral by
sampling points in a domain
Monte carlo integration: Error

#figure(
  image("assets/image-16.png",width: 30em),
  caption: [space-filling sampling plans in two dimensions. Samples are colored according to the order in which they are sampled. The uniform projection plan was generated randomly and is not optimized.]
)

== Book mentions
- Additive Recurrence: Recursively adds irrational numbers
- Halton Sequence: sequence of fractions generated with coprime numbers
- Sobol Sequence: recursive XOR operation with carefully chosen numbers

