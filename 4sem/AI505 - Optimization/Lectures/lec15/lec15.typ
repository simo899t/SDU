#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 15: Discrete Optimization",
  author: "Simon Holm",
  date: "April - 2026"
)

// Your content starts here

= Discrete Optimization
- Integer (linear) programming
- Exact solution techniques
  - Dedicated algorithms
  - Dynamic programming
  - Constraint programming
  - SAT solvers
- Approximation solution techniques
  - Approximation algorithms
  - Greedy algorithms and local search
  - Metaheuristics, Randomized optimization algorithms
  - Mathheuristics
  - $dots$ many more

= Discrete vs Combinatorial Optimization

In Combinatorial Optimization, variables have some combinatorial structure, ie, sets, permutations, paths

#definition(title: [Definition (Combinatorial Optimization Problem (COP))],[
  *Input:* Given a finite set $N = {1, dots ,n}$ of objects, wights $c_j$ for all $j in N$, a collection $cal(F)$ of feasible subsets of $N$
  *Task:* Find a minimum weight feasible subset, ie, 

  $ min_{S in cal(F)} sum_(j in S) c_j | S in cal(F) $
])

COP can also be modelled as discrete optimization problems.
Typically: *incidence vector* of $S, x^S in BB^n: x_j^S = mycases(1,j in S, 0, "otherwise", word: "if")$

#figure(
  image("assets/image.png")
)

= Integer Linear Programming (ILP)
#figure(
  image("assets/image-1.png"),
  caption: [Different kinds of integer linear programming]
)

== Mathematical Programming: Modeling
Find out exactly what the decision maker needs to know: (common to use binary variables $BB$, but not always)
- which investments to make
- which routes to take
- which items to pack
- which job $j$ should a person $i$ be assigned to?

== Rounding
One can relax a integer constraint to a linear one, solve the resulting linear program, and round the solution to an integer one. This is called *rounding*.
#figure(
  image("assets/image-2.png"),
  caption: [Rounding a linear program solution to an integer one can easily result in a very bad solution.]
)

== Cutting Planes
Lets tighten the linear programming relaxation by adding additional constraints, called *cutting planes*, that cut off the fractional solution but do not cut off any integer solutions.

#figure(
  image("assets/image-3.png"),
  caption: [Cutting planes can be used to tighten the linear programming relaxation.]
)

== Chvatal-Gomory’s Cutting Plane Algorithm
From the simplex we know that we can write the constraint $A x=b$ as 

#align($
A_B x_B^* + A_N x_N^* = b\
x_B^* + ceil(A_B^(-1)A_N) x_N^* = A_B^(-1) b\
x_B^* + ceil(A_B^(-1)A_N) x_N^* = ceil(A_B^(-1) b)\
"since" x_B^* = A^(-1)_B b - A^(-1)_B A_N x_N^*\
A^(-1)_B b - ceil(A^(-1)_B b) - (A^(-1)_B A_N - ceil(A^(-1)_B A_N))) x_N^* <= 0\
$)

Using the method of Gomory’s cut, we can add an additional inequality constraint for each nonintegral dimension

$ x_b^* - ceil(x_b^*) - sum_(j in N) (bar(A)_(b j) - ceil(bar(A)_(b j))) x_j^* <= 0 quad bar(A) = A_B^(-1) A_N $

This will cut off the relaxed solution, but not any integer solutions.

$ underbrace(x_b^* - ceil(x_b^*), > 0) - underbrace(sum_(j in N) (bar(A)_(b j) - ceil(bar(A)_(b j))) x_j^*), = 0) > 0 $

= Branch and Bound
Branch and bound is a general algorithm for finding optimal solutions in a large set of solution possibilities.

It is a divide-and-conquer algorithm that systematically explores the solution space by branching it into smaller subproblems and bounding the optimal solution of each subproblem to eliminate suboptimal solutions.

Consider the problem $z = min{c^top x: x in S}$

Then let $S = S_1 cup S_2 cup ... cup S_k$ be a composition of $S$ into smaller subsets, and $z^k = min{c^top x: x in S_k}$. Then $z = min_k z^k$

For instance if $S subset {0,1}^3$, the the algorithm is as follows:
#figure(
  image("assets/image-4.png"),
  caption: [Branch and bound algorithm for a binary optimization problem $S subset {0,1}^3$.]
)

== Bounding
Consider a minimization problem with the following bounds
- Let $ubar(z)^k$ be a *lower bound* on $z^k$ (dual bound)
- Let $bar(z)^k$ be a *upper bound* on $z^k$ (primal bound)

So that $ubar(z)^k <= z^k <= bar(z)^k$

Then
- $ubar(z) = min_k ubar(z)^k$ is a *lower bound* on $z$
- $bar(z) = min_k bar(z)^k$ is a *upper bound* on $z$

== Pruning
Only explore optimal paths by pruning (stop further explorations) suboptimal paths

#figure(
  image("assets/image-5.png"),
  caption: [example on how ]
)