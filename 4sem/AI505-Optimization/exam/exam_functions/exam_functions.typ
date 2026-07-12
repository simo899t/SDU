#import "@local/sdu-notes:0.1.0": *

#show: note.with(
  title: "Exam preparations",
  course: "AI505 - Optimization",
  author: "Simon Holm",
  date: "June, 2026",
)
#set heading(numbering: none)

#let lag = $cal(L)$
= Conditions for local minima
$ nf(x^*) = 0 $
$ nnf(x^*) >= 0 $

= Bracketing
For $a<b<c$

there exists a minimum in interval $[a,c]$ if
$ f(a) > f(b) and f(c) < f(b) $

== Fibonacci Search
When we know how many evaluations we have available

$ (F_(n-k+1))/(F_(n-k+2)) $

== Golden Ratio Search
If one were to use fibonacci with $oo$ function evaluations. it would converge to a avg distance of $phi$

Golden Ratio just uses $1/phi apx 0.6$
== Quadratic fit search
Iteratively fits a quadratic and uses that minimum



== Bisection method
Root:
$ sign(f(a)) != sign(f(b)) $

Minimum 
$ sign(nf(a)) != sign(nf(b)) $

So cut the midpoint and choose the side where this property still holds
#pagebreak()

= A simple minimization problem

Defining a minimization problem such that $x$ lies within a feasible set of solutions
$ min_x f(x) \ st x in cal(X) $

#example([
  $ min_(x_1,x_2) f(x_1,x_2) \ 
  st #align($x_1 &>= 0 \
             x_2 &>= 0 \
             x_1 + x_2 &<= 1 \ $) $
  #figure(
    image("assets/image-1.png")
  )
  ])

#pagebreak()
= Taylor expansion
Taylor expansion

Let $a$ be a fixed point. The Taylor expansion approximates $f(x)$ near $a$ using only information about $f$ at $a$.

$ f(x) apx f(a) + tran(nf(a)) (x-a) + 1/2 tran((x-a)) nnf(a) (x-a) +dots $
$ f(x) =f(a+h) apx f(a) + nf(a)/1! h + nnf(a)/2! h^2+dots $

== Lipschitz continuous
Upper bound on the derivative

For some constant $L$, $ norm(f(x)-f(y)) >= L norm(x-y) $

= Convexity

$ f: RR^n -> RR \ f(alpha x + (1-alpha) y) <= alpha f(x) + (1-alpha) f(y)  $

#figure(
  image("assets/image-2.png")
)

For a convex hull:
#figure(
  image("assets/image-3.png")
)
$ "conv"(X) = {sum_i lambda_i x_i | x_i in X, quad  lambda_i >= 0, quad  sum_i lambda_i = 1} $

= Smoothness
There exists $C^oo$ such that
$ nabla^n f where n to oo $

= L-smoothness
$ norm(nf(x)-nf(y)) >= L norm(x-y) $

= Numerical diff

$ f'(x) approx (f(x+h)-f(x))/h $

= Directional derivative

$ nabla_bold(s) f(x) = tran(nf(x))bold(s) = ppv(f(x),x_1) bold(s_1) + ppv(f(x),x_2) bold(s_2) + dots +  ppv(f(x),x_n) bold(s_n) $
  
= Positive Definiteness

#definition(title: [Positive definiteness], [
  A square, symmetric matrix $A$ is *positive definite* if $tran(x)A x$ is positive for all points other than the origin
  $ A succ 0 = tran(x)A x > 0 quad forall x in RR^(without {0}) $
])

#definition(title: [Positive semidefiniteness], [
  A square, symmetric matrix $A$ is *positive semidefinite* if $tran(x)A x$ is always nonnegative
  $ A succ.eq 0 = tran(x)A x >= 0 quad forall x in RR $
])

= Descent Direction Iteration
$ x_(k+1) = x_k alpha_k d_k where d = -B_k^(-1) nf(x_K) $
Note that this is because in Newton $B_k = nnf(x_k)$

And for 1st order methods like GD $B_k = I$

= Line Search
$ alpha^* = min_(alpha>=0) f(x_k + alpha d_k) $

= Backtracking
$alpha_0 = 1$

Then decrease alpha such that $ f_(k+1) <= f_k + beta alpha nabla_d_k f_k where beta in [0,1] $
#pagebreak()

= Wolfe conditions
1. First Wolfe Condition: Sufficient Decrease (Armijo condition)
$ f(x_(k+1)) <= f(x_k) + beta alpha nabla alpha nabla_d_k f(x_k) $
1. Second Wolfe Condition: Curvature Condition
$ nabla_(d_k) f(x_(k+1)) >= sigma nabla_(d_k) f(x_(k)) $

== Strong wolfe
Modify Curvature Condition such that
$ abs(nabla_(d_k) f_(k+1)) >= sigma abs(nabla_(d_k) f_(k)) $

== Strong Backtracking
- Bracketing-phase: golden ratio
- Zoom phase: bisection or some interpolation (like quadratic fit)

= Conjugate Gradient
Using line search results in $ tran(nf_(k+1))nf_(k) = 0 quad (nf(x_(k+1)) bot nf(x_(k))) $

$ tran(d)_i H d_j = 0, quad forall i != j "Vectors are not orthogonal." $

Where $H succ.eq 0$

Generally $d_0 = -nf(x_0)$ and, $ d_(k+1) = -nf_(k+1) + beta_k nf_k $
Where
$ beta_k^"Fletcher-Reaves" &= (tran(nf_(k+1))nf_(k+1))/(tran(nf_(k))nf_(k)) \
 beta_k^"Polak-Ribière"  &= (tran(nf(k+1))(nf_(k+1)-nf_k))/(tran(nf_(k))nf_(k)) $

#pagebreak()
= Rate of convergence
We need to make sure of method convergence.

by #theorem(title:[Theorem: Zoutendijk condition], [Consider any iteration of the form $x_(k+1) <- x_k + alpha_k d_k$, where dk is a descent direction and αk satisfies the Wolfe conditions. Suppose that $f$ is bounded below in $RR^n$ and that $f$ is continuously differentiable in an open set N containing the level set $L = {x : f (x) ≤ f (x_0)}$, where $x_0$ is the starting point of the iteration. Assume also that the gradient $nf$ is Lipschitz continuous on $N$, that is, there exists a constant $L > 0$ such that:
$ |f (x) − f (y)| ≤ L|x − y|, ∀x, y ∈ N $
Then:
$ sum_(k>=0) cos^2 theta_k norm(nf_k)^2 < oo $
])

This means that $cos^2 theta_k norm(nf_k)^2 -> 0$

Since $cos theta_k$ can be bounded by a constant $1/M$ we know that $ norm(nf_k)^2 -> 0 $



For a sequence of $x$ in $RR^n$

$ "Q-linear" approx "R-linear" < "Q-superlinear" < "Q-quadratic" $

== Q-linear (quotient-linear)
There exits $r in (0,1)$ such that
$ norm(x_(k+1) - x^*)/norm(x_k - x^*) <=r quad forall k >> 1 $

== Q-superlinear (quotient-superlinear)
$ limm(k->oo) norm(x_(k+1) - x^*)/norm(x_k - x^*) = 0 $


== Q-quadratic (quotient-superlinear)
For some constant $M > 0$
$ norm(x_(k+1) - x^*)/norm(x_k - x^*)^2 <=M quad forall k >> 1 $

== R-linear (root-linear)
For a sequence of nonnegative scalars ${v_k}$ where $v_k -> 0$
$ norm(x_(k+1) - x^*) <= v_k quad forall k >> 1 $

= Trust Region Methods

$ min_(x^prime) pred(f)(x^prime) \ st norm(x-x^prime) <= delta  $

$delta$ can be expanded or contracted by $eta$

$ eta = "real improvement"/"predicted improvement" = (f(x) - f(x^prime))/(f(x) - hat(f)(x^prime)) $

= Termination Conditions

- Maximum Iterations: $k>k_"max"$
- Absolute Improvement: $f(x_k) - f(x_(k+1)) < eps_a$
- Relative Improvement: $f(x_k) - f(x_(k+1)) < eps_r abs(f(x_k)) $
- Gradient Magnitude: $norm(nf(x_(k+1))) < eps_g$

= Steepest Descent

$ d_k = - nf(x_k)/norm(nf(x_k)) $

= Conjugate gradient method
In
generating its set of conjugate vectors, it can compute a new vector $d_k$ by using only the previous vector $d_(k-1)$. 
$ d_k = -nf(x_k) + beta_k d_(k-1) $
Where $ beta_k = (tran(nf(x_k)) A d_(k-1))/(tran(d_(k-1)) A d_(k-1)) $

= Momentum 
$ v_(k+1) & = beta v_k - alpha nf(x_k) \ x_(k+1) & = x_k + v_(k+1) $

= Nesterov momentum
(look ahead gradient)
$ v_(k+1) & = beta v_k - alpha nf(x_k + beta v_k) $

= Adagrad
Adapts the learning rate to the size of previous gradients

== RMSprop
Extends Adagrad such that the squared gradients are averaged and decayed

== AdaDelta
instead of learning rate modify RMSprop

= Adam
Momentum + RMSprop

== Nadam
Adam + nesterov momentum

== Hypergradient descent
Applying gradient descent to a hyperparameter


= Newton method
Multivariate update rule
$ x_(k+1) = x_k - H_k^(-1) nf(x_k) $

== Convergence
When $nnf(x^*) succ 0 "and" nf(x^*) = 0$ (locally bowl shaped) $ iimp min_x = x^*$

When $f$ is strong convex, then $nnf(x) succ 0, quad forall x$ 

= Secant method
Approximate $f''(x_k) apx (f'(x_k) - f'(x_(k-1)))/(x_k - x_(k-1)) $
So $ x_(k+1) = x_k - (f'(x_k) - f'(x_(k-1)))/(x_k - x_(k-1)) f'(x_k) $

= Quasi-Newton methods
Approximate $Q_k apx H^(-1)$ iteratively

= Derivative free methods
Black box infinite computational resources + unknown constraints


The objective function is unknown and can only be accessed through evaluations (oracles, zero'th or first order )
- Cyclic Coordinate Search
  - line search in alternating coordinate directions (can be augmented like conjugate descent)
- Powell's method
  - Non orthogonal directions
  - Drops already seen directions
- Hooke-Jeeves
  - evaluate $f(x)$ and $f(x pm alpha e_i)$ and select best one
  - If no improvements are found shrink $alpha$
- Generalized pattern search
  - Hooke-Jeeves but with $n+1$ directions
- Nelder-Mead
  - Reflection, expansion, contraction, shrinkage.
- DIRECT - Divided rectangles
  - Balances interval size and function evaluation
#pagebreak()
= Noisy Descent

$ x_(k+1) = x_k + alpha nf(x_k) + eps_k where eps_k tilde cal(N)(0,sigma^2_k I) $
$ sigma = 1/k $
= Stochastic Gradient Descent
$ nf(x) apx 1/abs(B) sum_(i in B) nf_i (x) $

= Mesh Adaptive Direct Search
Similar to generalized pattern search but uses controlled randomness to generate directions

Should preserve linearindepence 

= Simulated Annealing

$x^prime = x + eps, where eps in cal(N)(0, T) $

== Metropolis acceptance criterion:
Let $Delta = f(x')-f(x)$
Then $ p(x, x^prime) = cases(1 & iff Delta <= 0, e^(-Delta \/ t_k) & iff Delta > 0) $

$ T_(k+1) =  $

= Cross entropy/Max likelihood
Fit a distribution to elite points (lowest function evaluation)

$ theta^* = arg min_theta -summ(i=1,N, log p(x_i| theta)) = arg min_theta prod(i=1,N,p(x_i| theta)) $
One can model data and use to sample from and/or make predictions.
#pagebreak()

= Natural Evolution Strategies
Now gradient descent:
$ theta_(k+1) = theta_k - alpha 1/N summ(i=1,N, f(x_i) nabla_theta log p(x_i| theta)) $
Where $p(dot| theta) = cal(N)(mu,sigma^2)$

Similar to CE but all points.

== CMA-ES
Same but with covariance so that it can utilize multivariate data. 
$ x from gauss(mu,var cov) $

mean is weighed average of $m$-elites


CMA-ES uses cumulative vectors $p_1$ (length) and $p_2$ (shape), which a decaying memory vector $in RR^n$ (kinda like rnn's)

= COCO benchmarking
Benchmark you method against 24 functions
- $n$ dimensions
- $i$ function (1-24)
- $j$ instance (rotate/shift)
#pagebreak()

= Population-based methods
Optimize a collection of *individuals*

We only keep track of chromosomes (each individual is interpreted as a set of chromosome, commonly vectors $in RR^n$)

- *Selection*
  - Determine which individuals pass their genetic information on to the next generation
  - *Truncation* selection: truncate the lowest performers
  - *Tournament* selection: selects fittest out of k randomly chosen individuals
  - *Roulette Wheel* selection: individuals are chosen with probability proportional to their fitness
- *Crossover*
  - Combine chromosomes from two parents to create child
  - *Single-point*: swap occur after a single point
  - *Multiple-point*: swap $z$ times 
  - *Uniform* crossover: Each chromosome has $50%$ chance of crossover
- *Mutation*: randomly mutate some individuals to support explorations. 
  - Mutate by _mutation rate_
- *Replace*: Choose the new populations using all above
  - *Goal:* find the best population, but avoid duplicates.

== Differential Evolution
Choose 3 random distinct individuals $a,b$ and $c$

Then
$ z = a + w(b-c) $
$ x^prime_i cases(z_i iff i = j where j from cal(U){1, ..., n}
                  ,x_i otherwise) $

== Swarm
Now let each individual track, 
- position, 
- velocity
- Own best position
- Global best position

As particles fly toward the current global best, they pass through different regions of the search space. If one of them stumbles across a better point along the way — that becomes the new global best, and the whole swarm redirects toward it.


= Sampling Plans
Choosing how to initialize points before optimization begins. There are several methods.
- try optimizing multiple starting points and keep best result
- if $f$ is expensive, one can sample points from a _surrogate model_
*Surrogate model*: cheap fake version of $f$


== Full Factorial Design
Sample points uniformly and evenly spaced samples across domain
Simple, easy to design, but grows with $n^m$ and can miss local features

== Random Sampling
Sample randomly from some distribution over $[a_1,b_1] times dots times [a_n,b_n]$

== Uniform Projection Plans
Sample $m$ points from $m times m$ with the premise that every dimension must be uniformly covered (like N-queens but with rooks). 

== Stratified Sampling
Fix problem with local accuracy that full factorial has. Place points uniformly random within their square


== Space Filling Metrics
Fill the space so everything is covered. (measure how spread points are)
- Like pairwise distance

== Space-Filling Subsets
Choose a subset of the space what best represents the whole space.

== Quasi-Random Sequences 
Optimize the points such that $ abs(1/M summ(i=1,M,f(x_i)) -integral f(x) dx) -> 0 $

#pagebreak()

= Machine Learning as Optimization

== Expected risk vs empirical risk
Ideally, 
$ R(w) = EE[ell(h(x;w),y)] = integral ell (h(x;w),y) dvar(P) $

Instead we minimize *empirical risk* instead.

$ pred(h) = arg min_w 1/N summ(i=1,n,ell(h(x_i;w),y_i)) $

== Stochastic approach
Determine $nf_i$ from a single $x_i$ $ w_(k+1) = w_k - alpha_k nf_i_k (w_k) $
- very cheap, though might not always be optimal.

== Batch approach
Use the full gradient 

$ w_(k+1) = w_k - alpha_k 1/N summ(i=1, N, nf_i (w_k)) $

== Mini batch
$ w_(k+1) = w_k - alpha_k 1/abs(B) summ(i=1, abs(B), nf_i_k (w_k)) $


== Noise Reduction
- Dynamic Sampling: 
  - Increase $abs(B)$
- Gradient aggregation
  - $nf_i$ should infer on $nf_iplus$
- Iterative average methods
  - $ nf(obar(x)) where obar(x) = 1/N summ(i=1,N,x_i) $

#pagebreak()

== Second order methods
- Diagonal scaling
  - Scale only using a diagonal hessian $ H_k apx diag(H_k) $
  - It still holds curvature information, just not as much.
- quasi-Newton
  - construct approximations to the Hessian using only gradient information (L-BFGS)
- Gradient free
  - Use system of linear equations to find $s_k$ from $H_k s_k = - nf$

= SG convergence analysis
- Strongly convex + fixed $alpha$ = never converge. You need to shrink $alpha$ (slower).
- Non convex cannot be bound since there might be many local minima. you might approach a stationary point though. When shrinking $alpha$ yoy get closer to a minima, but slower.

= Constrained Optimization
1. Equality constraints: $h(x) = 0$
2. Inequality constraints: $g(x) <= 0$

$ #image("assets/image-5.png", width: 20em) $

#pagebreak()

== Transformations to Remove Constraints
$ min_(x in [a,b]) f(x) $

Instead define $cal(T)_(a,b) : RR to [a,b]$
$ min_(hat(x) in RR) f(cal(T)(hat(x))) $



== Lagrangian Function
$ lag(x,mu,lam) = f(x) +sum_i mu_i g_i (x) + sum_j lam_j h_j (x) $

== KKTC (Karush-Kuhn-Tucker conditions)
For any $x^*$
+ Primal feasibility: $g(x^*) <=0, h(x^*) = 0$
+ Dual feasibility: $mu >= 0$
+ Complementary slackness: $mu dot g(x^*) = 0$
+ Stationarity: $nf(x^*) + mu dot nabla g(x^*) + lam dot nabla h(x^*)$

=== Primal form
$ min_x max_(mu>=0, lam) lag(x,mu,lam) $

=== Dual form
$ max_(mu>=0, lam) min_x lag(x,mu,lam) $

= Penalty methods

Examples:
- Count penalty
  $ p_"count" (x) = sum_i (g_i (x) > 0) + sum_j (h_j (x) != 0) $
- Quadratic penalty
  $ p_"quadratic" (x) = sum_i max(g_i (x), 0)^2 + sum_j h_j (x)^2 $
- Mixed Penalty
  $ p_"mixed" (x) = p_"count" + p_"quadratic" $

== Barrier functions
$ p_"barrier" (x) = - sum_i log(-g_i (x)) $

= LP
when both the objective and all constraints are linear.
== Model form nonlinear to LP
$ min_x abs(A x -b) $
$ min_x tran(1)s \ st A x -b &<= s \ -(A x -b) &<= s $

== Simplex problems
Each inequality constrains are half-spaces, where equality would reduce the dimensions.

multiple solutions types
=== Inequality constraints
standard way of thinking using $<=$

minimize problem such that something is less than

=== Equality constraints
Easier to solve using $s.l.e.$ to solve for variables $x_i$

== Conversion
Introduce _slacks_
$ tran(a_i)x <= b iimp tran(a_i) x +s_i = b $

#pagebreak()

== The simplex algorithm

$ lag(x,mu >= 0, lam) = tran(c)x - tran(mu)x - lam (A x -b) $

$ nabla lag = c-mu-tran(lam)A = 0 iimp tran(A)lam + mu = c $

Split into ($B,N$)
$ cases(tran(A_B) lam+ mu_B = c_B, tran(A_N) lam+ mu_N = c_N) qquad  iimp lam = inv((tran(A_B))) c_B since x>=0, mu_B = 0 $

Now $ tran(A)_N inv((tran(A_B))) c_B + mu_N = c_N bii mu_N = c_N - tran(A)_N inv((tran(A_B))) c_B $
One must maintain $ A_B x_B^prime + A_{q} x_{q}^prime = A_B x_B = A x = B $
This is done by updating $x_B to x^prime_B$ by
$ x^prime_B = x_B - A^(-1)_B A_{q} x^prime_{q} $
Then the objective value function is updated
$tran(c)x^prime = tran(c)x + mu_{q} x_{q}^prime $
We can initialize the problem by populating $x$ with values that fit into $A x=b$

= PDHG
LPs with strong duality are saddle-points!

Do gradient descent ascend + momentum



#pagebreak()

- *Pivoting*: move elements from/to $B$ and $N$
  - $mu_N$ defines a cost of "moves" when partition elements in $B$ and $N$
  - Choose the leaving candidate the yields the smallest $x^prime$ (*minimum ratio test*)
  - for multiple entering candidates with $mu_{q} < 0$ several
  - If all components of $mu_N$ are non-negative, we have found a global optimum.


== Dual certification
Verify that a solution is optimal given that for primal solution $p^*$ and dual solution $d^*$

Weak duality guarantees that the duality gap:
$ d^* - p^* >= 0 $

If $d^*=p^*$ then $p^*$ must be the unique optimal value. That is for $f(x) = p^* iimp x^*$ 

Duals might be infeasible. *Example*: if $p^* = -oo$ then $d^* lt.eq.not -oo $.


= Discrete Optimization
Space is discrete. Variables can only take specific, distinct values. These problems tend to be computationally expensive, so one can utilize heuristics.

== Exact methods
- ILP
- Combinatorial optimization algorithms
- SAT
- Dynamic
- Constraint

== Heuristic Methods
- Greedy
- Local search
- Metaheuristics
  -  Genetic
  -  SA
#pagebreak()

= ILP
Variables can be $ZZ, NN_0 "or" BB$
- Mixed ILP (not all variables *has* to be integers)

== LP rounding
Solve LP-problem and round the solution to to integer. Typically round to suboptimal point so we don't round outside the feasible space. This can lead to problems where you essentially need to start searching again ig the optimal integer is far away from the $x^*$

== Cutting Plane method
Assume that LP $f(x) = x^*$ where $x^*$ is fractional. when solving $x^*$ is not a valid integer solution. Find cutting plane (linear constraints) such that the problem is constrained to exclude the infeasible non-integer space beyond the optimal integer $x^*$

$ max_x tran(c)x \ st A x &<= b \ x &in ZZ^n  $


=== CG method
Take the partition that 
$ A_B x_B^* + A_N x_N^* = bold(b) iimp x_B^* = inv(A_B) bold(b) - inv(A_B) A_N x_N^* $

Now floor some of the constraints such that

$ A_B bold(b) - floor(A_B bold(b)) - underbrace((inv(A_B) A_N - floor(inv(A_B) A_N))x_N^*, =0) <=0  $

$ A_B bold(b) - floor(A_B bold(b)) <= 0 quad ("integer") $

= Branch and Bound
#let lb = $ubar(z)^k$
#let ub = $obar(z)^k$

$ min tran(c)x where x in S $
$ S = S_1 cup S_2 cup dots cup S_k $
Define upper bound and lowerbound

$ lb <= z^k <= ub $

At each node one can prune any branch where $lb>=ub$

So the algorithm naturally stops when all expanded nodes have been pruned or *solved*
#pagebreak()

= Dynamic programming
optimal substructures  + overlapping subproblems

recursion can easily benefit from this, since it includes overlapping quite a bit
  
#example(title: [Example: Fibonacci], [
  Let $ F_n = F_(n-1) + F_(n-2) where F_1,F_2 = 1 $

  Then we can optimize storying known F's

  $ #image("assets/0226C4DA-99AC-4302-A86E-AD7C215B576E_1_102_a.jpeg") $
])

For TSP instead of brute force one can model so that $ z^*=g(i,S) = min_(k in S) {c_(i,k) + g(k,S - {k})} $

#pagebreak()

= Constraint programming
Assign values to variables such that some constraints are satisfied.

*Goal*: feasibility

- variables $bold(x)$
- Domain $x in cal(D)$
- Constraints: some rules (eg. adjacency)

The *core* of CP #emoji.face.teeth:
- Modelling
- Inference: after assigning a variable, propagate the consequence to reduce domains
- Heuristics: Guide when multiple options are valid. (like assign most constrained first)
- Symmetry: some assignments might lead to a symmetric result. compensate for this.
- Backtracking

= Discrete Optimization: Random optimization heuristics ROH
We can utilize heuristics typically work well in practice, but often lack theoretical guarantees. 

We can utilize the following

  - White box: LP,ILP,CP
  - Gray box: neural networks
  - Black box: function evaluation based like `ROAR-NET-API`

- Construction: construct on a partial solution such that it becomes complete
  - Greedy
  - Beam
  - Rollout
- Local search: optimize the complete solution
  - First improvement
  - Best improvement
  - $Astar$
- Metaheuristics
  - SA

= Projects

== Assignment 1
=== Case 1

some minus errors in formulas

Space-Filling Metrics

Penalty function
Balance optimality and convergence
Newton method Hessian, penalize between points

force PD $ nnf(x) = Q Lambda tran(Q) $
Steps: nelder mead (within reasonable no. steps)

Time: they all converge given the time (different no. steps)

Maybe we should have used Nadam also

=== Case 2

More control

Gradient descent was fine. maybe the data was not super hard to learn

Second order method

== Assignment 2
=== Case 1

- Computing the gradient
- Convex proof
- Proving semi definiteness
- 1 newton step
- Modify constraint $to$ linear dependency
- Normalize the last constraint
- Bound $abs(x)<=1$

=== Case 2 
No heuristics

lb terminology. We have no lb, only objective value and the ub 