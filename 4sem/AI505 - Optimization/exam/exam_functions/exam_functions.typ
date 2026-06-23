#import "@local/sdu-notes:0.1.0": *

#show: note.with(
  title: "Exam preparations",
  course: "AI505 - Optimization",
  author: "Simon Holm",
  date: "June, 2026",
)
#set heading(numbering: none)

#let lag = $cal(L)$

= Bracketing
For $a<b<c$

there exists a minimum in interval $[a,c]$ if
$ f(a)<f(b) and f(c)<f(b) $

== Fibonacci Search
When we know how many evaluations we have available

Places evaluations such that one evaluation always carries over to the next iteration.

== Golden Ratio Search
If one were to use fibonacci with $oo$ function evaluations. it would converge to a avg distance of $phi$

Golden Ratio just uses $phi$
== Quadratic fit search
Iteratively fits a quadratic and uses that minimum

== Lipschitz continuous
Upper bound on the derivative

For some constant $L$, $ abs(nf) >= L $

== Bisection method
Find roots since if $sign(nf(a)) ≠ sign(nf(b))$ then $[a,b]$ guaranteed to contain a root.

So cut the midpoint and choose the side where this property still holds

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

$ f(x) = f(a) + tran(nf(a)) (x-a) + 1/2 tran((x-a)) nnf(a) (x-a) $
$ f(a+h) = f(x) + nf(a)/1! h + nnf(a)/2! h^2 + O(norm(h)^3) $
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
$ v_(k+1) &= beta v_k - alpha nf(x_k) \ x_(k+1) = x_k + v_(k+1) $

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
- Cyclic Coordinate Search
  - line search in alternating coordinate directions
- Powell's method
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

= Stable Step Size
Consider $ f(x) tran(1/2 x) Q x - tran(b)x, where Q = tran(Q) succ 0 $
Then $nf(x) = Q x -b $
One can derive that the step size that stabilizes the iteration
$ 0<alpha< 2/(lambda_"max" (Q)) $

= Noisy Descent

$ x_(k+1) = x_k + alpha nf(x_k) + eps_k where eps_k tilde cal(N)(0,sigma^2_k) $

= Stochastic Gradient Descent
$ nf(x) apx 1/abs(B) sum_(i in B) nf_i (x) $

= Mesh Adaptive Direct Search
Similar to generalized pattern search but uses controlled randomness to generate directions

= Simulated Annealing

$x^prime = x + eps, where eps in cal(N)(0, T) $

== Metropolis acceptance criterion:
Let $Delta = f(x')-f(x)$
Then $ p(x, x^prime) = cases(1 & iff Delta <= 0, e^(-Delta \/ t_k) & iff Delta > 0) $

= Cross entropy/Max likelihood
Fit a distribution to elite points (lowerst function evaluation)

$ theta^* = arg min_theta -summ(i=1,N, log p(x_i| theta)) = arg min_theta prod(i=1,N,p(x_i| theta)) $
One can model data and use to sample from and/or make predictions.
#pagebreak()

= Natural Evolution Strategies
Now gradient descent:
$ theta_(k+1) = theta_k - alpha 1/N summ(i=1,N, f(x_i) nabla_theta log p(x_i| theta)) $
Where $p(dot| theta) = cal(N)(mu,sigma^2)$

Similar to CE but all points.

== CMA-ES
Same but with covariance so that it can utilize mutivariate data. 
$ x from gaus(mu,var cov) $

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
Optimize the points such that $ integral f(x) dx apx 1/M summ(i=1,M,f(x_i)) $

#pagebreak()

= Machine Learning as Optimization

Predict $ h(x;w), given  {(x_i,y_i)}^n_(i=1) $.

== Expected risk vs empirical risk
Ideally, you want to minimize the expected risk

$ R(w) = EE[f(w;xi)] = integral ell(h(x;w),y) dif P(x,y) $

Since $P(x,y)$ is unknown, we minimize *empirical risk* instead.

$ R_n (w) = 1/N summ(i=1,n,ell(h(x_i;w),y_i)) $

Usually regularize $R_n(h) + lambda Omega (h)$

== Stochastic approach
Stochastic Gradient like $ w_(k+1) = w_k - alpha_k nf_i_k (w_k) $
- very cheap, though might not always be optimal.

== Batch approach
use the full gradient of a batch instead of the whole dataset.

$ w_(k+1) = w_k - alpha_k nabla R_n (w_k) = w_k - (alpha_K)/N summ(i=1, N, nf_i (w_k)) $

== Noise Reduction
Stochastic gradient methods can have noisy gradients leading to variance (and overfitting).
- Dynamic Sampling: 
  - increase the mini-batch size used in the gradient computation
- Gradient aggregation
  - aggregate new gradients with the previous iterations 
  - SAGA - Stochastic average gradient algorithm 
    - if the average of gradient went in direction $bold(d)$ it should also infer the new gradient in direction $bold(d)$


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
$ min_x norm(A x -b)_1 $
$ min_x tran(1)s \ st A x -b &<= s \ -(A x -b) &<= s $


= Simplex problems
Each inequality constrains are half-spaces, where equality would reduce the dimensions.

multiple solutions types
== Inquality contstraints
standard way of thinking using $<=$

minimize problem such that something is less than

== Equality constraints
Easier to solve using $s.l.e.$ to solve for variables $x_i$

== Conversion
Introduce _slacks_
$ tran(a_i)x <= b iimp tran(a_i) x +s_i = b $

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

- *Pivoting*: move elements from/to $B$ and $N$
  - $mu_N$ defines a cost of "moves" when partition elements in $B$ and $N$
  - Choose the leaving candidate the yields the smallest $x^prime$ (*minimum ratio test*)
  - for multiple entering candidates with $mu_{q} < 0$ several
  - If all components of $mu_N$ are non-negative, we have found a global optimum.



= Types
- Continuos
- Stochastic
- Deterministic
