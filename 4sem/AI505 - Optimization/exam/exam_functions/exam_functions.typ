
#import "../../../../temp/temp.typ": *

#show: note.with(
  title: "Exam preparations",
  course: "AI505 - Optimization",
  author: "Simon Holm",
  date: "June, 2026",
)
#set heading(numbering: none)

#let lag = $cal(L)$

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
  
= Positive Definteness

#definition(title: [Positive definteness], [
  A square, symmetric matrix $A$ is *positive definite* if $tran(x)A x$ is positive for all points other than the origin
  $ A succ 0 = tran(x)A x > 0 quad forall x in RR^(without {0}) $
])

#definition(title: [Positive semidefinteness], [
  A square, symmetric matrix $A$ is *positive semidefinite* if $tran(x)A x$ is always nonnegative
  $ A succ.eq 0 = tran(x)A x >= 0 quad forall x in RR $
])

= Descent Direction Iteration
$ x_(k+1) = x_k alpha_k d_k where d = -B_k^(-1) nf(x_K) $
Note that this is because in Newton $B_k = nnf(x_k)$

And for 1st order methods like GD $B_k = I$

= Wolfe conditions
1. First Wolfe Condition: Sufficient Decrease (Armijo condition)
$ f(x_(k+1)) <= f(x_k) + beta alpha nabla alpha nabla_d_k f(x_k) $
2. Second Wolfe Condition: Curvature Condition
$ nabla_(d_k) f(x_(k+1)) >= sigma nabla_(d_k) f(x_(k)) $

== Strong wolfe
Modify Curvature Condition such that
$ abs(nabla_(d_k) f(x_(k+1))) >= sigma abs(nabla_(d_k) f(x_(k))) $

#pagebreak()
= Rate of convergence
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
== R-linear (root-linear)
For a sequence of nonnegative scalars ${v_k}$ where $v_k -> 0$
$ norm(x_(k+1) - x^*) <= v_k quad forall k >> 1 $

= Trust Region Methods

$ min_(x^prime) pred(f)(x^prime) \ st norm(x-x^prime) <= delta  $

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

= Stable Step Size
Consider $f(x) tran(1/2 x) Q x - tran(b)x, where Q = tran(T) succ 0 $
Then $nf(x) = Q x -b $
One can derive that the step size that stabilizes the iteration
$ 0<alpha< 2/(lambda_"max" (Q)) $

= Noisy Descent

$ x_(k+1) = x_k + alpha nf(x_k) + eps_k where eps_k tilde cal(N)(0,sigma^2_k) $

= Stochastic Gradient Descent
$ nf(x) apx 1/abs(B) sum_(i in B) nf_i (x) $

#pagebreak()
= Simulated Annealing

$x^prime = x + eps, where eps in cal(N)(0, T) $

== Metropolis acceptance criterion:
Let $Delta = f(x')-f(x)$
Then $ p(x, x^prime) = cases(1 & iff Delta <= 0, e^(-Delta \/ t_k) & iff Delta > 0) $

= Lagrangian Function
$ lag(x,mu,lam) = sum_i mu_i g_i (x) + sum_j mu_j h_j (x) $

== Primal form
$ min_x max_(mu>=0, lam) lag(x,mu,lam) $

== Dual form
$ max_(mu>=0, lam) min_x lag(x,mu,lam) $