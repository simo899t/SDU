
#let title = "Lecture 4: Local Descent"
#let author = "Simon Holm"
#let date = "19/02/2026"

#import "../../../../../../temp.typ": *

#pagebreak()

// content starts here

= Descent Direction iteration
Descent Direction Methods use a local model to incrementally improve design point until some
convergence criteria is met. You can see this as a more general way to view something like gradient descent, where we iterate $x$ iterations.

In these cases you would 
1. termination conditions at xk ; if not met, continue.
2. Decide a *descent direction* $bold(d)_k$ using local information, commonly required that $bold(d)_k nf(x_k)<0$
3. Decide step size (ie, magnitude of the overall step that depends on $alpha_k$ , sometimes but not always $norm(d_k)_2 = 1$
4. Then coimpute the next design point $x_(k+1)$
$ x_(k+1) <- x_k + alpha_k d_k $

= Descent Direction

From newtons method, we know that we can approximate the solution by $ f(x+bold(p)) approx f(x) + nf(x)^T bold(p) + 1/2 bold(p)^T nnf(c+t bold(p)) bold(p) $
Then by $ m_k(p) = f(x) + nf(x)^T bold(p) + 1/2 bold(p)^T nnf(c+t bold(p)) bold(p) $

$ d_k = p = arg min_p m_k (p) ==> nabla m_k (p) =nf(x_k) + H_k p $
Then 
$ 0=nf(x_k) + H_k p ==> p = -H^(-1)_k nf(x_k) $

We can then by $ d_k nf(x_k) = -nf(x_k)^T B_k^(-1) nf(x_k)<0 $
We can say that $d_k$ is a descent direction

= Line Search for stepsize
Assuming that we already have the search direction, we can then use it to comute $alpha$ the step factor

From previous classes we now different techniques to $min_(alpha>=0) phi(alpha)$

By assuming that $p_k$ is a descent direction, that is, $phi'(0) < 0$, we can confine our search values to positive values of $alpha$

#code(
  ```py
    def line_search(f, x, d)
      objective = lambda alpha: f(x + alpha * d)
      a, b = bracket_minimum(objective)
      alpha = minimize(objective, a, b)
      return x + alpha * d
  ```
)

Often computationally costly, so approximate line search is used instead


= Line search alternatives
As mentioned earlier $alpha$ is sometimes equal to the stepsize when $norm(d_k)_2 = 1$

For approximating the line search we typically do either
- A fixed stepfactor/learning rate $alpha$
- Or a *decaying step factor/learning rate*

$ alpha_k = alpha_1 gamma^(k-1) quad "for" gamma in [0,1] $

== Approximate Line Search
We can bound a point of sufficient decrease, that is

$ f(x_(k+1)) <= f(x_k) + beta alpha nabla_(d_k) f(x_k) $
Where $beta in [0,1], "usually" beta = 1 times 10^(-4)$

This is simply just setting a bound for the a function. This way we can ignore great parts of parts which we know do not hold the minimizer. This is Armijo condition 

$ #image("/assets/IMG_7642.jpeg", width: 30em) $

#code(
  ```py
def backtracking_line_search(f, grad, x, d, alpha_0=1, p=0.5, beta=1e-4):
  y, g, alpha = f(x), grad(x), alpha_0
  while ( f(x + alpha * d) > y + beta * alpha * np.dot(g, d) ) :
    alpha *= p
  return alpha
  ```
)

#image("/assets/image-6.png")

== Wolfe conditions

=== First Wolfe Condition: Sufficient Decrease (Armijo condition)

$ f(x_(k+1)) <= f(x_k) + beta alpha nabla_(d_k) f(x_k) $
Where $beta in [0,1], "usually" beta = 1 times 10^(-4)$

Ensures that the function decreases enough relative to the directional derivative.

Note that it punishes taking many small steps to crawl toward a minimum. This way we prefer solutions that doesnt take forever to find.

=== Second Wolfe Condition: Curvature Condition

$ nabla_(d_k) f(x_(k+1)) >= sigma nabla_(d_k) f(x_k) $

This just means that we make sure that $nf(x_(k+1))$ is sufficiently more flat than $nf(x_k)$ (e.g we are approaching a stationary point), if this doesnt hold we should probably take bigger steps, inforder to get closer to the minimizer (stationary point). For this a higher $sigma$ allows less flattening before accepting a step, and vice versa.

for these $beta < sigma < 1$

== Strong wolfe conditions

A step length may satisfy the Wolfe conditions without being particularly close to a minimizer of $f(x_k + alpha d_k)$

We can modify the curvature condition to force-excluding proints that are far from stationary points

It does this by shrinking the interval with exclution each time the step fails Armijo or the curvature condition.

We end up with something like this
#image("/assets/image-7.png")

= Approximate Line Search Goal

We wish to find some non-fixed $alpha_k$

== Strong Backtracking

Strong Bracketing is the idea that we use both bracketing as well as zooming (with strong wolfe conditions)

$ "Bracket minimum" -> "Zoom to approach solution" $

So it has two phases

1. Bracketing phase
  - Start with $[alpha_0=0, alpha_1=1]$
  - Increase step (with some bracketing algorithm)
  - Find an interval with a guaranteed solution
2. Zoom phase
  - Shrink interval
  - Use Wolfe conditions to optimize
#pagebreak()



== Bracket Phase 
The bracket phase is the same as before, but this time we also check for the Strong Wolfe conditions and terminate if they're satisfied. 

The Strong Wolfe check in condition 2 is just an opportunistic shortcut — "while we're here, check if we got lucky and already landed on a good point." If yes, skip the zoom, we're done.

#pseudo[
  Input: $alpha_"max"$
  Output: $alpha^*$
  + $k <- 0$;
  + $alpha_0 <- 0$
  + Choose $alpha_"max" > 0$
  + $alpha_1 in (0,alpha_"max")$
  + *while* True *do*
    + Evaluate $phi(alpha_i)$;
    + *if* $phi(alpha_i) > alpha(0)+beta alpha_i phi'(0)$ or $[phi(alpha_i) >= phi(alpha_(i-1)) " and "i>1]$ *then*
      + $alpha^* <- "zoom"(alpha_(i-1), alpha_i)$ and break;
    + Evaluate $phi'(alpha_i)$;
    + *if* $abs(phi'(alpha_i))<=-sigma phi'(0)$ *then*
      + $alpha^* <- alpha_i$ and break;
    + *if* $phi'(alpha_i) >= 0$ *then*
      + $alpha^* <- "zoom"(alpha_i, alpha_(i-1))$ and break;
    + Choose $alpha_i + 1 in (alpha_i, alpha_(i-1))$
    + $i <- i +1$
]

== Zoom phase

Now we can decrease the size of the interval until an acceptable step length $alpha$ is identified.
Here we can choose algorithms such as Bisection, Cubic/Quadratic interpolation or golden section search (like bisection but splits at $phi$ instead of the midpoint). Can be optimal for bisection-style search with no gradient info.

Note that in practice *cubic interpolation dominates* many usecases

#pseudo[
  Input: $alpha_"low", alpha_"high"$
  Output: $alpha^*$
  + *while* True *do*
  + Interpolate (using quadratic, cubic or bisection) to find a trial step length $alpha_j$ between $alpha_"low"$ and $alpha_"high"$;
  + Evaulate $phi(alpha_j)$;
  + *if* $phi(alpha_j) > phi(alpha_o) + beta alpha_j phi'(alpha_o)$ or $phi(alpha_j) >= phi(alpha_"low")$
  + $alpha_"high" <- alpha_j$;
  + *else*
    + Evaluate $phi'(alpha_j)$
    + *if* $abs(phi'(alpha_j)) <= -sigma phi'(+)$ *then*
      + $alpha^* <- alpha_j$ and break;
    + *if* $phi'(alpha_j)(alpha_"high"-alpha_"low") >= 0$ *then*
      + $alpha_"high" <- alpha_"low"$;
    + $alpha_"low" <- alpha_j$
]



= Convergence analysis
A sequence ${x_k}$ converges to $x$ if for any $eps > 0$, there's an index $K$ such that:
$ |x_k - x| <= eps quad forall k >= K $

In this case we analyse $ cos(theta_k) = (-nabla f_k^T d_k)/(norm(nabla f_k)norm(d_k)) $

This means that we are simply trying to figure out *if* a sequence of guesses actually get arbitrarily close to the target $x^*$?

== Convergence of Line Search
=== Zoutendijk's condition

Notice that if a series of non-negative numbers $a$ where to satisfy
$ summ(k=0,oo,a_k) < oo $
Then $a_k -> 0$

With this we can say that given an algorithm with some directions and some steepest descents, then $ summ(k>=0,, cos^2 theta_k norm(nabla f_k)^2) < oo $

_*note* that this is a consequence of Wolfe and smoothness assumptions_

Notice that if the search directions do not become orthogonal to the gradient (e.g. the algorithm is not bad) then it implies that

$ cos^2(theta_k)norm(nabla f_k) -> 0 $

Because of this since $cos^2(theta_k)norm(nabla f_k)$ cannot be 0 unless $nabla f_k = 0$ (e.g. stationary point), we can be sure that the gradient norms $norm(nabla f_k)$ converge to zero, provided that the search directions are never too close to orthogonality with the gradient (not bad).

Note that we cannot prove convergance to minimum by this only that we appreach a strationary point. We can use Hessian ($nabla^2f$) to prove that $x^*$ is a minimum, and not a maximum or saddlepoint. With this extra bit of information, we can prove that the convergence is converging to a local miniumum
#pagebreak()


=== Global Convergence
We wish to show that it always has a meaningful downhill component, that is even in the worst case, you're still making real progress toward a minimum. We can do this by bounding $cos(theta_k)$ by some constant.

Let
$ norm(B_k) dot norm(B^(-1)_k) = M forall k $
Lets substitute $d_k=-b^(-1)_k nabla f$ in the expression for cosine

$ cos(theta_k) = (-nabla f_k^T d_k)/(norm(nabla f_k)norm(d_k)) $
$ cos(theta_k) = (nabla f_k^T B^(-1)_k nf(x_k))/(norm(nabla f_k)norm(-B^(-1)_k nf(x_k))) $

Then because of $ v^T A v= lambda_"min" (A^(-1)) norm(v)^2 >= 1/norm(A) norm(v)^2 $

Due to $lambda_"min" (A^(-1))1/(lambda_"max" (A)) 1/norm(A)$

Then,

$ cos(theta_k) = (nabla f_k^T B^(-1)_k nf(x_k))/(norm(nabla f_k)norm(-B^(-1)_k nf(x_k))) $

$ cos(theta_k) = frac(norm(nf(x_k))^2\/norm(B_k),  norm(nf(x_k))^2 norm(B^(-1)_k)) $

This equals to 

$ cos(theta_k) = 1/(norm(B_k) norm(B_k^(-1))) = 1/M $

Now we know that even in the worst case and the method cannot stall without ever finding a minimum. This is called global gonvergence. Note, this makes no promise about efficiency, and in the worst case it _could_ take a very long time.

== Rate of Convergence
Let $x_k$ be a sequence of steps in $r in RR^n$ that (at some point) converges to a minimum $x^*$ (not necessarily the minimizer).

When analyzing rate of convergence we wish to learn how much closer to the solution a step takes us. This is important as steps might be computationally expensive, and we would rather take less steps, if its posible.

We would like to classify methods by their rate of convergence.

=== Q-linear
For this there should exist a constant $r in (0,1)$
$ norm(x_(k+1)-x^*)/norm(x_k -x^*) <=r, quad forall k "sufficiently large" $
If this holds the algoritm is said to be *Q-linear*

Exmaple: ${1+(0.5)^k}$ converges Q-linearly to 1, with a rate of convergance $r=0.5$

=== Q-superlinear
The convergence is said to be *Q-superlinear* if 

$ lim(k->oo) norm(x_(k+1)-x^*)/norm(x_k -x^*) =0 $

Example: ${1+k^(-k)}$ wil Superlinearly converge to 1

This is because
$ qquad square $


=== Q-quadratic




$ norm(x_(k+1)-x^*)/norm(x_k -x^*)^2 <=M, quad forall k "sufficiently large" $

Superlinear convergence (quadratic, cubic, quartic, etc) is regarded as fast and desirable, while
sublinear convergence is usually impractical.

=== R-linear