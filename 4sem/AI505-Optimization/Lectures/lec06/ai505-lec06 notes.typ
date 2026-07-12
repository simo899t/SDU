
#let title = "Lecture 6: Second-Order Methods"
#let author = "Simon Holm"
#let date = "March - 2026"

#import "@local/tempst:0.1.0": *

#note(
  title: title,
  author: author,
  date: date
)

// Your content starts here

= Newton Method
1. Approximate a function using second-order Taylor series expansion
2. analytically obtain the location where a quadratic approximation has a zero gradient.
3. use that location as the next iteration to approach a local minimum.

#figure(
  image("assets/image-38.png"),
  caption: [Common causes of error in Newton's method (aproximation is $bluemath("blue")$)]
)




== Univariate function

$ q(x) = f(x_k) + (x-x_k)f'(x_k) + ((x-x_k)^2)/2 f''(x_k) $

$ ddx q(x) = f'(x_k) + (x-x_k)f''(x_k) = 0 $
$ x_(k+1) = x_k - (f'(x_k))/f''(x_k) $


#figure(
  image("assets/image-37.png"),
  caption: [finding roots of derivative function]
)


#pagebreak()

== Multivariate function
$ f(x) approx f(x_k) + nf(x_k)^T (x-x_k) + 1/2(x-x_k)^T H_k(x-x_k). $
Where $H$ is the hessian matrix

now evaluate $nabla q(x)$

$ nabla q(x) = nf(x_k) + H(x_k)(x-x_k) = 0. $

This leads to the multivariate update rule 
$ x_(k+1) = x_k - H_k^(-1) nf(x_k) $

_Note that: If f is quadratic and its Hessian is positive definite, then the update converges to the global
minimum in one step._

== Strong convexity

For Strong convexity the second order derivative is positive,

#definition(
  [The function $F: RR^d -> RR$ is *strongly convex* if there exists a constant such that
  $ F(bar(w)) >= F(w) + nabla F (w)^T(bar(w)-w) + 1/2 (bar(w)-w)^T c norm(bar(w)-w)_2^2, quad forall (bar(w),w) in RR^d times RR^d $

  Or equivantly if there exists $c>0$, then:
  $ nabla^2 F(w) succ.eq c dot mat(1) $
  ],
  title: [Definition: Strong Convex Function]
)

=== Implication

- $f$ has  *exactly one* minimizer $x^* in RR^d$ with $f^* =^"def" f(x^*)$
- $f(x) - f^* <= 1/(2 lambda) norm(nf(x))^2$: quadratic growth.

=== Interpretation
- Compared to ordinary convexity:
$ f(x) >=f(x) + nf(x)^T (y-x) $
strong convexity adds a quadratic curvature term.

This enforces that the function grows at least quadratically away from any point. This will in the end ensure better convergence
$ f(x) = c/2 norm(x)^2 <- "is convex" $ 
- Monotonixity of gradient (gradient doesn't flip direction)
Strong convexity is equivalent to strong monotonicity of the gradient: 
 
$ (nf(x) - nf(y))^T (x-y) >= x norm(x-y)^2 quad forall x,y $

- Eigenvalue Interpretation:
- Geometric Meaning:

== Smoothness Constant
Definition in Optimization. It is NOT the smoothness in Mathematics $(C^oo)$

Lipschitzness controls the changes in function value, while smoothness controls the changes in gradients. Instead of bounding the gradient we bound the hessian

#definition(
  [We say $f(x)$ is $L bold("-smooth")$ when
  $ f(y) <= f(x) + nf(x)^T (y-x) + L/2norm(y-x)^2, quad L>0 $

  Some alternative definitions
  $L norm(x-x_0)^2 /2 -f(x)$ is covex (upper bounded by a quadratic function)
  $-L I succ.eq nnf(x) succ.eq L I$ ($f$ has Lipschitz Hessian)
  $lang nf(y)-nf(x),y-x rang <= L norm(y-x)^2$ (weaker when not require convexity)
  ],
  title: [Definition (Lipschitz Gradient (Upper Curvature Bound))]
)

#figure(
  image("assets/image-44.png"),
  caption: [Illustration of Strong Convexity and Smoothness]
)

= Secant Method
For univariate functions, if the second derivative (hessian $H$) is unknown, it can be approximated using the secant method

$ f''(x_k) approx (f'(x_k)-f'(x_(k-1)))/(x_k - x_(k-1)) $

and used in the step update

$ x_(k+1) = x_k - (x_k - x_(k-1))/(f'(x_k)-f'(x_(k-1))) f'(x_k) $

It requires an additional initial design point and suffers from the same problems as Newton's
method and may take more iterations to converge due to approximating the second derivative.

This can of course be used for multivarite functions with:
$ x_(k+1) = x_k - (x_k - x_(k-1))/(nf(x_k)-nf(x_(k-1))) nf(x_k) $

= Quasi-Newton Method
Quasi-Newton methods use only gradients (not second derivatives) to iteratively build a model of the objective function.
This produces superlinear convergence — a big improvement over steepest descent, especially on difficult problems.

We use an approximation

