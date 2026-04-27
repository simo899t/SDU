#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 2: Derivatives and Gradients",
  author: "Simon Holm",
  date: "February - 2026"
)

= Definitions for this course
- $[a, b] = {x ∈ R | a ≤ x ≤ b}$ closed interval 
- $(a, b) = {x ∈ R | a < x < b}$ open interval

== Linear combination
- with $v_1,v_2 dots, v_k in RR^n$
- and $lambda = [lambda_1,lambda_2dots,lambda_k]^T in RR^k$
$ x = lambda_1v_1 + dots + lambda_k v_k = sum_(i=1)^k lambda_i v_i $
- Conic combination
- Affine combination
- Convex combination

==  Convex set
If $x,y in S$ and $0<=lambda<=1$ then $lambda x+(1-lambda)y in S$
- This means that for a set and any two point $x,y$ all point in between these points must be inside the set
#figure(
  image("assets/image.png"),
  caption: [examples of non-convex vs convex]
)
== Convex functions


If for any two points $forall x,y in R^n$ with $alpha in [0,1]$ it holds that
$$f(alpha x+(1-alpha)y)>= alpha f(x)+(1-alpha)f(y)$$

#figure(
  image("assets/image-1.png"),
  caption: [In this example graph is convex on some interval while concave (opposite of convex) in some other interval]
)

== Hulls
For a set of points $S psubset RR^n$
- $"lin"(S)$ Linear hull (span)
- $"cone"(S)$ conic hull
- $"aff"(S)$ affine hull
- $"conv"(S)$ convex hull

#figure(
  image("assets/image-2.png"),
  caption: [convex hull are the points surrounding all points (much like a rubber band)]
)

$ "conv"(X) = {lambda_1 x_1+dots+lambda_n x_n | x_i in X, lambda_1, dots,lambda_n >=0 "and" sum_i lambda_1 = 1} $

= Derivatives
== Gradient Vector
$ nabla_S f(x) equiv underbrace(lim_(h->0) (f(x+h s)-f(x))/h, "forward difference") = underbrace(lim_(h->0) (f(x+(h s)/2)-f(x-(h s)/2))/h, "central difference") = underbrace(lim_(h->0) (f(x)-f(x-h s))/h, "backward difference") $

To compute $nabla_s f(x)$:
- Compute $ nabla_s f(x) = (diff f)/(diff x_1)s_1 +(diff f)/(diff x_2)s_2+ dots + (diff f)/(diff x_n)s_n =nabla f(x)^T S = nabla f(x) dot S  $

== Matrix Calculus
$$nabla_x b^T x = nabla_x x^T b = b$$
$$nabla_x x^T A x = (A+A^T)x$$
== Positive definiteness
#figure(
  image("assets/image-3.png"),
  caption: [A matrix A is positive definite if and only if all its eigenvalues are positive. (This can be used for recognition.)]
)
== LU Decomposition
For $$P A = L U$$ Where $L$ is a *lower triangular* matrix, $U$ is an *upper triangular* matrix and $P$ is a *permutation matrix* (obtained by rearranging the rows of $A$)
- Use `LAPACK` (FORTRAN library for python)

= Symbolic Differentiation
Symbolic derivatives can give valuable insight into the structure of the problem domain and, in some cases, produce analytical solutions of extrema (e.g., solving for $diff/(diff x) f (x) = 0$) that can eliminate the need for derivative calculation altogether.

But they do not lend themselves to efficient runtime calculation of derivative values, as they can get exponentially larger than the expression whose derivative they represent


= Numerical Differentiation
Neighboring points are used to approximate the derivative

Such as: 
$ f(x) approx underbrace(lim_(h->0) (f(x+h)-f(x))/h, "forward difference") = underbrace(lim_(h->0) (f(x+(h)/2)-f(x-(h)/2))/h, "central difference") = underbrace(lim_(h->0) (f(x)-f(x-h))/h, "backward difference") $
![[Pasted image 20260210113235.png]]
- The O(n) complexity of numerical differentiation for a gradient in $n$ dimensions is the main obstacle to its usefulness in machine learning, where n can be as large as millions or billions in *state-of-the-art* deep learning models

= Automatic Differentiation

Automatic differentiation techniques are founded on the observation that any function is evaluated by performing a sequence of simple elementary operations involving just one or two arguments at a time:
- addition
- multiplication
- division
- power operation $a^b$
- trigonometric functions
- exponential functions 
- logarithmic $ln(x)$
- chain rule $(d z)/(d x) = (d z)/(d y) dot (d y)/(d x)$