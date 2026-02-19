
#let title = "Lecture 5: First-Order Methods"
#let author = "Simon Holm"
#let date = "19/02/2026"

#import "../../../../../../temp.typ": *

 #sdu-title(
   title: title,
   author: author,
   date: date
 )

#pagebreak()

// Your content starts here


= Intro
This covers ways to select a descent direction.
- first-order methods that rely on gradient
- second-order methods that rely on Hessian information

Advantages of first order methods:
- cheap iterations: good for small and large scale optimization
- helpful because easy to warm restart
Limitations of first order methods:
- not hard to find challenging instances for them.
- can converge slowly.

= Gradient Descent

We know that the steepest descent is the opposite to the gradient
$ -nabla f(x_k) $

This is normalised steepest descent
$ d_k= -(nabla f(x_k))/norm(nabla f(x_k)) $

#code(
  ```py 
  class DescentMethod:
    alpha: float
  class GradientDescent(DescentMethod):
    def __init__(self, f, grad, x, alpha):
      self.alpha = alpha
    def step(self, f, grad, x):
      alpha, g = self.alpha, grad(x) 
      return x - alpha * g
  ```
) 

== Orthogonal next direction


== Conjugate Direction
Def.: $A$ set of nonzero vectors ${d_0, d_1, dots , d_ell}$ is said to be conjugate with respect to the symmetric positive definite matrix $A$ if
$ d_i^T A d_j = 0, quad "for all "i!=j $

These are defined as 
$ x^*-x_0 = sigma_0d_0 + sigma_1d_1 + dots + sigma_(n-1)d_(n-1) $

Then we can prove that $sigma_k = alpha_k$ (proof in lecture)

Then $ x^* =x_0+ alpha_0d_0 + alpha_1d_1 + dots + alpha_(n-1)d_(n-1) $

== Construction of conjugate vectors
n
generating its set of conjugate vectors, it can compute a new vector $d_k$ by using only the previous vector $d_(k-1)$. Hence, little storage and computation requirements.

$ d_k = -r_k + beta_k d_(k-1) $

We wish to find $ beta_k = (r_k^T A d_(k-1))/(d_(k-1)^T A d_(k-1)) $
Because of the constraint $d_k^T A d_k = 0$





#image("/assets/image.png")