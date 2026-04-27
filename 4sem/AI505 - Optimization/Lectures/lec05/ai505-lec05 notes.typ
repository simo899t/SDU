
#let title = "Lecture 5: First-Order Methods"
#let author = "Simon Holm"
#let date = "March - 2026"

#import "../../../../temp/temp.typ": *

 #note(
   title: title,
   author: author,
   date: date
 )

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

== Conjugate Direction
Def.: $A$ set of nonzero vectors ${d_0, d_1, dots , d_ell}$ is said to be conjugate with respect to the symmetric positive definite matrix $A$ if
$ d_i^T A d_j = 0, quad "for all "i!=j $

These are defined as 
$ x^*-x_0 = sigma_0d_0 + sigma_1d_1 + dots + sigma_(n-1)d_(n-1) $

Then we can prove that $sigma_k = alpha_k$ (proof in lecture)

Then $ x^* =x_0+ alpha_0d_0 + alpha_1d_1 + dots + alpha_(n-1)d_(n-1) $
#pagebreak()

== Construction of conjugate vectors
n
generating its set of conjugate vectors, it can compute a new vector $d_k$ by using only the previous vector $d_(k-1)$. Hence, little storage and computation requirements.

$ d_k = -r_k + beta_k d_(k-1) $

We wish to find $ beta_k = (r_k^T A d_(k-1))/(d_(k-1)^T A d_(k-1)) $
Because of the constraint $d_k^T A d_k = 0$

#image("assets/image.png")


== Conjugate Descent 

#image("assets/image-36.png")

= Momentum
Use momentum of descent to find better minimum
$ x_(k+1) = x_k + v_(k+1) $
where $ v_(k+1) = beta v_k - alpha nf(x_k). $

== Nesterov Momentum
Lets slow down enough at the bottom of a valley, do they don't overshoot.

$ x_(k+1) = x_k + v_(k+1) $
where $ v_(k+1) = beta v_k - alpha nf(x_k+beta v_k). $

== Adagrad

$ x_(i, k+1) = s_(i,k)-alpha/(eps+sqrt(s_(i,k))) nabla f_i(x_k) $
where
$ s_(i,k) = summ(j=1,k,(nabla f_i (x_k))^2) $
and $ eps approx 1 times 10^(-8), quad alpha = 0.01 $

== RMSProp (roots mean square)

Extends Adagrad to avoid monotonically decreasing learning rate by maintaining a decaying average of squared gradients

We can do this by the following

$ x_(i,k+1) = x_(i,k) - alpha/(eps + "RMS"(nabla f_i (x_k))) nabla f_i (x_k) $

where 

$ "RMS"(x) = sqrt(1/n (x_1^2+x_2^2+dots+x_n^2)) $

Note that this take the full average of the gradients

We can also use only 2, to prioritize the points where we have reached, usign a parameter $gam$ we can have more control over the momentum rather than just using an average.

$ x_(i,k+1) = x_(i,k) - alpha/(eps + sqrt(pred(s)_(i,k))) nabla f_i (x_k) $
where

$ pred(s)_(k+1) = gam pred(s)_k + (1-gam) (nf(x_k) dot.o nf(x_k)), quad gam in [0,1] $

== AdaDelta
extends Adagrad to avoid monotonically decreasing learning rate
Modifies RMSProp to eliminate learning rate parameter entirely (e.g the learning rate is set to the non learnable average change of $x$)

$ x_(i,k+1) = x_(i,k) - ("RMS"(Delta x_i))/("RMS"(nf_i (x_k))) nabla nf_i (x_k) $

Using AdaDelta, its also common to use

$ x_(i,k+1) = x_(i,k) - ("RMS"(Delta x_i))/(eps + sqrt(pred(s)_(i,k))) nabla f_i (x_k) $
where

$ pred(s)_(k+1) = gam pred(s)_k + (1-gam) (nf(x_k) dot.o nf(x_k)), quad gam in [0,1] $
#pagebreak()

== Adam (adaptive moment estimation method)

Combination of previous methods, though adam adapts the learning rate to each parameter.

At each iteration the following values are computed

+ Biased decaying momentum $ v_(k+1) = beta v_k - alpha nf(x_k) $
+ Biased decaying squared gradient $ s_(k+1) = gam s_k + (1-gam) (nf(x_k) dot.o nf(x_k)) $
+ Corrected decaying momentum $ pred(v)_(k+1) = (v_(k+1))/(1-gam_(v,k)) $
+ Corrected decaying squared gradient $ pred(s)_(k+1) = (s_(k+1))/(1-gam_(s,k)) $
+ Finlly the next iteate $ x_(i,k+1) = x_(i,k) - (alpha pred(v)_(k+1))/(eps + sqrt(pred(s)_(k+1))) $

This model has default values set to $alpha = 0.001, gam_v = 0.9, gam_s = 0.999, eps = 1 times 10^(-8)$

== Adamax
Same as Adam, but the bias decaing "squared" gradient is based on the max-norm $L_oo$.


$ s_(k+1) = gam^oo s_k + (1-gam^oo) (norm(nf(x_k))_oo \
 = max(gam s_k, norm(nf(x_k))_oo) $

== Nadam
We have seen that Nesterov is often more efficient

Nadam is just Adam which uses the Nesterov momentum

== Hypergradient Descent
Learning rate determines how sensitive the method is to the gradient signal. A good learning rate is important since many accelerated descent methods are highly sensitive to parameters such as learning
rate.

We can then use gradient descent to the parameters of an underlying gradient descent.