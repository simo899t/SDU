#import "../../../../../../temp.typ": *

#note(
  title: "Lecture 6 - Gradient-based Learning & Backpropagation",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "March, 2026",
)

= The Central Idea

#figure(
  image("/assets/image-24.png"),
  caption: [Updating the model parameters following the steepest slope]
)

The derivative specifies how to scale a small change in input to obtain a corresponding change in the output:
$ f(x+eps) approx f(x) = eps f'(x) $

We then know that $f(x-eps sign(f'(x))) < f(x)$

For this we need partial derivatives
$ ppx(x_i) f(x) $
Measures how $f$ changes as only variable $x_i$ increases at point $x$

Then the gradient contains all the prtial derivatives: $nabla_x f(x)$


Then $x' = x-eps nabla_x f(x)$ "with "$eps$" bein the learning rate"




= Stochastic Gradient Descent (SGD)

"Stochastic" graident is estimated.

In each step of SGD we can sample a minibatch of examples
$ B= {x_1, dots x_(m')} $
- drawn uniformly from the training set
- Minibatch size $m'$ is typically chosen to be small: 1 to a hundred
- Crucially $m'$ is held fixed even if sample set is in billions
- We may fit a training set with billions of examples using updates computed on only a hundred examples
- This is mostly due to comutational limits (practical)
- This also includes randomness which can generally be good

== SGD Estimate using minibatch
We can estimate the gradient by:
$ g = 1/(m')nabla_theta summ(i=1,m',L(x_i,y_i,theta)) $
Using *only* the examples of the minibatch

#figure(
  image("/assets/image-25.png"),
  caption: [The Gradient Descent is not perfect, but good enough in practice]
)
#pagebreak()

== Specialties
Neural Network training not different from ML models with gradient descent. The components are needed:
1. optimization procedure, e.g., gradient descent
2. cost function, e.g., MLE
3. model family, e.g., linear with basis functions

The difference lies in the fact that nonlinearity causes non-convex loss

#figure(
  image("/assets/image-26.png",width: 30em),
  caption: [Convex vs. Non-Convex functions]
)

== Problems

This introduces a number problems..
1. We can end-up in local minima

#figure(
  image("/assets/image-27.png",width: 30em),
  caption: [Example of an example finding a local minima (which is not necessarily the minimizer) ]
)

#pagebreak()

2. Stationary points

#figure(
  image("/assets/image-28.png",width: 30em),
  caption: [Saddle Points where $f'(x)=0$ are neither maxima nor minima]
)
3. Cliffs and Exploding Gradients
Neural networks with many layers have steep regions i.e., cliffs.
Gradient update step can move parameters extremely far, jumping off cliff altogether.

#figure(
  image("/assets/image-29.png",width: 30em),
  caption: [Example of a dangerous cliff]
)
4. Inexact Gradients

Optimization algorithms assume we have access to exact gradient or Hessian matrix. In practice we have a noisy or biased estimate (e.g. using minibatch)
#pagebreak()

5. Bad initial points

Optimization based on local downhill moves
can fail if local surface does not point towards
the global solution

#figure(
  image("/assets/image-30.png",width: 30em),
  caption: [We need a good inital point to find a good minimum]
)

5. The Learning Rate

#figure(
  image("/assets/image-31.png",width: 30em),
  caption: [Loss might not converge when learning rate is too big]
)
Reduce learning rate if no convergence.

= Backpropagation
#figure(
  image("/assets/image-32.png",width: 30em),
  caption: [example of backpropagation usage]
)

We use the chainrule. In Leibnitz notation

if $y = f(u)$ and $u = g(x)$ then
$ ddx y = dd(u)y dot ddx u $

== Decomposing a neural net into functions
Consider $f(x) = W_2 "ReLU"(W_1 x)$ representing a 2-layer NN.

This can be composed into:
$ a(x) = W_1 x, quad z(x) = "ReLU"(x), quad y(x) = W_2 x $

Then $ f(x) y(z(a(x))) $

Then we have the Loss $E(f(x),y)$

== Differentiating the individual functions

Take a unitds weight $a(z) bold(w)^T bold(z) = w_1 z_1, dots w_n z_n$

then $ dd(w_i) a=z_1, quad dd(z_i) a=w_1 $

Take the leaniar unit $"ReLU"(x) = max(0,x)$
$ dd(x')= 1 "if" x>0 "and" 0 "otherwise" $

Loss function example $E(f(x),y) = 1/2 (f(x)-y)^2$

Then $ dd(f(x)) = f(x)-y $

#figure(
  image("/assets/image-34.png",width: 30em),
  caption: [Backpropagation: A Simple Example]
)

== General feedforward networks

Given that $a:j = sum_i w_(j i)z_i$

With an activation function $z_j = a_j$

Then $pp(w_(j i)) E_n = pp(a_j) E_n dot pp(w_(j i)) a_j$

Usually we give the notation that the 'errors': $delta_j = pp(a_j) E_n$

so $ pp(w_(j i)) E_n = delta_j z_i $

We can then backpropagate, each node to learn the $delta_i$ for each node

So for each layer
$ delta_j = underbrace(h'(a_j), "Derivative of
activation function") dot sum_(k) w_(k j)overbrace(delta_(k), "output error") $

#figure(
  image("/assets/image-35.png"),
  caption: [Easy example]
)



