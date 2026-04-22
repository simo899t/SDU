#import "../../../../temp/temp.typ": *

#show: exercise.with(
  title: "Exercise sheet 7",
  course: "AI504 — Knowledge Representation",
  author: "Simon Holm",
  date: "April, 2026",
)

= Exercise $1^+$
Write the update rule for stochastic gradient with mini-batches of size 𝑚 on a generical machine learning model $y = h(x)$ and with loss function $L$.
Write the update formula with momentum in the case of mini-batch of size 𝑚.

== Solution
stochastic gradient with mini-batches of size 𝑚 with minimization of the loss function $L(h(x),y)$
$ w_(k+1) = w_k - alpha_k / m sum_(i in B_k) nabla_w L(h(x_i; w_k), y_i) $

with momentum
$ w_(k+1) = w_k + v_(k+1) $
where $ v_(k+1) = beta v_k - alpha_k / m sum_(i in B_k) nabla_w L(h(x_i; w_k), y_i) $

= Exercise $2^*$
In a regression task we assume $h(x;w) = w_0+w_1 x_1+w_2 x_2+dots + w_d x_d$. For the estimation of the parameters $w in RR^(d+1)$ using the examples ${(x_1,y_1), dots (x_n,y_n)}$ we can use the least squares loss function
$ min R_n (w) = summ(i=1,n,L(h(x_i ;w),y_i)) = min norm(y-X w)^2_2 $

Where $ X = mat(1, x_(11), x_(21), dots, x_(d 1);
                1, x_(11), x_(21), dots, x_(d 1);
                dots.v, dots.down,,;
                1, x_(1n), x_(2n), dots, x_(d n))
                 $
This problem admits a closed form solution by means of the normal equations $w = (X^T X)^(-1) X^T y$. You find
the derivation of this result in these slides from DM579/AI511.
The $L_2$ Regularized risk is
$ min_w R_n (w) + lambda norm(w)_2^2 = summ(i=1,n,L(h(x_i ;w),y_i)) + lambda summ(j=0,d,w_j^2) = norm(y-X w)^2_2 + lambda norm(w)_2^2 $

admits also a closed-form solution: $w = (X^T X+ lambda I)^(-1) X^T y$

Provide a computational analysis of the cost of computing the estimates of $w$ by means of these closed-form
solutions and compare these costs with the cost of carrying out the gradient descent. When is the gradient
descent faster?