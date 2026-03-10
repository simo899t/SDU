#import "../../../../../../temp.typ": *

#show: exercise.with(
  title: "Exercise sheet 4",
  course: "AI504 — Knowledge Representation",
  author: "Simon Holm",
  date: "March, 2026",
)

= Exercise $1^*$
Find examples where each of the four termination conditions would not work individually, showing the importance of having more than one.
== Solution

+ _Maximum iterations_

$ k> k_"max" $

Using max iterations as termination, can lead to problems when a problems takes longer time to solve that there are iterations. fx in the case where $alpha$ (steplength) is very small, we need many iterations, yet dont want to terminate too early.

Also some functions like $f(x) = 1/x$ will never find a minimum.

+ _Absolute improvement_

$ f(x_k) - f(x_(k+1)) < eps_a $

Here an issue can occur if the gradient is too small, in that case, the curve will flatten (e.g a saddlepoint and terminate because it has no improvement).


+ _Relative improvement_

$ f(x_k) - f(x_(k+1)) < eps_r abs(f(x_k)) $

Ensures that max error $eps$ is dependent on the function value so as the function apreaches the minimizer, the max error also decreases.

This becomes a problem when function value doesnt apreach 0, or is negative.

+ _Gradient magnitude_

$ norm(nf(x_(k+1))) < eps_g $

If the gradient gets too small before finding a minimum (like a saddlepoint) it will terminate prematurely

= Exercise $2^*$
What advantage does second-order information provide about the point of convergence that first-order information lacks?

== Solution

We can use the hessian to determine the shape of the function. (we can more easily avoid saddlepoints)

= Exercise $3^*$
When would we use Newton's method instead of the bisection method for the task of finding roots in one dimension?

== Solution

Newtons method uses the Hessian, so it would be favorable when the hessian is available, since it converges faster than bisection




= Exercise $4^*$
Apply Newton's method to $f(x) = 1/2 x^T H x $starting from $x_0 = [1,1]$. What have you observed? Use $H$ as follows:

$ H=mat(1,0;0,1000) $

Next, apply gradient descent to the same optimization problem by stepping with the unnormalized gradient. Do two steps of the algorithm. What have you observed? Finally, apply the conjugate gradient method. How many steps do you need to converge?

Repeat the exercise for:

$ f(x) = (x_1+1)^2 + (x_2+3)^2 +4 $

starting at the origin.

Note that $H=A$, hence we could have derived $A$ also by calculating the Hessian.

== Solution



$ f(x) = 1/2 [x_1,x_2]^T mat(1,0;0,1000) [x_1,x_2] $

Then $ nf(x) =H x $
and $ nnf(x) = H $

By newton method
$ x_(k+1)=x_k-[nnf(x_k)]^(-1) nf(x_k) $
$ x_(k+1)=x_k-H^(-1) H x_k $
$ x_(k+1)=x_k-x_k = 0 $

By GD
#align($ 
qqqquad x_(k+1)&=x_k- alpha nf(x_k) 
\
x_(k+1)&=(I-alpha H)x_k
\
&= mat(1-alpha,0;0,1-1000 alpha) x_k
\
&= mat((1-alpha)^(k+1),0;0,(1-1000 alpha)^(k+1)) x_k
 $)
#pagebreak()

Then  $ 0 < abs(1-alpha)<1 $

#align($ 
0 < abs(1-alpha)<1
\
0 < abs(1-1000alpha)<1
\
0 < alpha < 2/1000
 $)

Now lets do the same on $ f(x) = (x_1+1)^2 + (x_2+3)^2 +4 $

This can be rewtitten to $ 1/2 mat(x_1,x_2)^T A mat(x_1;x_2), quad "where" A = mat(2,0;0,2) $
So now on the form $f(x) = 1/2 x^T A x$ which converges to 0 in 1 step.

= Exercise
Compare Newton's method and the secant method on $f(x) = x^2 + x^4, "with" x_1 = -3 "and" x_0 = -4$. Run each method for 10 iterations. Make two plots:

+ Plot $f$ vs the iteration for each method.
+ Plot $f'$ vs $x$. Overlay the pregression of each method, drawing lines from $(x_i,f'(x_i))$ to $(x_(i+1),f'(x_(i+1)))$ for each transition

What can we conclude about this comparison?

== Solution


= Exercise 6 Strongly Convex Functions

Consider: $ f(x) = x^4 $

- Show that $f$ is convex.
- Compute $f''(x)$
- Show that $f$ is not strongly convex on $RR$
- Explain geometricly why the function fails to be strongly convex

== Solution

= Exercise 7 Strongly Convex Functions