#import "../../../../temp/temp.typ": *

#show: exercise.with(
  title: "Exercise sheet 4",
  course: "AI504 — Knowledge Representation",
  author: "Simon Holm",
  date: "March, 2026",
)

= Exercise $1^+$
Direct methods are able to use only zero-order information, that is, only evaluations of $f$. How many evaluations are needed to approximate the derivative and the Hessian of an $n$-dimensional objective function using finite difference methods? Why do you think it is important to have zero-order methods?
== Solution
In higher dimensions it is very computationally expensive to compute $nf$ and $nnf$. Because of this we resolve to zero-order methods (like methods that use distribution)  

= Exercise $2$

== Solution

= Exercise $3^*$
The Nelder-Mead algorithm has three parameters $alpha, beta,$ and $gam$. How would you approach the problem of tuning these parameters?

== Solution
- $alpha$
  
  scaling factor for the reflection step
    $ x_r = x_m + alpha dot x_m - x_h $

- $beta$
  
  scaling factor for the expansion step
    $ x_e = x_m + beta dot (x_r-x_m) $

- $gam$
  
  scaling factor for the contraction step
    $ x_c = x_m + beta dot (x_h-x_m) $

#pagebreak()

= Exercise $4^*$
Consider the natural evolutionary strategy for an univariate function. Assume the univariate normal distribution as proposal distribution $p(x|theta)=cal(N)(x|mu,sigma^2)$
- Derive the update rule for $theta$
-  after a number of iterations the value of $mu$ becomes equal to $x_*$, that is, the minimum of $f$, what will
be the update rule for $sigma^2$ and what will be the diﬀiculty encountered by the algorithm?

== Solution
We iteratively search for a good distribution $theta$

$ theta_(k+1) = theta_(k) + alpha nabla_theta EE_(x tilde p(dot|theta))[f(x)] $

where

$ EE_(x tilde p(dot|theta))[f(x)] = integral f(x) p(x|mu,sigma^2) dx approx 1/m summ(i=1,n,f/x_i) $

Idearly, we look for $ (mu, sigma^2) -> (x^*,0) $

So lets rewrite this
#align($
nabla_theta EE_(x tilde p(dot|theta))[f(x)] = integral f(x) p(x|mu,sigma^2) dx
\
"since" nabla_theta  log(p(x|theta)) = 1/(p(x|theta)) dot nabla_theta p(x|theta)
\
integral f(x) p(x|mu,sigma^2) dx = integral f(x) nabla_theta log(p(x|theta)) p(x|theta)
\
"using the same rule as before for expected values,"
\
EE_(x tilde p(dot|theta)) [f(x) nabla_theta log(p(x|theta))] apx 1/m summ(i=1,n,f(x_i) nabla_theta log(p(x_i|theta))) 


$)

= Exercise $5^*$

== Solution

= Exercise $6^*$

== Solution