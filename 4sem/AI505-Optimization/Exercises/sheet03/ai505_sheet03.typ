#import "@local/tempst:0.1.0": *

#exercise(
  title: "Exercise sheet 3",
  course: "AI504 — Knowledge Representation",
  author: "Simon Holm",
  date: "February, 2026",
)

= Exercise $bold(1^*)$

Implement the extended Rosenbrock function

$ f(x) = summ(i=1, n/2,[a(x_(2 i)-x_(2 i-1)^2)^2+(1-x_(2 i-1))^2]) $

where $a$ is a parameter that you can vary (for example, 1 or 100).

 The minimum is $x^* = [1, 1, dots , 1], f(x^*) = 0$.

Consider as starting point $[-1, -1, dots, 1]$.

Solve the minimization problem with `scipy.optimize` using all methods seen in class that are suitable for this task. Observe the behavior of the calls for various values of parameters.
Use the #link("https://coco-platform.org/")[COCO test suite] (see #link("https://www.tandfonline.com/doi/full/10.1080/10556788.2020.1808977")[article]) to carry out this exercise. The advantages of the platform is that it provides:
- a set of problem instances to use, about 1000 to 5000 problems (number of functions $times$ number of dimensions $times$ number of instances)
- a collection of results from the literature
- tools to launch and analyze the experiments

The COCO framework considers functions divided in suites. Functions, $f_i$, within suites are distinguished by their identifier $i = 1, 2, dots $ They are further parametrized by the (input) dimension, $n$, and the instance number, $j$. We can think of $j$ as an index to a continuous parameter vector setting. It parametrizes, among other things, search space translations and rotations. In practice, the integer $j$ identifies a single instantiation
of these parameters. We then have:

$ f_i^j eq.triple f[n,i,j]: RR^n -> RR qquad x |-> f_i^j(x) =f[n,i,j](x) $

Varying $n$ or $n$ leads to a variation of the same function $i$ of a given suite. Fixing $n$ and $j$ of function $f_i$ defines an optimization problem instance $(n,i,j) eq.triple (f_i ,n,j)$ that can be presented to the solver. Each problem receives again an index within the suite, mapping the triple $(n, i, j)$ to a single number.

Varying the instance parameter $j$ represents a natural randomization for experiments in order to:
- generate repetitions on a single function for deterministic solvers, making deterministic and non-
deterministic solvers directly comparable (both are benchmarked with the same experimental setup)
- average away irrelevant aspects of the function definition,
- alleviate the problem of overfitting, and
- prevent exploitation of artificial function properties

== Solution


