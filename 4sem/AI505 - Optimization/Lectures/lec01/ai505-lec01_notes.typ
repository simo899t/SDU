#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 1: Linear Constrained Optimization",
  author: "Simon Holm",
  date: "April - 2026"
)

= Introduction to this course
- Continuous multivariate optimization
	- Single-valued, single-variate
		- $f: RR -> RR$
	- Single-valued, multivariate
		- $f: RR^n -> RR$
	- Vector-valued, multivariate
		- $f: RR^n -> RR^n$
		- **Not correiculum**

- Discrete
	- $f:ZZ^+_0 -> RR$

Contents of the course
- Introduction, univariate problems
- Multivariate Problems, Gradient-Based Methods
- Derivative free methods
- Optimization for Machine Learning
- Constrained Optimization, Linear Programming
- Sampling methods
- Discrete Optimization and Heuristics

Communication Means
	GitHub $->$ https://ai-505.github.io
	ItsLearning
	Write to Professor Marco ($"marco@imada.sdu.dk"$)
	Alternatively to instructor Sai $"(sgnagarajan@imada.sdu.dk)"$

Assessments 
- Mandatory assignments in groups of 2:
	- Assignment 1
	- Assignment 2
	- Oral exam on June 29-30, 2026

Oral exam consists of questions based on the assignments + can be extended to cover other parts of the curriculum

Final grade: primarily based on the assignments but the oral exam may adjust the grade up or down by one grade level.

Exercise sessions
- $+$ are done before class
- $*$ are done in class
- unmarked are for self study
Note that are good examples of assignment questions.

Schedule for the year
![[Pasted image 20260203122456.png]]

#pagebreak()


Objectives to optimize
- Efficiency
- Safety
- Accuracy
Constraints
- Cost
- Weights
- Structural integrity
Challenges
- High-Dimensional Search Spaces
- Multiple Competing Objectives
- Model Uncertainty

Model, Representation, Implementation
- Model: mathematical object in some class $M$
- Representation: An object of an abstract data type $R$ used to store the model
- Implementation: An object of a concrete type used to store the model.

- Any object from the real world might have different models. 
- Any model might have several representations (exact). 
- Any representation might have different implementations (exact).

We will focus on the algorithmic aspects of optimization that arise after the problem has been properly formulated

= The optimization process
Multiple different designs for optimization a process

#figure(
  image("assets/image.png"),
  caption: [This is very simple, but it illustrates *incrementally improving a design* until it can no longer be improved or until the budgeted time or cost has been reached.]
)

== White/glass box
Evaluating an analytical expression. This is expressible mathematically.

== Black box
Running physical experiments, e.g. wind tunnel tests.
These are more difficult to express mathematically, and will probably not result in a formula/mathematical expression.

Note that computer simulation is also a black box, but it might be less computational expensive.

== computer simulations

= Basic optimization problem
minimize $f(x)$ w.r.t. $x$ where $x in cal(X)$ 

this is formulated as such
$ x^*=arg min_(x in cal(X)) f(x) $
Here 
- Feasible Set $cal(X)$
- Design Point $x$
- Design Variables
- Objective Function: $f : RR^n → RR$ (scalar-valued function)
- Minimizer

#figure(
  image("assets/image-1.png"),
  caption: [Example of a basic optimization problem.]
)

Remember that there is only one global minimum, but there might be many minimizers

Also $ arg max_(x in cal(X)) f(x) equiv arg min_(x in cal(X)) -f(x) $
==  Constraints
Take the following example

#figure(
  image("assets/image-2.png"),
  caption: align($ min_x f(x) \
  s.t. quad x_1 &>= 0\
  x_2 &>= 0\
  x_1+x_2 &<= 1\
  \
  x in RR^2 $)
)

$ x^* = arg min_(x_1,x_2) f(x_1,x_2) quad "where" 0<=x_1,x_2 quad, 1>=x_1+x_2  $
=  Conditions for local minima

Univariate objective functions
$f'(x^*) = 0$, first-order necessary condition (FONC)
$f''(x^*) >=0$, second-order necessary condition (SONC)

Multivariate objective functions
$nabla f(x^*) = 0$, first-order necessary condition (FONC)
$nabla^2f(x^*) >=0$, second-order necessary condition (SONC)

Note that here $nabla^2f(x^*)$ is the hessian matrix
#pagebreak()

=  Taylor Expansion
Because of $ f(x+h) = f(x)+ integral_0^h f´(x+a) " "dif a $
then by
![[Pasted image 20260205083725.png]]
$ f(x+h) = sum_(n=0)^(oo) (f^(n)(x))/n! h^n $
Then for multiple dimensions
$ f(x) =f(a)+nabla f(a)^T (x-a)+1/2(x-a)^T nabla^2 f(a)(x-a) + dots $

= Benchmark functions
Classic benchmark functions (Rastrigin, Rosenbrock, Ackley, Sphere, etc.) have well‑studied shapes. We can use this to test whether our optimizer is good or not

Benchmarks tell you whether your optimizer is **competent enough to even deserve being tested on a real ML problem**.

=  Problem clarification
- Univariate $f : RR → RR$ vs Multivariate $f : RR^n → RR$
- Real-valued $f : RR^n → RR$ vs vector functions $f : RR^n → RR^m$
- Linear vs Nonlinear
- Nonlinear: Convex vs Nonconvex, unimodal vs multimodal 
- Constrained vs unconstrained
- Smooth (differentiable) vs non smooth (non differentiable)
- Deterministic vs Uncertain
- Continuous vs Discrete

