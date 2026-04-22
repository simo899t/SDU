Introduction

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

![[Pasted image 20260203113932.png]]
This is very simple, but it illustrates **incrementally improving a design** until it can no longer be improved or until the budgeted time or cost has been reached.

#### White/glass box
Evaluating an analytical expression. This is expressable mathematically.

#### Black box
Running physical experiments, e.g. wind tunnel tests.
These are more difficult to express mathematically, and will probably not result in a formular/mathematical expression.

Note that computer simulation is also a black box, but it might be less computational expensive.

#### Basic optimization problem
minimize $f(x)$ w.r.t. $x$ where $x in cal(X)$ 

this is formulated as such
$$x^*=arg min_(x in cal(X)) f(x)$$
Here 
- Feasible Set $cal(X)$
- Design Point $x$
- Design Variables
- Objective Function: $f : RR^n → RR$ (scalar-valued function)
- Minimizer

![[Pasted image 20260203120022.png]]

Remember that there is only one global minimum, but there might be many minimizers

Also $$arg max_(x in cal(X)) f(x) equiv arg min_(x in cal(X)) -f(x)$$
#### Constraints
Take the following example
![[Pasted image 20260203120225.png]]
$$x^* = arg min_(x_1,x_2) f(x_1,x_2) quad "where" 0<=x_1,x_2 quad, 1>=x_1+x_2 $$
#### Conditions for local minima

Univariate objective functions
$f'(x^*) = 0$, first-order necessary condition (FONC)
$f''(x^*) >=0$, second-order necessary condition (SONC)

Multivariate objective functions
$nabla f(x^*) = 0$, first-order necessary condition (FONC)
$nabla^2f(x^*) >=0$, second-order necessary condition (SONC)

Note that here $nabla^2f(x^*)$ is the hessian matrix

#### Taylor Expansion
Because of $$f(x+h) = f(x)+ integral_0^h f´(x+a) " "d a $$
then by
![[Pasted image 20260205083725.png]]
$$f(x+h) = sum_(n=0)^(oo) (f^(n)(x))/n! h^n$$
Then for multiple dimentions
$$f(x) =f(a)+nabla f(a)^T (x-a)+1/2(x-a)^T nabla^2 f(a)(x-a) + dots$$

#### Benchmark functions
Classic benchmark functions (Rastrigin, Rosenbrock, Ackley, Sphere, etc.) have well‑studied shapes. We can use this to test whether our optimizer is good or not

Benchmarks tell you whether your optimizer is **competent enough to even deserve being tested on a real ML problem**.

#### Problem clasification
- Univariate $f : RR → RR$ vs Multivariate $f : RR^n → RR$
- Real-valued $f : RR^n → RR$ vs vector functions $f : RR^n → RR^m$
- Linear vs Nonlinear
- Nonlinear: Convex vs Nonconvex, unimodal vs multimodal 
- Constrained vs unconstrained
- Smooth (differentiable) vs non smooth (non differentiable)
- Deterministic vs Uncertain
- Continuous vs Discrete

