
#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Exam preparations",
  course: "AI505 - Optimization",
  author: "Simon Holm",
  date: "June, 2026",
)
#set heading(numbering: none)

= Definitions
- Univariate, Multi-variate, real-valued, vector-valued functions
- Convex, Non-Convex, Concave functions
- Conditions for Local Minima
- Taylor expansion
- Unimodal, Multimodal
- Lipschitz Continuity
- Strongly Convex
- Smoothness
- Positive Definitness

= Derivatives
- Derivatives in Multiple Dimensions
- Numerical Differentiation
- Automatic Differentiation

= Bracketing
Finding an Initial Bracket:
- Fibonacci Search
- Golden Section Search
- Quadratic Fit Search
- Bisection Method

= Local Descent
Line Search:
- Exact Line Search
- Approximate Line Search:
  - Backtracking line search (Armijo line search)
  - Strong backtracking line search (bracketing + zoom)
- Rate of convergence: linear, superlinear, quadratic
- Trust Region Methods
- Termination Conditions

First-Order Methods: 
- Gradient Descent
- Conjugate Gradient
- Momentum (general ideas only):
  - Nesterov Momentum
  - Adagrad
  - RMSProp
  - Adadelta
  - Adam
  - Hypergradient Descent

Second-Order Methods:
- Newton’s Method and convergence results
- Secant Method
- Quasi-Newton Methods:
  - DFP
  - BFGS
  - L-BFGS

Direct Methods:
- Black box optimization
- Cyclic Coordinate Search
- Powell’s Method
- Hooke-Jeeves
- Generalized Pattern Search
- Nelder-Mead Simplex Method

= Beyond Local Optima
- Noisy Descent
- Stochastic Gradient Descent
- Mesh Adaptive Direct Search
- Simulated Annealing
- Cross-Entropy Method
- Natural Evolution Strategies
- Covariance Matrix Adaptation
- Genetic Algorithms
- Differential Evolution
- Particle Swarm Optimization

= Sampling Methods
- Full Factorial
- Random Sampling
- Uniform Projection Plans
- Stratified Sampling
- Space-Filling Metrics
- Space-Filling Subsets
- Quasi-Random Sequences

= Optimization for ML
ML tasks:

- empirical risk minimization
- stochastic gradient
- batch gradient
- mini-batch approach

Beyond stochastic gradient:

- noise reduction methods (dynamic sample size gradient aggregation, iterated averaging)

= Constrained Optimization

- Lagrangian multiplier method
- KKT conditions
- Min-max inequality and Duality
- Penalty Methods
- Interior Point Methods
- Linear Programming
- Duality in LP
- Simplex Method
- Modeling in LP
- Primal-Dual Hybrid Gradient (PDHG) Method

= Discrete Optimization
- Integer Linear Programming
  - Branch and Bound
  - Cutting Planes
- Dynamic Programming
- ROAR-NET API Specification
- Construction Heuristics
- Local Search
- Metaheuristics (only some ideas)