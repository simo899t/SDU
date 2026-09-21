#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         "Lecture 3: Optimization Recap",
  author:        "Simon Holm",
  course:        "AI508 — Algorithmic Game Theory",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

#definition(title: "Definition: Convex Set")[
  A set $cal(X) subset RR^d$ is _convex_ if, for every $x,y in cal(X)$ and every $lam in [0,1]$,
  $ lam x + (1-lam)y in cal(X) $ 
  The point $lam x + (1-lam)y$ is a _convex combination_ of $x$ and $y$. More generally if $lam_i >= 0$ and $sum_(i=1)^(m) lam_i = 1$ then,
  $ sum_(i=1)^(m) lam_i x_i $
  is a convex combination of the point $Set(x)$
]

Important examples of convex sets include affine subspaces, half-spaces, Euclidean balls,
boxes, polytopes, and the probability simplex
$ Del_d def {x in RR^d : x_i >= 0 space forall i, sum_(i=1)^(d) x_i = 1} $

#figure(
  image("assets/image.png"),
  caption: [n a convex set, every line segment between two feasible points remains feasible],
) <label>

#pagebreak()

= Convex Functions
To define convexity of a function, we assume that its domain is convex.

#definition(title: "Definition: Convex function")[
  Let $cal(X) subset RR^d$ be convex. A function $f: cal(X) to RR$ is _convex_ if, for all $x,y in cal(X)$ and $lam in [0,1]$,
  $ lam x + (1- lam) y <= lam f(x) + (1- lam) f(y). $

  Geometrically, the value of a convex function on a line segment lies below the straight line
joining the function values at the endpoints.
]

Note that a function $f$ is _concave_ if $-f$ is convex. It is _strictly convex_ if the definition inequality is strict whenever $x!=y$ and $lam in (0,1)$.

#example(title: "Example: Convex functions")[
  The functions
  $ f(x) = x^2, quad f(x) = norm(x)^2_2, quad f(x)=e^x $
  are convex. A linear function $f(x)$ is both convex and concave 
]

= Jensen's inequality
The two-point definition immediately extends to any finite convex combination.
#theorem(title: "Theorem: Jensen's inequality")[
  Let $f: cal(X) -> RR$ be _convex_. Let $x_1,dots,x_m in cal(X)$ and let $lam_i >= 0$ satisfy $sum_(i=1)^(m) lam_i = 1$. Then
  $ f(sum_(i=1)^(m) lam_i x_i) <= sum_(i=1)^(m) lam_i f(x_i). $ 
]


#corollary(title: "Corollary: Jensen's inequality on a discrete random variable")[
  If $X$ is a finitely supported random variable and $f$ is convex, then
  #boxed($ f(EE[X]) <= EE(f(x)). $)
]
#pagebreak()

= First-order characterization of convexity
For differentiable functions, convexity can be characterized by tangent hyperplanes.
#theorem(title: "Theorem: First-order characterization")[
  Let $cal(X) subset RR^d$ be convex and let $f:cal(X) to RR$ be differentiable. Then $f$ is convex if and only if, for all $x,y in cal(X)$,
  #boxed($ f(x) >= f(y) +  chevron(nabla f(y)\, x - y) $)

  So for a differentiable convex function, the tangent hyperplane at any point is a *global lower bound* on the function.
]

#figure(
  image("assets/image-1.png"),
  caption: [For a differentiable convex function, every tangent line lies below the graph.],
) <label>
#pagebreak()

= Strong convexity and smoothness

#definition(title: "Definition: Strong convexity")[
  A differentiable function $f: cal(X) to RR$ is $alpha$-_strongly convex_ for $alpha >0$, if for all $x,y in cal(X)$,
  $ f(x) >= f(y) + chevron(nabla f(y) \, x-y) + alpha/2 norm(x-y)^2_2 $
  Strong convexity says that the function lies not merely above its tangent hyperplane, but above a quadratic bowl around that tangent hyperplane.
]

A different property (_smoothness_) controls how quickly the gradient can change.

#definition(title: "Definition: Smoothness")[
  A differentiable function $f: cal(X) to RR$ is L-_smooth_ if
  $ norm(nabla f(x) - nabla f(y))_2 <= L norm(x-y)_2 $
  more intuitively:
  $ (norm(nabla f(x) - nabla f(y))_2)/(norm(x-y)_2) <= L $
  for all $x,y in cal(X)$
]

For an $L$-smooth function one has the useful upper quadratic bound
$ f(x) <= h(y) + chevron(nabla f(y)\, x-y) + L/2 norm(x-y)_2^2 $
Thus, strong convexity gives a quadratic lower bound and smoothness gives a quadratic upper bound.

#pagebreak()

= Second-order conditions
When a function is twice differentiable, its _Hessian_ provides a convenient way to test convexity.

#theorem(title: "Theorem: Second-order characterization")[
  Suppose $f$ is _twice differentiable_ on an open convex set containing $cal(X)$. Then
  $ f" is convex" bii nabla^2 f(x) succ.eq 0 quad forall x in cal(X) $ 
  Here $nabla^2 f(x) succ.eq 0$ means that
  $ tran(v)nabla^2 f(x) >= 0 forall v in RR^d $
  Hessian is _positive semidefinite_.
]

The same idea can be applied for $alpha$_-strongly convex_
$ nabla^2 f(x) succ.eq alpha I iimp "h is "alpha"-strongly convex" $
and, for twice differentiable convex $f$
$ 0 succ.eq nabla^2 f(x) succ.eq L I bii f" is "L"-smooth" $


#example(title: "Example: ")[
  For $ f(x) = 1/2 tran(x) Q x + tran(c) x + r, $
  where $Q = tran(Q)$, we have
  $ nabla f(x) = Q x + c, quad nabla^2 f(x) = Q $
  Hence $f$ is convex exactly when, $Q succ.eq 0$, and it is $alpha$-strongly convex when $Q succ.eq alpha I$.
]
#pagebreak()

= Usefulness of convexity: local and global minima
The central structural fact is that convex functions does not have "bad" local minima.

#theorem(title: "Theorem: Local minima are global under convexity")[
  Let $cal(X)$ be convex and let $f: cal(X) to RR$ be convex. Every local minimizer of $f$ over $cal(X)$ is a *global* minimizer
]

Strict convexity gives _uniqueness_

#theorem(title: "Theorem: Uniqueness under strict convexity")[
  If $f$ is strictly convex on a convex set $cal(X)$, then $f$ has at most one global minimizer. Consequently, if a minimizer exists, it is #underline[*unique*].
]

= First-order optimality over a convex set
For constrained convex optimization, the gradient need not be zero at an optimum: the boundary may prevent us from moving in the direction of $-nabla f$. The correct condition is a variational inequality

#theorem(title: "Theorem: First-order optimality condition")[
  Let $cal(X)$ be convex and $f:cal(X) to RR$ be differentiable and convex. A point $x^* in cal(X)$ is a _Global minimizer_ if and only if 
  #boxed($ chevron(nabla f(x^*)\, x-x^*) >= 0 quad forall x in cal(X). $)
]

If $x^*$ is an interior point of $cal(X)$, then we may move a small amount in both directions along every coordinate. The condition above then reduces to 
$ nabla f(x^*) = 0 $

#pagebreak()

= Gradient descent and projected gradient descent
For an unconstrained differentiable objective, gradient descent repeatedly moves opposite to the gradient. For smooth convex objectives, appropriate step sizes yield convergence to a global minimizer.

#boxed($ x_kplus = x_k - alpha_k nabla f(x_k). $)

The stepsize $alpha_k > 0$ controls how far we move.

When the feasible set is a proper subset $cal(X) psubset RR^d$, a gradient step may leave the feasible region. Projected gradient descent corrects this by projecting back:
#boxed($ x_kplus = Pi_(cal(X)) (x_k - alpha_k nabla f(x_k)), $)

where $ Pi_(cal(X)) (z) def arg min_(x in cal(X)) norm(x-z)_2 $
For closed convex $cal(X)$, the Euclidean projection exists and is unique.

= Constrained optimization: the Lagrangian and KKT conditions
Consider the constrained marginalization problem
#set math.equation(numbering: "(1)")
$ min_(x in cal(X)) f(x) \ st g_i (x) &<= 0, quad i =1,dots,m \ h_j (x) &= 0, quad j =1,dots,r $ <constrained-opt>
#set math.equation(numbering: none)
We associate a nonnegative multiplier $lam_i$ with each inequality constraint and an unrestricted multiplier $mu_i$ with each equality constraint.

#definition(title: "Definition: Lagrangian")[
  The _Lagrangian_ of the problem above [@constrained-opt].
  #boxed($ cal(L) (x,lam,mu) = f(x) + sum_(i=1)^(m) lam_i g_i (x) + sum_(i=1)^(r) mu_i h_i (x) $)
  At an optimum, the Lagrange multipliers measure how the active constraints balance the gradient of the objective.
]
#pagebreak()

#theorem(title: "Theorem: Karush-Kuhn-Tucker conditions")[
  Suppose $f$ and $g_1,dots,g_m$ are differentiable convex functions and the equality constraints $h_j$ are affine. Under a standard constraint qualification such as Slater's condition, a feasible point $x^*$ is optimal if and only if there exists multipliers $lam^*, mu^*$ satisfying:
  #set enum(numbering: "(i)")
  + *Primal feasibility*: $ g_i (x^*) <= 0, quad h_j (x^*) = 0. $
  + *Dual feasibility*: $ lam_i^* >= 0. $
  + *Stationarity*: $ nf(x^*) + sum_(i=1)^(m) lam_i^* nabla g_i (x^*) + sum_(j=1)^(r) mu_j nabla h_j (x^*) = 0 $
  + *Complementary slackness*: #boxed($ lam_i^* g_i (x^*) = 0 quad forall i $)
  #set enum(numbering: "1.")
]

Slater’s condition means, roughly, that there exists a feasible point satisfying all nonlinear
inequality constraints strictly. It is a convenient condition that rules out degenerate pathologies.

#example(title: "Example: A one-dimensional KKT calculation")[
  Consider
  $ min_x x^2 \ st x >=1. $
  Write the constraint as $ g(x) = 1-x <= 1 $
  The Lagrangian is $ cal(L)(x,lam)_(lam>=0) = x^2+lam(1-x) $
  The KKT conditions are
  $ x>=1, quad lam>=0, 2x-lam=0, quad lam(1-x)=0. $
  If $lam=0$, stationary gives $x=0$, which is infeasible. Hence the constraint must be active:
  $ x^* = 1. $
  Stationarity then gives $ lam^* = 2. $
  Thus the constrained optimum is $x^* = 1$, even though the unconstrained minimizer of $x^2$ is $0$
]

= Linear programming
A Linear program (LP) optimizes a linear objective over a feasible set defined by linear constraints. LPs are among the most important optimization problems in algorithms, economics, operations research and game theory.

== Primal Linear Programming
#example(title: "Example: Primal LP")[
  One convenient primal form is
  $ max_(x in RR^n) &tran(c)x #comment[primal problem]) \ st A x &<= b, \ x &>=0 $
  where $A in RR^(m times n)$, $b in RR^m$ and $c in RR^n$.
]

The feasible region of an LP is a polyhedron. When bounded, it is a polytope. Many equivalent LP forms are used in the literature. Equalities can be represented by two inequalities, unrestricted variable can be written as differences of nonnegative variables, and minimization can be converted to maximization by negating the objective.

#figure(
  image("assets/image-2.png"),
  caption: [An LP optimizes a linear objective over a polyhedral feasible region. When an
optimum exists on a bounded polytope, at least one optimal solution is a vertex.],
)

== Dual Linear Programming
#example(title: "Example: Dual LP")[
  Associated with the primal problem is the _dual_ problem
  $ max_(x in RR^n) &tran(b)y #comment[dual problem]) \ st &tran(A) y >= c, \ &y >=0 $ 
]
The primal has $n$ variables and $m$ inequality constraints; the dual has $m$ variables and $n$ inequality constraints. The dual variables can be interpreted as prices or shadow values attached to the primal constraints.

== Weak duality
#theorem(title: "Theorem: Weak duality")[
  For every _primal-feasible_ $x$ and _dual-feasible_ y,
  #boxed($ tran(c)x <= tran(b)y $)
]
== Strong duality
The remarkable fact for LPs is that, under the usual feasibility/boundedness assumptions, the best such upper bound is exact

#theorem(title: "Theorem: Strong duality for LP")[
  If the primal LP has a finite optimal value, then the dual has an optimal solution and
  #boxed($ max_(x "feasible") tran(c)x = min_(y "feasible") tran(b)y $)
  Equivalently, whenever _both_ sides admit optimal solutions $x^*, y^*$
  $ tran(c)x^* = tran(b)y^* $
]
We will use strong duality as a standard theorem. There are several proofs, for example
through the separating-hyperplane theorem, _Farkas’ lemma_, or the *simplex method*.

== Complementary slackness
Strong duality gives an especially useful way to recognize optimal primal–dual pairs.
#theorem(title: "Theorem: Complimentary slackness")[
  Let $x^*$ be primal feasible and $y^*$ dual feasible. They are both optimal of and only if
  #boxed($ y_i^* (b_i - (A x^*)_i) = 0 quad i = 1,dots,m $)
  and
  #boxed($ x_j^* ((tran(A) y^*)_j - c_j) = 0 quad j = 1,dots,n $)
]
Complementary slackness says, informally:
-  dual variable can be positive only when the corresponding primal constraint is tight;
-  primal variable can be positive only when the corresponding dual constraint is tight.
#example(title: "Example: Primal, dual and complementary slackness")[
  Consider

  $ max_(x_1,x_2 >=0) &3x_1 + 2x_2 \ st &x_1 + x_2 <= 4, \ &x_1 <=2, \ &x_2 <= 3. $

  The dual is
  $ max_(x_1,x_2 >=0) &4y_1 + 2y_2 + 3y_3 \ st &y_1 + y_2 <= 3, \ &y_1 <=2. $
  Take $ x^* = (2,2), quad y^* = (2,1,0). $

  The primal value is $ 3(2)+2(2)=10 $

  and the dual value is $4(2)+2(1) + 3(0) = 10$

  Thus weak duality already certifies that both solutions are optimal.

  Complementary slackness is also visible
  - he first and second primal constraints are tight, and their dual variables $y_1^*, y_2^*$ may be positive
  - the third primal constraint is slack $(x^*_2 = 2 < 2)$, and indeed $y_3^*$;
  - both primal variables

]







= Exercises

#question(title: "2.34")[

]
#answer[

]

#question(title: "2.35")[

]
#answer[

]