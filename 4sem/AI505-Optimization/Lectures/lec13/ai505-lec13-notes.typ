#import "@local/tempst:0.1.0": *
#show: note.with(
  title: "Lecture 13: Constrained Optimization",
  author: "Simon Holm",
  date: "April - 2026"
)

// Your content starts here

= Contrained Optimization

Minimizing an objective subject to design point restrictions called *constraints*

- New optimization problem statement
$qquad min_x & f(x) \ "s.t." & x in cal(X) $

Where $cal(X)$ is the *feasible set* 
- $RR^n$ or $ZZ^m$ contrained to a subset
#figure(
  image("assets/image.png"),
  caption: [1 dimention of constraints, where the solution changes]
)

#figure(
  image("assets/image-1.png",width: 20em),
  caption: [2 dimentions]
)

- Generally, constraints are formulated using two types:
  + Equality constraints: $h(x) = 0$
  + Inequality constraints $g(x) <= 0$

#pagebreak()

= Transformations to Remove Constraints
If necessary, some problems can be reformulated to use constraints in the objective function


Let $x$ be constrained between $a$ and $b$
$ x = t_(a,b) (hat(x)) = (b+a)/2 + (b-a)/2 ((2 hat(x))/(1+hat(x)^2)) $
#figure(
  image("assets/image-2.png", width: 20em),
  caption: [transformation]
)

= Lagrangian Multiplier Method
The method of Lagrangian Multipliers is used to optimize a function subject to (equality) constraints

(only equality constraints, critical points, because gradient of $f$ and the gradient of $h$ are aligned)

So instead of
$qquad min_x & f(x) \ "s.t." & x in cal(X) $

We can use the *Langrangian function*
- $cal(L)(x,lambda) = f(x) - lambda h(x)$
- Then $nabla_x cal(L)(x,lambda) = 0 " and " nabla_lambda cal(L)(x,lambda) = 0$
- This gets $nf(x) = lambda nabla h(x) qquad h(x)=0$
- Then solve for $x$ and $lambda$

Intuitively, the method of Lagrange multipliers finds the point $x^*$ where the constraint function is orthogonal to the gradient (since $nabla h$ is normal to $h$)

*_Note: this only works wiht equallity constraints_*
#pagebreak()

== Inequality Constraints

#figure(
  image("assets/image-1.png",width: 20em),
  caption: [For inequality constraints, the local behavior fo the gradient in the exact meeting point of two constraint functions is a ned gradient that cannot be improved.]
)

$ nf(arrow(x)^*) = lambda_1 nabla g_1 (arrow(x)^*) + lambda_2 nabla g_2 (arrow(x)^*) \ lambda_1,lambda_2 >=0 $

#figure(
  image("assets/image-3.png", width: 20em),
  caption: [The new gradient vector composed of two gradients]
)

#pagebreak()




Lets do the Langrangian Function Method 
$ cal(L)_oo (x) = f(x) + oo (g(x)>0) $
Though this is impractical because it is discontinuous and nondifferentiable.

- Instead, for $mu>= 0:$
    + $cal(L)(x,mu) = f(x) + mu g(x)$
    + $cal(L)_oo (x) =max_(mu) cal(L)(x,mu)$
- For $x$ infeasible $cal(L)oo (x) = oo$; for $x$ frasible $cal(L)_oo (x) = f(x)$
- The new optimization problem becomes
  $ min_x max_(mu>=0) cal(L)(x,mu) $
This is called the *primal problem*

== Necessary Conditions - KKT Conditions (Karush-Kuhn-Tucker)

For (FONC) $x^*$ to be a critical point then we need

$cases(
  nabla f(arrow(x)^*) = arrow(lambda) nabla arrow(g)(arrow(x)^*),
  arrow(g)(arrow(x)^*) >= 0,
  arrow(lambda) >= 0,
  arrow(lambda) dot arrow(g)(arrow(x)^*) = 0,
)$

Particular cases:
- $f$ is concave, $g$ is convex, then KKT are also sufficient
- $x^*$ interior point
-  Patological cases where they do not hold (formal expressions of the conditions include assumptions to avoid these cases).

- Generalized Lagrangian Function:
$ cal(L)(x,mu,lambda) = f(x) + sum_i mu_i g_i (x) + sum_i lambda_i h_i (x) $
- The *primal form* of the optimization problem
$ min_x max_(mu>=0,lambda) cal(L)(x,mu,lambda) $
- Reversing the order of operations leads to the *dual form*
$ max_(mu>=0,lambda) min_x cal(L)(x,mu,lambda) $
#pagebreak()

#theorem(
  title: [Theorem (Max-min inequality)],
  [For any function $f: Z times W -> RR$,
  $ underbrace(sup_(z in Z) inf_(w in W) f(z,w), "dual") <= underbrace(inf_(w in W) sup_(z in Z) f(z,w), "primal") $]
)

For us:
$ max_(mu>=0,lambda) min_x cal(L)(x,mu,lambda) <= min_x max_(mu>=0,lambda) cal(L)(x,mu,lambda) $

Therefore, the solution to the dual problem $d^*$ is a lower bound to the primal solution $p^*$

= Penalty methods
#figure(
  image("assets/image-4.png"),
  caption: [Penalty by counting violations]
)

#figure(
  image("assets/image-5.png"),
  caption: [pentalty methods]
)

- Count penalty:
  $ p_"count" (x) = (sum_i g_i (x) >0)  + sum_j (h_j (x)!= 0) $
- Quadratic penalty:
  $ p_"quadratic" (x) = sum_i max(g_i (x),o)^2  + sum_j h_j (x)^2 $
- Mixed penalty:
  $ p_"mixed" (x) = rho_1 p_"count"(x) + rho_2 p_"quadratic"(x) $

#figure(
  image("assets/image-6.png"),
  caption: [multiple penalty methods]
)

== Augmented Lagrange Method
#figure(
  image("assets/image-7.png"),
  caption: [Adaptation of penalty method for equality constraints
  $ p_"Lagrangian" (x) def 1/2 rho sum_i (h_i (x))^2 - sum_i lambda h_i (x) $]
)

== Interior Point Methods
Also called *barrier methods*, interior point methods ensure that each step is feasible

This allows premature termination to return a nearly optimal, feasible point

Barrier functions are implemented similar to penalties but must meet the following conditions:
+ Continuous
+ Non-negative
+ Approach infinity as x approaches boundary

Methods
- Inverse Barrier:
  $ p_"barrier" (x) = - sum_i 1/(g_i (x)) $
- Log Barrier
  $ p_"barrier" (x) = - sum_i cases(log(-g_i (x)) quad & g_i (x)<= -1,
                                          0 &"otherwise"
   ) $
