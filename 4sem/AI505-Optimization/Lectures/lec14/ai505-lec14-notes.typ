#import "@local/tempst:0.1.0": *
#show: note.with(
  title: "Lecture 14: Linear Constrained Optimization",
  author: "Simon Holm",
  date: "April - 2026"
)

// Your content starts here

= Linear Programming
== Problem Formulation
If an optimization problem has a linear objective and constraints, it is called a *linear programming problem (linear program, LP)*

- The general form is
#align($ min_x c^T x \
s.t quad A x &<= b\
D x &>= e\
F x &= G\
\
x,c &in RR^n,\
A in &RR^(m times n), b in R^m\
D in &RR^(p times n), e in R^p\
F in &RR^(q times n), g in R^q
$)

=== Numerical example
$ #image("assets/image.png") $
$ mat(2,3,-8;4,1,3) dot mat(x_1;x_2;x_3) <=mat(5;9) $
Where $ x_1,x_2,x_3 in RR $

=== General example
$ #image("assets/image-1.png") $
Binary programming works better. So if you had to choose an amount of each item, you should make each item (if you have $10 "phones" = {x_1, dots, x_10}$) a parameter.

== Conversion to linear program
Many problems can be converted into linear programs that have the same solution.
$ #image("assets/image-2.png") $

== Forms
Two canonical ways to write an LP. Start from the general form and convert to one of these to hand it to an algorithm.

=== Standard form
All constraints are inequalities, variables non-negative:
#align($ min_x c^T x \
s.t quad A x &<= b\
x &>= 0
$)

Conversion rules:
- A $>=$ constraint is flipped by multiplying by $-1$
- An equality $F x = g$ becomes two inequalities: $F x <= g$ and $-F x <= -g$
- A free variable $x_i in RR$ is split: $x_i = x_i^(+) - x_i^(-)$ with $x_i^(+), x_i^(-) >= 0$

Nice for geometric intuition — the feasible set is a polytope defined by half-spaces.

=== Equality form
Also called _standard equality form_ or _slack form_. The *main constraints* are equalities; $x >= 0$ is a separate sign restriction (non-negativity bound), not counted among the "constraints" in this naming:
#align($ min_x c^T x \
s.t quad A x &= b quad &#text[(main constraints — equalities)]\
x &>= 0 quad &#text[(sign restriction)]
$)

Conversion rules:
- $A x <= b$ becomes $A x + s = b$ with slack $s >= 0$
- $A x >= b$ becomes $A x - s = b$ with surplus $s >= 0$
- Free variables split the same way as above

This is the form the *simplex algorithm* operates on — it needs equalities so it can pick a basis $A_B$ (an $m times m$ invertible submatrix) and solve $x_B = (A_B)^(-1) b$ at each vertex. That is why the dimension grows to $2n + m$: $n$ originals $+ n$ for splitting free vars $+ m$ slacks.

=== Quick example
Original problem:
$ min quad x_1 + x_2 quad s.t. quad 2 x_1 + x_2 <= 4, quad x_1, x_2 >= 0 $

Equality form — add slack $s >= 0$:
$ min quad x_1 + x_2 quad s.t. quad 2 x_1 + x_2 + s = 4, quad x_1, x_2, s >= 0 $

= Simplex Algorithm
Different kinds of problems may lead to different kinds of solutions
$ #image("assets/image-3.png") $
#pagebreak()

== Problem Formulation
Linear programs are often solved in *equality form*

#align($ min_x c^T x \
s.t quad A x &= b\
x &>= 0\
\
x,c &in RR^(2n+m),\
A in RR^(2n+m)\
b in &RR^(m)
$)
#figure(
  image("assets/image-4.png"),
  caption: [Simplex feasible set]
)

== Info abt the simplex algorithm
- Guaranteed to solve any feasible and bounded linear program
- Works on the equality form
- Assumes that rows of A are linearly independent and $m ≤ n′ (n′ ≤ 2n + m)$
- The feasible set of a linear program forms a polytope. (faces of $n-1$ dimention)

- The simplex algorithm moves between vertices of the polytope until it finds an *optimal vertex*

#figure(
  image("assets/image-5.png"),
  caption: [Fundamental Theorem of Linear Programming]
)

We can find these vertices using a system of linear equations.
$ A x = A_B x_B = b qquad bi x_B = (A_B)^-1 b $
for each vertex. Though this is not really dont in practice (very costly).

In practice for this is done by LU-Decomposition #link("https://www.geeksforgeeks.org/engineering-mathematics/l-u-decomposition-system-linear-equations/")[[LINK]] with $ cal(O)(3m) $

Where $m$ is the number of constraints

== The Simplex Algorithm
- every vertex has an associated partition $(B,N)$
- not every partition corresponds to a vertex. $A_B$ might be not invertible or the point $x_B$ might not be $>= 0$.
- identifying partitions that correspond to vertices corresponds to solving an LP problem as well!

The algorithm consists of two phases
1. *Initialization Phase:* finding a feasible starting vertex
2. *Optimization Phase:* finding the optimal vertex

=== The problem setup

The LP
#align($ min_x c^T x \
s.t quad A x &= b\
x &>= 0\
\
x,c in &RR^(2n+m),\
A in &RR^(2n+m)\
b in &RR^(m)
$)

Since $n>m$, A vertex is obtained by: $x_N=0$
- Picking $n−m=2$ variables to set to zero (the non-basis $N$)
- Solving for the remaining $m=2$ variables (the basis $B$)

$ A x = A_B x_B + A_N x_N = b $
Since $x_B = 0$ $ A x = A_B x_B = b quad ==> quad x_B = A_B^(-1) b $


Lagrangian function:
$ cal(L)(x,mu>=0,lambda) = c^top x - mu^top x - lambda^top (A x - b) $
The necessary conditions for optimality (KKT) are also sufficient for linear programs

- feasibility $ A x = b, x >= 0 $
- dual feasibility $ mu >= 0 $
- complementary slackness $ mu dot x = 0 $
- stationarity $ A^top lambda + mu = 0 $




= Duality
Linear programs have a simple dual form:

#figure(grid(columns: 2,column-gutter: 6em,
align($ min_x c^top x \ st A x &= b \x>= 0 $), align($ max_lambda b^top lambda \ st quad A^top lambda <= c $))
)

Let solution to the primal be $p^*$ and solution to the dual be $d^*$. Then, $p^* = d^*$ guarantees to be the unique optimal value because the duality gap is zero.

This is due to:
#align($
min_x max_lambda c^top x + lambda^top (A x - b)\
st A x = b
$)

For the case of $A x >= b$ then we search for $lambda <= 0$

#theorem(title: [Theorem (Strong Duality Theorem)], [
  Given:
  $ (P) qquad min{c^top x | A x = b, x >= 0} $
  $ (D) qquad max{b^top lambda | A^top lambda <= c}. $
  
  Exactly one of the following occurs:
  1. Both (P) and (D) are feasible, and $p^* = d^*$.
  2. (P) is unbounded below and (D) is infeasible.
  3. (P) is infeasible and (D) is unbounded above.
  4. (P) has feasible solution, then let an optimal be: $x^* = [x_1^*, dots, x_n^*]$. Then, (D) is feasible and has an optimal solution $lambda^*$ such that $lambda^* = [lambda_1^*, dots, lambda_m^*]$.
  Then $ p^* = c^top x^* = b^top lambda^* = d^* $
])

#pagebreak()

= Primal-Dual Hybrid Gradient (PDHG) Method for LP

== First-order methods for LP


For the standard form of the linear program
#align($ min_x c^top x \ st A x = b, x >= 0 $)
we can write the Lagrangian as
$ cal(L)(x, mu >= 0) = c^top x + mu^top (A x - b) $

#figure(
  image("assets/image-1.png", width: 20em),
  caption: [Lagrangian function for LP]
)

Then we can do gradient ascent on the dual variables $mu$ and gradient descent on the primal variables $x$:
#align($
x^(k+1) = "Projection"_(x>=0) (x^k - alpha_k (c + A^top y^k) = (x^k - alpha (c-A^top y^k))^+\
y^(k+1) = y^k + alpha_k (b- A x^k)
$)

Gradient Descent Ascent may not converge to the saddle point. 

== Example:

$ max_x min_y x y $

we have closed form of each iteration:

$ mat(x_k;y_k) = mat(1,aleph;-alpha,1)^k dot mat(x^0;y^0) $

The determinant of $mat(1,aleph;-alpha,1)$ is $1+alpha^2 < 1$ no matter how small α is, which means this approach never converges.

We can fix this issue by updating $y^(k+1)$

For the original LP problem, usually we use the following metrics for termination:
- Primal feasibility: $||A x - b|| <= epsilon$
- Dual feasibility: $||c- A^top lambda|| <= epsilon$
- Duality gap: $|c^top x - b^top lambda| <= epsilon$

Use `method = "heights"` from `scipy.minimize.linprog`