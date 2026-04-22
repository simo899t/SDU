#import "../../../../temp/temp.typ": *
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
$ cal(L)(x,mu>=0,lambda) = c^T x - mu^T x - lambda^T (A x - b) $
The necessary conditions for optimality (KKT) are also sufficient for linear programs




= Duality

= Primal-Dual Hybrid Gradient (PDHG) Method for LP