
#let title = "Lecture 7: Second-Order Methods"
#let author = "Simon Holm"
#let date = "March - 2026"

#import "../../../../../../temp.typ": *

#note(
  title: title,
  author: author,
  date: date
)

// Your content starts here

= Derivative-Free Methods
- Also called direct search methods, zero-order, black box, pattern search
- Direct method search using function evaluations only

= Cyclic Coordinate Search

= Powell's Method

= Hooke-Jeeves
evaluates $f(x)$ anf $f(x plus.minus alpha e_i)$ for a given $alpha$ in every coordinate direction from an anchoring point $x$
- It accepts any improvement it may find.
- If no improvements are found, it decreases the step size.
- The process repeats until the step size is sufficiently small.
- 2n evaluations for an n-dimensional problem

#figure(
  image("assets/image-47.png"),
  caption: []
)
= Generalized Pattern Search
Generalization of Hooke-Jeeves method

It seaches for the best directions.

Construct a pattern $P$ from set of directions $cal(D)$ of size $m$

Note that you need $m+1$ directions to span the space

$ P={x+alpha d "for each" d in cal(D)} $

#figure(
  image("assets/image-45.png"),
  caption: [Vizualtion that we need 3 directions to span the space $RR^2$]
)

#figure(
  image("assets/image-46.png"),
  caption: [Example of Generalized Pattern Search]
)
= Nelder-Mead Simplex Method
Uses simple algorithm to traverse search space using set of points defining a simplex

#figure(
  image("assets/image-48.png"),
  caption: [Nelder-Mead Simplex Method]
)

#figure(
  image("assets/image-49.png")),
  caption: [Nelder-Mead Simplex Method algorithm]
)

#pseudo[
  *Algorithm* Simplex search (Nelder-Mead)
- Let $x_1 dots x_(n+1)$ be vertices of a simplex
- Let $h: y_h = max_i y_i = max f(x_i) "and" l: y_l = min_i y_i = min f(x_i)$
- Let $bar(x)$ be the centroid of the points with $i!=h$ and $d(x_i,x_j)$ the distance between two points $x_i$ and $x_j$ ($j<-0$)
+ iter. $k: (k<-k+1)$ Generate the *reflextion* $x_R$ of $x_h$
+ Case 1 *if* $y_l <= y_R < y_h$ *then* go to *Reflect*
+ Case 2 *else if* $y_R < y_l$ *then*
  + Generate the *expantion*  $x_E$ of $x_h$
  + *if* $y_E < y_l$ *then* $x_h <- x_E$ and go to *Reflect*
  + *else* $x_h <- x_R$ and go to *Reflect*
+ Case 3 *else if* $y_R > y_i, forall i != h$ *then* $x_h <- min{y_h,y_R}$ and generate the *contraction 1*
  + *if* $y_C <= min{y_h,y_R}$ *then* $x_h <- x_C$ and go to *Reflect*
  + *else* *contraction 2* $x_i <- (x_i + x_l)/2$
]

= DIRECT - Dividet Rectangles

#figure(
  image("assets/image-50.png"),
  caption: []
)

Since we for blackbox problems we might now know anything about $L$
#figure(
  image("assets/image-51.png"),
  caption: []
)

#figure(
  image("assets/image-55.png"),
  caption: []
)

Choose the largest
#figure(
  image("assets/image-52.png"),
  caption: []
)


#figure(
  image("assets/image-53.png"),
  caption: []
)

#figure(
  image("assets/image-54.png"),
  caption: []
)




