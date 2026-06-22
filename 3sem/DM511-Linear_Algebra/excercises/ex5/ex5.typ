#set document(title: "Homework 1", author: "Simon Holm")
#set page(
  paper: "us-letter",
  margin: (left: 3cm, right: 3cm, top: 2cm, bottom: 2cm)
)
#set text(font: "New Computer Modern", size: 11pt)
#set math.equation(numbering: "(1)")
#set heading(numbering: "1.")

// Set matrix delimiters to square brackets globally
#set math.mat(delim: "[")

// Title page
#align(center)[
  #text(size: 18pt, weight: "bold")[Exercises 5]
  
  #v(1em)
  
  Simon Holm\
  AI503: Linear Algebra\
  Teacher: Vaidotas Characiejus
]

#pagebreak()

// Table of contents
#outline(depth: 1, indent: auto)

#pagebreak()

= Problem 1
Find the coordinate vector of $v = (2, -1, 3)$ relative to the basis $S = {v_1, v_2, v_3}$
for $RR^3$, where
$ v_1 = (1,0,0), quad v_2 = (2,2,0), quad v_3 = (3,3,3) $

== Solution
We need to find scalars $x$, $y$, and $z$ such that:
$ v = x v_1 + y v_2 + z v_3 $
$ (2, -1, 3) = x(1,0,0) + y(2,2,0) + z(3,3,3) $

This gives us the system of linear equations:
$ cases(
  x + 2y + 3z &= 2,
  2y + 3z &= -1,
  3z &= 3
) $

Solving by back-substitution:
$ cases(
  z &= 1,
  y &= (-1 - 3z)/2 = (-1 - 3)/2 = -2,
  x &= 2 - 2y - 3z = 2 - 2(-2) - 3(1) = 3
) $

Therefore, the coordinate vector is $(3, -2, 1)$.



= Problem 5
Determine whether each statement is true or false:

(a) If $B$ is a basis of a subspace $S$ of $RR^n$, then $P_(B arrow B) = I$.

(b) Every basis of $RR^3$ contains exactly $3$ vectors.

(c) Every set of $3$ vectors in $RR^3$ forms a basis of $RR^3$.

(d) If $S = "span"(v_1, ..., v_k)$, then ${v_1, ..., v_k}$ is a basis of $S$.

(e) The rank of the zero matrix is $0$.

(f) The $n times n$ identity matrix has rank $n$.

(g) If $A in RR^(3 times 7)$, then $"rank"(A) <= 3$.

(h) If $A$ and $B$ are matrices of the same size, then $"rank"(A + B) = "rank"(A) + "rank"(B)$.

(i) If $A$ and $B$ are square matrices of the same size, then $"rank"(A B) = "rank"(A) dot "rank"(B)$.

(j) The rank of a matrix equals the number of non-zero rows that it has.

(k) If $A, B in RR^(m times n)$ are row equivalent, then $"null"(A) = "null"(B)$.

#pagebreak()
== Solution

(a) *True.* The change of basis matrix from a basis to itself is the identity matrix $I$.
$ cases(
  u_1 &= 1 dot u_1 + 0 dot u_2,
  u_2 &= 0 dot u_1 + 1 dot u_2
) => P_(B arrow B) = mat(1,0;0,1) = I $

(b) *True.* This follows from the definition of basis. Since $dim(RR^3) = 3$, every basis of $RR^3$ must contain exactly $3$ linearly independent vectors.

(c) *False.* The vectors must be linearly independent. For example, ${(1,0,0), (2,0,0), (0,1,0)}$ contains $3$ vectors but they are not linearly independent.

(d) *False.* The spanning set ${v_1, ..., v_k}$ may contain linearly dependent vectors. A basis requires both spanning and linear independence.

(e) *True.* The zero matrix has no non-zero rows, so $"rank"(0) = 0$.

(f) *True.* The $n times n$ identity matrix has $n$ linearly independent rows, so $"rank"(I_n) = n$.

(g) *True.* By definition, the rank of a matrix cannot exceed the number of rows, so $"rank"(A) <= 3$.

(h) *False.* In general, $"rank"(A + B) <= "rank"(A) + "rank"(B)$. Equality does not always hold.

(i) *False.* For matrix multiplication, $"rank"(A B) <= min("rank"(A), "rank"(B))$, not the product.

(j) *False.* The rank equals the number of linearly independent rows, not just non-zero rows.

(k) *True.* Row equivalent matrices have the same null space since row operations preserve the solution set of $A bold(x) = bold(0)$.

= Problem 6
For each of the following matrices $A$, find bases for each of $"range"(A)$, $"null"(A)$, $"range"(A^T)$, and $"null"(A^T)$.

$ (a): A = mat(1,0;0,1), quad (b): B = mat(1,2,1;0,1,2), quad (c): C = mat(0,0,0;0,1,2;0,0,0), quad (d): D = mat(1,0;0,2;0,1) $

== Solution

(a) For $A = mat(1,0;0,1)$ (the identity matrix):
- $"range"(A) = "span"{vec(1,0), vec(0,1)} = RR^2$
- $"null"(A) = {vec(0,0)}$ (only the zero vector)
- $"range"(A^T) = "range"(A) = RR^2$
- $"null"(A^T) = "null"(A) = {vec(0,0)}$

(b) For $B = mat(1,2,1;0,1,2)$:
$"range"(B) = "span"{vec(1,0), vec(2,1), vec(1,2)} = "span"{vec(1,0), vec(2,1)}$

These are linearly independent, therefore they span the whole of $RR^2$.
- $"range"(B) = RR^2$
- $"null"(B)$: Solve $B vec(x,y,z) = vec(0,0)$
  $ cases(
    x + 2y + z &= 0,
    y + 2z &= 0
  ) $
  From the second equation: $y = -2z$
  From the first: $x = -2y - z = -2(-2z) - z = 4z - z = 3z$
  So $"null"(B) = "span"{vec(3,-2,1)}$

(c) For $C = mat(0,0,0;0,1,2;0,0,0)$:
- $"range"(C) = "span"{vec(0,1,0)}$
- $"null"(C) = "span"{vec(1,0,0), vec(0,-2,1)}$

(d) For $D = mat(1,0;0,2;0,1)$:
- $"range"(D) = "span"{vec(1,0,0), vec(0,2,1)} = RR^2$
- $"null"(D) = {vec(0,0)}$

#pagebreak()
= Problem 7
Compute the rank and nullity of the following matrices.

$ (a) A = mat(1,-1;-1,1), quad (b) B = mat(6,2;3,1), quad (c) C = mat(1,2;3,5), quad (d) D = mat(1,0;0,2;3,1) $

== Solution
Note that $"rank"(A) + "nullity"(A) = "number of columns"$

(a) For $A = mat(1,-1;-1,1)$:
$ "rank"(A) = "rank"(mat(1,-1;-1,1) arrow.r^(R_2 + R_1) mat(1,-1;0,0)) = 1 $
$ "nullity"(A) = 2 - 1 = 1 $

(b) For $B = mat(6,2;3,1)$:
$ "rank"(B) = "rank"(mat(6,2;3,1) arrow.r^(R_2 - 1/2 R_1) mat(6,2;0,0)) = 1 $
$ "nullity"(B) = 2 - 1 = 1 $

(c) For $C = mat(1,2;3,5)$:
$ "rank"(C) = "rank"(mat(1,2;3,5) arrow.r^(R_2 - 3R_1) mat(1,2;0,-1) arrow.r^(-R_2) mat(1,2;0,1)) = 2 $
$ "nullity"(C) = 2 - 2 = 0 $

(d) For $D = mat(1,0;0,2;3,1)$:
$ "rank"(D) = "rank"(mat(1,0;0,2;3,1) arrow.r^(R_3 - 3R_1) mat(1,0;0,2;0,1) arrow.r^(R_3 - 1/2 R_2) mat(1,0;0,2;0,0)) = 2 $
$ "nullity"(D) = 2 - 2 = 0 $