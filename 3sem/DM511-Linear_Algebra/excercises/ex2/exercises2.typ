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
  #text(size: 18pt, weight: "bold")[Exercises 2]
  
  #v(1em)
  
  Simon Holm\
  AI503: Linear Algebra\
  Teacher: Vaidotas Characiejus
]

#pagebreak()

// Table of contents
#outline(indent: auto)

#pagebreak()

= Problem 1

Write down the elementary matrix $E in bb(R)^(3 times 3)$ corresponding to each of the following row operations.

+ $R_1 arrow.l.r R_3$
+ $R_3 - 4R_1$ 
+ $3R_1$
+ $R_2 + 3R_1$

== Solution

+ $R_1 arrow.l.r R_3$
  $ mat(0, 0, 1; 0, 1, 0; 1, 0, 0) $

+ $R_3 - 4R_1$ 
  $ mat(1, 0, 0; 0, 1, 0; -4, 0, 1) $

+ $3R_1$
  $ mat(3, 0, 0; 0, 1, 0; 0, 0, 1) $

+ $R_2 + 3R_1$
  $ mat(1, 0, 0; 3, 1, 0; 0, 0, 1) $

= Problem 2

For both of the following matrices $A in bb(R)^(m times n)$, find a matrix $E in bb(R)^(m times m)$ such that $E A$ equals the reduced row echelon form of $A$.

+ $A = mat(1, 2; 3, 4)$

+ $A = mat(0, -1, 3; 0, 2, 1)$

== Solution

To bring these matrices to their reduced row echelon forms, we need to find the appropriate elementary matrices.
We do this by $A dot A^(-1) = I_n$

+ For $A = mat(1, 2; 3, 4)$
  
  First we find the inverse of A:
  $ A^(-1) = mat(4, -2; -3, 1) dot det(A) = A^(-1) = mat(4, -2; -3, 1) dot 1/(1 dot 4 - 2 dot 3) = mat(-2, 1; 1.5, -0.5) $

  Now we can find E ($I_n$) by multiplying $A$ and $A^(-1)$:
  $ E = A^(-1) dot A = mat(-2, 1; 1.5, -0.5) dot mat(1, 2; 3, 4) = mat(1, 0; 0, 1) $

+ For $A = mat(0, -1, 3; 0, 2, 1)$
  
  Let's use row operations to bring A to its reduced row echelon form:
  $ mat(0, -1, 3; 0, 2, 1) arrow.r^(R_1 + R_2) mat(0, 1, 4; 0, 2, 1) $

  $ mat(0, 1, 4; 0, 2, 1) arrow.r^(R_2 - 2R_1) mat(0, 1, 4; 0, 0, -7) $

  $ mat(0, 1, 4; 0, 0, -7) arrow.r^(-1/7 dot R_2) mat(0, 1, 4; 0, 0, 1) $

  $ mat(0, 1, 4; 0, 0, 1) arrow.r^(R_1 - 4R_2) mat(0, 1, 0; 0, 0, 1) $

= Problem 3

Assume that $a d - b c eq.not 0$ and derive the formula for the inverse of a $2 times 2$ matrix

Let $A = mat(a, b; c, d)$

== Solution

We know that $A dot A^(-1) = I_n$, where $I_n$ is the identity matrix.
$ mat(a, b; c, d) dot mat(x_1, x_2; x_3, x_4) = mat(1, 0; 0, 1) $

This gives us the following system of equations:
$ a x_1 + b x_3 &= 1 \
  a x_2 + b x_4 &= 0 \
  c x_1 + d x_3 &= 0 \
  c x_2 + d x_4 &= 1 $

We can solve this system using substitution or elimination. Let's use elimination:
for each $x_i$ we can find the following:
$ x_1 &= d/(a d - b c) \
  x_2 &= (-b)/(a d - b c) \
  x_3 &= (-c)/(a d - b c) \
  x_4 &= a/(a d - b c) $

Thus, the inverse of the matrix $A$ is given by:
$ A^(-1) = mat(x_1, x_2; x_3, x_4) = mat(d/(a d - b c), (-b)/(a d - b c); (-c)/(a d - b c), a/(a d - b c)) $

This can be simplified to:
$ A^(-1) = 1/(a d - b c) mat(d, -b; -c, a) $

= Problem 4

Use the inversion algorithm to find the inverse of the matrix (if the inverse exists).
$ A = mat(1, 2, 3; 2, 5, 4; 1, 0, 8) quad quad B = mat(-1, 3, -4; 2, 4, 1; -4, 2, -9) $

== Solution

= Problem 5

= Problem 6

A matrix $A in bb(R)^(n times n)$ is called nilpotent if there is some positive integer $k$ such that $A^k = 0$. In this exercise, we will compute some formulas for matrix inverses involving nilpotent matrices.

+ Suppose that $A^2 = 0$. Prove that $I - A$ is invertible, and its inverse is $I + A$.
+ Suppose that $A^3 = 0$. Prove that $I - A$ is invertible, and its inverse is $I + A + A^2$.
+ Suppose that $k$ is a positive integer and $A^k = 0$. Prove that $I - A$ is invertible and find its inverse. \
  _Hint: Use induction to prove that $(I - A)(sum_(j=0)^(k-1) A^j) = I - A^k$ with the convention that $A^0 = I$._

== Solution

+ We want to prove that:
  $ (I - A)(I + A) = I \
    (I + A)(I - A) = I $
  
  We can show that 
  $ = I^2 + I A - I A - A^2 = I \
    = I^2 - A^2 = I $

+ We want to prove that:
  $ (I - A)(I + A + A^2) = I \
    (I + A + A^2)(I - A) = I $
  
  We can show that
  $ (I - A)(I + A + A^2) = I^2 + A + A^2 - A - A^2 - A^3 = I^2 = I \
    - A^3 = I \
    (I + A +- A^3 = I^3 = I $
 A^2)(I - A) = I^2 
+ 

= Problem 7

Suppose $A in bb(R)^(m times n)$ is such that $A^T A$ is invertible, and let $P = A(A^T A)^(-1)A^T$.

- What size is $P$?
- Show that $P^T = P$.
- Show that $P^2 = P$. (Matrices such that $P^T = P^2 = P$ are called projection matrices.)

== Solution

If $A$ is invertible then $A A^(-1) = I_n$ this means that: $P = A(A^T A)^(-1)A^T = I_n$ is a projection matrix.

- What size is $P$?
  - $m times m$
- Show that $P^T = P$.
- Show that $P^2 = P$. (matrices such that $P^T = P^2 = P$ are called projection matrices) 

#pagebreak()

= Problem 10

The Sherman-Morrison formula is a result that says that if $A in bb(R)^(n times n)$ is invertible and $v$, $w in bb(R)^(n times 1)$, then
$ (A + v w^T)^(-1) = A^(-1) - (A^(-1) v w^T A^(-1))/(1 + w^T A^(-1) v) $

As long as the denominator is not zero. Prove that this formula holds by multiplying $A + v w^T$ by its proposed inverse.

== Solution

We want to prove that: 
$ (A + v w^T) (A^(-1) - (A^(-1) v w^T A^(-1))/(1 + w^T A^(-1) v)) = I $

We can rewrite this as:
$ = A A^(-1) - A ((A^(-1) v w^T A^(-1))/(1 + w^T A^(-1) v)) + v w^T A^(-1) - v w^T ((A^(-1) v w^T A^(-1))/(1 + w^T A^(-1) v)) $

and then as:
$ = I - (I v w^T A^(-1))/(1 + w^T A^(-1) v) + (v w^T A^(-1))/1 - v w^T ((w^T A^(-1) v w^T A^(-1))/(1 + w^T A^(-1) v)) $

$ = I - (v w^T A^(-1))/(1 + w^T A^(-1) v) + (v w^T A^(-1))/(1 + w^T A^(-1) v) - v w^T ((A^(-1) v w^T A^(-1))/(1 + w^T A^(-1) v)) $