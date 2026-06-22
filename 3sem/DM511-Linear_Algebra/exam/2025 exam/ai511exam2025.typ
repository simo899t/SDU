#set document(title: "Homework 1", author: "Simon Holm")
#set page(
  paper: "us-letter",
  margin: (left: 3cm, right: 3cm, top: 2cm, bottom: 2cm)
)
#set text(font: "New Computer Modern", size: 11pt)
// #set math.equation(numbering: "(1)")
// #set heading(numbering: "1.")

// Set matrix delimiters to square brackets globally
#set math.mat(delim: "[")

#let code(content) = block(
  fill: gradient.linear(
    rgb("#232528"), 
    rgb("#242127"), 
    angle: 45deg
  ),
  stroke: (
    left: 3pt + rgb("#151515"),
    rest: 0.5pt + rgb("#151515")
  ),
  inset: (left: 14pt, right: 14pt, top: 12pt, bottom: 12pt),
  radius: 6pt,
  [
    #text(
      fill: rgb("#e8eaed"), 
      font: "Monaco", 
      size: 9.5pt,
      weight: "medium"
    )[#content]
  ]
)

#let theorem(title: "Theorem", content) = block(
  fill: gradient.linear(
    rgb("#fafbfc"), 
    rgb("#f1f3f4"), 
    angle: 135deg
  ),
  stroke: (
    left: 3pt + rgb("#2c5aa0"),
    rest: 0.5pt + rgb("#e1e5e9")
  ),
  inset: (left: 18pt, right: 14pt, top: 14pt, bottom: 14pt),
  radius: 8pt,
  [
    #text(weight: "bold", fill: rgb("#1a365d"), size: 12.5pt)[#title]
    #v(0.5em)
    #text(fill: rgb("#2d3748"), size: 10.5pt)[#content]
  ]
)

#let definition(title: "Definition", content) = block(
  fill: gradient.linear(
    rgb("#fffef7"), 
    rgb("#fef9e7"), 
    angle: 135deg
  ),
  stroke: (
    left: 3pt + rgb("#d69e2e"),
    rest: 0.5pt + rgb("#f7d794")
  ),
  inset: (left: 18pt, right: 14pt, top: 14pt, bottom: 14pt),
  radius: 8pt,
  [
    #text(weight: "bold", fill: rgb("#744210"), size: 12.5pt)[#title]
    #v(0.5em)
    #text(fill: rgb("#553c0f"), size: 10.5pt)[#content]
  ]
)

// Title page
#align(center)[
  #text(size: 16pt, weight: "bold")[AI511/MM505 Linear Algebra with Applications
Take-Home Exam
Autumn 2025]
  
  #v(1em)
  
  Simon Holm\
  AI503: Linear Algebra\
  Teacher: Vaidotas Characiejus\
  November 5, 2025
]

#pagebreak()

// Table of contents
#outline(depth: 1, indent: auto)

#pagebreak()

= Problem 1
Consider the following system of linear equations
$ cases(5x+2y-3z=9,
        6x+3y-3z=12,
        4x+2y-2z=8) $
#enum(numbering: "a)")[
  (2 pts) Is $(x, y, z) = (1, 2, 0)$ a solution of the system?
][
  (2 pts) Give the expression of the augmented matrix corresponding to the system.
][
  (10 pts) Transform the augmented matrix into a reduced row echelon form to find the set of solutions of the system.
]
== Answer
#enum(numbering: "a)")[
    $ (1,2,0)=cases(5+2dot 2=9,
        6+3 dot 2=12,
        4+2dot 2=8) $

    This is *true*
    #v(1em)
][
    $ (x,y,z)=mat(5,2,-3,9;
          6,3,-3,12;
          4,2,-2,8; augment: #3) $
  #v(1em)
][
    Now do row operations to transform this matrix intro RREF
    $ mat(5,2,-3,9;
          6,3,-3,12;
          4,2,-2,8; augment: #3) ==>^(R_1-R_3) mat(1,0,-1,1;
                                                   6,3,-3,12;
                                                   4,2,-2,8; augment: #3) $
    $ mat(1,0,-1,1;
          6,3,-3,12;
          4,2,-2,8; augment: #3) ==>^(R_2-2/3R_3) mat(1,0,-1,1;
                                                   0,0,0,0;
                                                   4,2,-2,8; augment: #3) $
    $ mat(1,0,-1,1;
          0,0,0,0;
          4,2,-2,8; augment: #3) ==>^(R_3 - 4R_1) mat(1,0,-1,1;
                                                   0,0,0,0;
                                                   0,2,-2,4; augment: #3) $
    $ mat(1,0,-1,1;
          0,0,0,0;
          0,2,2,4; augment: #3) ==>^(R_3 <=> R_1) mat(1,0,-1,1;
                                                   0,2,2,4;
                                                   0,0,0,0; augment: #3) $
    $ mat(1,0,-1,1;
          0,2,2,4;
          0,0,0,0; augment: #3) ==>^(1/2R_2) mat(1,0,-1,1;
                                                   0,1,1,2;
                                                   0,0,0,0; augment: #3) $
    Since this is not an identity matrix there are an infinite amount of solutions
    $ cases(x-z = 1,
            y+z = 2) = cases(x=1+z,
                             y=2-z) $
    Therefore:  ${(1+z,2-z,z):z in RR}$
]

= Problem 2
Suppose that
$ A = mat(3,8,1,7;
          2,9,10,2;
          1,1,8,8;
          2,2,5,10) quad "and" quad B=  mat(7,2,4,8;
                                            3,4,2,4;
                                            6,10,3,6;
                                            10,9,2,4) $
Calculate the determinant of $C$, where $C= A B$
== Answer
First calculate $A B$
$ A B = mat(121,111,45,90;
            121,158,60,120;
            138,158,46,92;
            150,152,47,94) $
Since $"col"_4 = 2dot"col"_3$ the coloums are linearly dependent on eachother, because of this one dimension collapses and therefore $det(A B) = 0$


= Problem 3
Consider the subsets of $RR^3$
$ A = {(8,7,6)},quad B= {(4,9,8), (-3,5,7)} $
and
$ C = {(3,7,1), (8,10,9), (-4,8,-14)} $

#enum(numbering: "a)")[
  (10 pts) Determine which of the subsets A, B, and C are bases of some subspace of $RR^3$ and which are not bases of any subspace of $RR^3$.
][
  (4 pts) Determine the dimensions of the subspaces that A, B, and C span.
]
== Answer
#enum(numbering: "a)")[
    To determine whether a set is a basis of some subspace, it needs to contain only linearly independent vectors.
    
    === Set A = {(8,7,6)}: 
    
    Has only 1 non-zero vector, so it has to be linearly independent. 
    
    Therefore, *A is a basis of some subspace*.
    
    === Set B = {(4,9,8), (-3,5,7)}: 
    
    If B is a basis, then $(4,9,8) != k(-3,5,7)$, then $4 != -3k$, $9 != 5k$, $8 != 7k$. From the first equation, $k = -4/3$. But $9 eq 5(-4/3) = -20/3$, so they're not scalar multiples. 
    
    Therefore, *B is a basis of some subspace*
    
    === Set C = {(3,7,1), (8,10,9), (-4,8,-14)}: 
    #code[
```py
import numpy as np
mat = np.array([
    [3, 7, 1],
    [8, 10, 9],
    [-4, 8, -14]
])

print('Matrix:')
print(mat)

# Calculate determinant with numpy
det_mat = np.linalg.det(mat)
print(f'Determinant: {det_mat}')

[OUTPUT]
Matrix:
[[  3   7   1]
 [  8  10   9]
 [ -4   8 -14]]
Determinant: 0.0
```
    ]
    
    Since $det(C) = 0$, the vectors are linearly dependent (at least one vector can be written as a linear combination at least one other).  Therefore, *C is not a basis of any subspace*.
    #v(1em)
][
    The dimension of span equals the number of linearly independent vectors:
    
    *dim(span(A)) = 1* 
    $ "span"(A) = {c_1 vec(8,7,6): c_1 in RR} $
    (A has 1 linearly independent vector)
    
    *dim(span(B)) = 2* 
    $ "span"(B) = {c_1 vec(4,9,8) + c_2 vec(-3,5,7): c_1, c_2 in RR} $
    (B has 2 linearly independent vectors)

    #v(11em)

    *dim(span(C))  = 2*
    
    Since C is not a basis (the vectors are linearly dependent), we need to find the rank of C to determine how many linearly independent vectors it actually contains. We row reduce the matrix formed by the vectors:
    $ mat(3,7,1;8,10,9;-4,8,-14) ==>^(R_2 - 8/3 R_1) 
    mat(3,7,1;0,-26/3,19/3;-4,8,-14) $
    since $8-3x=0 <==> x = 8/3$
    $ ==>^(R_3 + 4/3 R_1) mat(3,7,1;0,-26/3,19/3;0,52/3,-38/3) $
    since $-4+3x=0 <==> x = 4/3$
    $ ==>^(-3/26 R_2) mat(3,7,1;0,1,-19/26;0,52/3,-38/3) $
    since $(-26/3) dot (-3/26) = 1$
    $ ==>^(R_3 - 52/3 R_2) mat(3,7,1;0,1,-19/26;0,0,0) $
    since $(52/3) - (52/3) dot 1 = 0$ and $(-38/3) - (52/3) dot (-19/26) = 0$
    
    Since we have 2 non-zero rows after row reduction, rank(C) = 2.
    
    Therefore, dim(span(C)) = rank(C) = 2.
    
    $ "span"(C) = {c_1 vec(3,7,1) + c_2 vec(8,10,9) + c_3 vec(-4,8,-14): c_1, c_2, c_3 in RR} $
    
    (C has exactly 2 linearly independent vectors among the 3 given)
]

#pagebreak()
= Problem 4
Consider the matrix
$ A= 1/2mat(1,1;1,1) $

#enum(numbering: "a)")[
  (2 pts) Show that $ A = (w w^T)/(w^T w), $ where $w = (1, 1)$.
][
  (8 pts) Explain how $RR^2$ is transformed by the matrix transformation $T_A : RR^2 -> RR^2$ given by $T_A (x) = A x$ for $x in RR^2$.
][
  (8 pts) The eigenvalues of $A$ are $lambda_1 = 1$ and $lambda_2 = 0$ with their corresponding eigenvectors $2^(-1\/2)(1, 1)$ and $2^(-1\/2)(-1, 1)$ (they are given - you do not need to compute them). Let $E_lambda$ denote the eigenspace corresponding to the eigenvalue $lambda$. Describe how the matrix $A$ transforms non-zero vectors in $E_(lambda_1)$ and in $E_(lambda_2)$. Make a connection with your response in part (b).
]

== Answer
#enum(numbering: "a)")[
    Given $w = mat(1;1)$, we need to show that $A = (w w^T)/(w^T w)$.
    
    First, $w^T = mat(1,1)$ and $w^T w = mat(1,1) mat(1;1) = 1 + 1 = 2$.
    
    Then, $ w w^T = mat(1;1) mat(1,1) = mat(1 dot 1, 1 dot 1; 1 dot 1, 1 dot 1) = mat(1,1;1,1) $
    
    Therefore:
    $ A = (w w^T)/(w^T w) = (mat(1,1;1,1))/2 = 1/2 mat(1,1;1,1) $
    
    This matches the given matrix $A$.
    #v(1em)
][
    The matrix $A = 1/2 mat(1,1;1,1)$ is a projection matrix that projects vectors onto the line $y = x$ in $RR^2$.
    
    For any vector $vec(x,y) in RR^2$:
    $ T_A (mat(x;y)) = 1/2 mat(1,1;1,1) mat(x;y) = 1/2 mat(x+y;x+y) = ((x+y)/2) mat(1;1) $

    This means the vector $mat(x;y)$ is projected onto the line $y = x$, resulting in the point $ ((x+y)/2, (x+y)/2) $. 
    #v(2em)

][
    The eigenvalues tell us how $A$ transforms vectors in each eigenspace:

    They must satisfy $ A v=lambda v $
    
    *For vectors in $E_(lambda_1)$ (eigenvalue = 1):*
    #v(0.05em)
    - Any vector $v in E_(lambda_1)$ satisfies $A v = 1 dot v = v$
    #v(0.05em)
    - These vectors are *unchanged* by the transformation
    #v(0.05em)
    - $ E_(lambda_1) = "span"{mat(1;1)}$ (the line $y = x$)
    #v(0.05em)
    
    *For vectors in $E_(lambda_2)$ (eigenvalue = 0):*
    #v(0.05em)
    - Any vector $v in E_(lambda_2)$ satisfies $A v = 0 dot v = vec(0,0)$
    #v(0.05em)
    - These vectors are *collapsed to zero*
    #v(0.05em)
    - $E_(lambda_2) = "span"{mat(1;-1)}$ (the line $y = -x$)
    #v(0.05em)
    

  *What does this say about the transformation*
  #v(0.05em)
    - Vectors already along $y = x$ stay unchanged (eigenvalue 1)
    #v(0.05em)
    - Vectors along $y = -x$ get annihilated (eigenvalue 0)
    #v(0.05em)
    - All other vectors get projected onto the line $y = x$
]
      

= Problem 5
Let $A in RR^(n times n)$ be symmetric and define the function $f : RR^n \\ {0} -> RR$ by setting
$ f(x) = (x^T A x)/(x^T x) $
for nonzero $x in RR^n$.

#enum(numbering: "a)")[
  (2 pts) What does the assumption that $A in RR^(n times n)$ is symmetric tell us about its eigenvalues?
][
  (8 pts) Suppose that $v$ is an eigenvector of $A$ with its corresponding eigenvalue $lambda$. Show that $f(v) = lambda$, i.e., the value of $f$ at an eigenvector is the corresponding eigenvalue.
][
  (2 pts) Suppose that $X in RR^(n times p)$ with $n >= p$. What is the size of the matrix $X^T X$? Is the matrix $X^T X$ symmetric?
][
  (12 pts) Show that the eigenvalues of $X^T X$ are non-negative.
][

  (12 pts) Suppose additionally that $X in RR^(n times p)$ is of full rank. Show that the eigenvalues of $X^T X$ are positive.
]

#pagebreak()
== Answer
#enum(numbering: "a)")[
#theorem(title: "A special case of Theorem 3.3.2 of Johnston (2021)")[
  If $A in RR^(n times n)$ is symmetric, then all of its eigenvalues are real.
]
By theorem, if a  is symmentric, all of its eigenvalues must be real.
#v(1em)
][
If $A v = lambda v$ (since v is an eigenvector)

then
$ f(v) = (v^T A v)/(v^T v) = (v^T lambda v)/(v^T v) = lambda (v^T v)/(v^T v) = 
lambda $

The value of $f$ at an eigenvenctor must be the corresponding eigenvalue
#v(1em)
][
For a matrix $X in RR^(n times p)$, then $X^T X in RR^(p times n)RR^(p times n)$

for this
$ (p times n)dot (n times p) = p times p $

So.. $ X^T X in RR^(p times p) $

To check if $X^T X$ is symmetric we use the following theorem:
#theorem(title: "Theorem 1.3.4 of Johnston (2021)")[
  Let A and B be matrices with sizes such that the operations below
make sense and let c ∈ R be a scalar. Then
  
  (c) $(A B)^T = B^T A^T$
]

$ (X^T X)^T = X^T (X^T)^T = X^T X $

Therefore $X^T X$ is symmetric.
#v(33em)
][
Let $lambda$ be an eigenvalue of $X^T X$ with corresponding eigenvector $v != 0$.

Then $X^T X v = lambda v$.

Taking the dot product of both sides with $v$ will make isolating $lambda$ much easier:
$ v^T (X^T X v) = v^T (lambda v) $
$ v^T X^T X v = lambda v^T v $

Since $v^T v > 0$ (because $v != 0$), we can divide both sides by $v^T v$:
$ lambda = (v^T X^T X v)/(v^T v) $

Now, let $w = X v$. Then:
$ v^T X^T X v = v^T X^T (X v) = (X v)^T (X v) = w^T w = ||w||^2 >= 0 $

We know this from:
#definition(title: "Definition 1.2.2: Length of a Vector")[
  The length of a vector $v = (v_1, v_2, . . . , v_n)in RR^n$, denoted by $norm(v)$, is the quantity
  $ norm(v) = sqrt(v dot v) = sqrt(v_1^2+v_2^2+...+v_n^2) $
]

Now we know that $v^T v > 0$ and $v^T X^T X v >= 0$, so..
$ lambda = (v^T X^T X v)/(v^T v) >= 0 $

Therefore, all eigenvalues of $X^T X$ has to be non-negative.
#v(1em)
][
  For the matrix $A in RR^(n times p)$ to be in full rank, $A$ must be linearly indenpentent $ det(A)!=0 $
  
  By contradriction
  
  Assume that one of A's eigenvalues $lambda=0$
  
  Then $ det(A) =product_(i=1)^n lambda_(i) = 0 quad (bot)  $

  By this, $lambda$ cannot be 0

  And then
  $ lambda = (v^T X^T X v)/(v^T v) > 0  $
  Therefore, the eigenvalues of a full rank matrix $A$ must be positve
]
#pagebreak()
= Problem 6
You are only required to respond true or false to the following statements.

#enum(numbering: "a)")[
  (2 pts) A product of an invertible matrix and a non-invertible matrix is an invertible matrix.
][
  (2 pts) Every subspace of $RR^n$ has infinitely many vectors.
][
  (2 pts) If $A in RR^(3 times 3)$ with $det(A) = 3$, then $"rank"(A) = 3$.
][
  (2 pts) $0$ is an eigenvalue of every square matrix.
][
  (2 pts) Every diagonalisable matrix is invertible.
]

== Answer
#enum(numbering: "a)")[
    *This is false.*
    
    This can easily be proven by $ det(A B) = det(A)det(B) $ 
    

  By this, if A is non-invertable ($det(A)=0$), then $A B$ will also be non-invertable ($det(A B) = 0$)
  #v(1em)
][
  *This is false.*
  
  Since the zero subspace ${0}$ does not contain infinitely many vectors
  #v(1em)
][
  *This is true.*
  
  If $det(A) = 3 != 0$, then $A$ is invertible, which means $A$ has full rank. For a $3 times 3$ matrix, full rank means $"rank"(A) = 3$.
  #v(1em)
][
  *This is false*

  Proof by contradiction.

  Let matrix A be invertible and this be the case
  $ A v = lambda v <==> A v =0 $
  Then $ A^(-1)(A v) =A^(-1)dot 0 ==> I v=0  ==> v = 0 quad (bot) $
  Since $v$ cannot be $0$ (because it is an eigenvector) then $lambda != 0$ for invertible matrices
  #v(15em)
][
  *This is false.*
  
  For any matrix to be diagonalisable it must satisfy:
  $ A = P D P^(-1) $
  where $D$ is diagonal and $P$ is invertible.

  Assume that $A$ is invertible, then $det(A) != 0$.

  Now by contradiction
  $ A = mat(1,0;0,0) $
  
  This matrix is diagonalizable (because $A = I^(-1) A I = A$).
  
  And with 
  
  
  $ det(A - A I) = 

det(mat(1,0; 0,0) - lambda mat(1 0; 0 1)) $$ det(mat(1-λ,0; 0,-λ])) = (1-λ)(-λ) = -λ(1-λ) = λ(λ-1)
 $
 setting this to 0
 $ lambda(lambda-1)=0 ==> lambda =0, 1 $
  But $det(A) = 1 dot 0 - 0 dot 0 = 0$, so $A$ is not invertible.
]


