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
  #text(size: 18pt, weight: "bold")[Exercises 3]
  
  #v(1em)
  
  Simon Holm\
  Course: number: name\
  Teacher: name
]

#pagebreak()

// Table of contents
#outline(indent: auto)

#pagebreak()

= Problem 1
valuate the determinant of the matrix by first reducing the matrix to row echelon
form and then using some combination of row operations and cofactor expansion.

== Solution
$ A = mat(3,-6,9;-2,7,-2;0,1,5) $
$ A = arrow.long.r^(R_2 + 2/(3)R_1)mat(3,-6,9;0,3,4;0,1,5) $
$ A = arrow.long.r^(R_3 - 1/(3)R_2)mat(3,-6,9;0,3,4;0,0,11/3) $


We know that the determinant of a triangular matix is:
$ product_(i=1)^n a_(i i) =3 dot 3 dot 1/3 = 33 $

= Problem 2
Show that the determinant is equal to 0 without directly evaluating the determinant.

== Solution
$ A = mat(-2, 8, 1, 4;3,2,5,1;1,10,6,5;4,-6,4,-3) "and " A^T = mat(-2, 3, 1, 4; 8, 2, 10, -6; 1, 5, 6, 4; 4, 1, 5, -3) $

If $arrow.long.r^(R_2 - 2R_4)$ on $A^T$ then we get a row of 0's

$ A^T arrow.long.r^(R_2 + 2R_4) mat(-2, 3, 1, 4; 0, 0, 0, 0; 1, 5, 6, 4; 4, 1, 5, -3) $

And since we know that a matrice with 1 or more rows of 0 has a determinant of 0, we can verify that $det(A)) = 0$

#pagebreak()
= Problem 3
Use the determinant to decide whether the given matrix is invertible.

== Solution
$ A = mat(2,0,3;0,3,2;-2,0,-4) $
If we reduce to REF, we can easily calculate the determinant
$ A arrow.long.r^(R_3 + R_1) mat(2,0,3;0,3,2;0,0,-2) $ 

Since we know that We know that the determinant of a triangular matix is:
$ det(A) = product_(i=1)^n a_(i i) $

Now we can calculate that $det(A) =2 dot 3 dot (-2) = -12$

Since we know that any matrix $A$ has an inverse if and only $det(A) != 0$. $A$ must have an inverse.

= Problem 4
Find the domain and codomain of the transformation defined by the equations.
$ w_1=4x_1+5x_2 $
$ w_2 = x_1 -8x_2$
== Solution
For the transformation
$ T:RR^2 --> RR^2 $
$ (x_1,x_2) --> (4x_1+5x_2,x_1-8x_2) $
This says that 
Domain = $RR^2$
Range = $RR^2$

We can compute the standard matrix for these equations
$RR^2$ has a basis ${e_1,e_2} = {(1,0),(0,1)}$
$ T(e_1) = T(1,0)=4 dot (1) $ *finish from picture*

#pagebreak()
= Problem 5
The images of the standard basis vectors for $RR^3$ are given for a linear transformation
$T : RR^3 → RR^3$. Find the standard matrix for the transformation, and find T (x).
$ T (e_1) = mat(1;3;0), T (e_2) = mat(0;0;1), T (e_3) = mat(4;-3;-1), x = mat(2;1;0) $

The standard matrix in $RR^3$ will be
$ T = mat(T(e_1),T(e_2),T(e_3))= mat(1,0,4;3,0,-3;0,1,-1) $
now we can find $T(x)$ because we know that $T(x)=A x$
$ T(x) = mat(1,0,4;3,0,-3;0,1,-1) dot mat(2;1;0) = mat(2;6;1)$

= Problem 6
A function of the form $f (x) = m x + b$ is commonly called a “linear function”
because the graph of $y = m x + b$ is a line. Is $f$ a matrix transformation on $RR$?

We use the following definition
#image("linDef.png")
By this we can prove that these only hold if $b=0$.

#pagebreak()
= Problem 7
Find the standard matrix $A$ for the linear transformation $T : RR^2 -> RR^2$ for which

$ T(mat(1;1))=mat(1;-2), T(mat(2;3))=mat(-2;5) $

== Solution
We want to find $T(e_1)$ and $T(e_2)$

We know that 
$ T(mat(1;1))=mat(1;-2) <==> A mat(1;1) = mat(1;-2)<==> mat(a,b;c,d) dot mat(1;1) = mat(1;-2) $

by this we find that
$ mat(a,b;c,d) dot mat(1;1) = mat(1;-2) <==> mat(a+b;c+d)=mat(1;-2) = cases(a+b=1,c+d=-1) $

We also know that
$ T(mat(2;3))=mat(-2;-5) <==> A mat(1;1) = mat(2;3)<==> mat(a,b;c,d) dot mat(1;1) = mat(2;3) $

by this we find that
$ mat(a,b;c,d) dot mat(1;1) = mat(2;3) <==> mat(a+b;c+d)=mat(2;3) = cases(a+b=2,c+d=3) $

2 sets 2 equations of 2 unknowns


we know that for $a = 1-b " and " 2a+3b=-2$
$ 2-2b+3b=-2 $
$ -2b+3b=-4 $
$ b=-4 $

We know that
$ c+d=-2 " and " 2c+3d=5$
$ c=-2-d $
$ 2(-2-d)+3d=5 $
$ -2d+3d=9 $ 
$ d = 9 $
$ c = -2 -(9)=-11 $

So the final matrix is 
$ T= mat(5,-4;-11,9) $

#pagebreak()
= Problem 8
Counter-clockwise rotation of vectors in $RR^2$ by an angle $theta$ in radians is a linear
transformation, which is denoted by $R^theta$ and its standard matrix is given by
$ [R^theta]=mat(cos theta,-sin theta;sin theta,cos theta) $

== Solution
$det[R^theta] = cos^2theta + sin^2theta = 1, [R^theta] = R^theta$

= Problem 9
Suppose that
$ A=mat(cos(pi/4),-sin(pi/4);sin(pi/4),cos(pi/4)) $
Computer the matrix A160. [Hint: Think about this problem geometrically.]

== Solution
We know that rotation my $theta$ in $RR^2$ is given by
$ [R^theta]=mat(cos theta,-sin theta;sin theta,cos theta) $
We then use $160 dot pi/4$
$ A=mat(cos(160pi/4),-sin(160pi/4);sin(160pi/4),cos(160pi/4)) $

