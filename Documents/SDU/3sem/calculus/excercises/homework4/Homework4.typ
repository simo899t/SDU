#import "@preview/plotsy-3d:0.2.1": plot-3d-surface
// Document setup
#set page(
  paper: "us-letter",
  margin: (left: 3cm, right: 3cm, top: 2cm, bottom: 2cm),
)
#set text(
  font: "Times New Roman",
  size: 11pt,
  lang: "en",
)
#show math.equation.where(block: true): eq => {
  block(width: 100%,align(center, $eq$))
}

#set enum(numbering: "(a)")
#set math.equation(numbering: none)
#set math.mat(delim: "[", gap: 0.3em)
#set heading(numbering: "1.")

//----------------------
#let redmath(x) = text(fill: red, $#x$)
#let bluemath(x) = text(fill: blue, $#x$)
#let greenmath(x) = text(fill: green, $#x$)

#let evaluated(expr, size: 100%) = $lr(#expr|, size: #size)$

// Custom box functions for easy use
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
  #text(size: 18pt, weight: "bold")[Exercises 4]
  
  #v(1em)
  
  Simon Holm \
    AI503: Calculus \
    Teacher: Shan Shan
]

#pagebreak()

// Table of contents
#outline(indent: auto)

#pagebreak()

= Problem 1
Consider $f(x, y) = x^2 + y^2 - 4x - 6y$.

+  Find the critical points of $f$.

+  Classify them (minimum, maximum, or saddle point) using the Hessian.

== Solution
+  We first find the derivatives 

  $ f_x(x,y)=2x-4 $
  $ f_y(x,y)=2y-6 $
  We find the critical point(s) at $ f_x(x,y)  = 0 $
  $ 2x-4=0 => x = 4/2 = 2 $

  $ f_y(x,y)=0 $
  $ 2y-6 = 0 => x=6/2=3 $


  $f$ only has 1 critical point on $(2,3)$


+  We know the hessian as
  $ H(f)(x)= #text(size: 1.5em)[$mat((partial^2f)/(partial x^2),(partial^2f)/(partial x y);(partial^2f)/(partial y x),(partial^2f)/(partial y^2))$] $

  this results to $ H(f)(x)=mat(2,0;0,2) $

  Since we know that at the critical point $ det(mat(2-lambda,0;0,2-lambda)) =0 $ We can then find that: $(2-lambda) dot (2-lambda) = 0$ and $lambda = 2$

  This is then a #underline[local minumum] by


  #theorem(title: "Second-Derivative Test Hessian")[
  At a critical point $P_0$, let $H = H(f)(P_0)$ be the Hessian matrix: Compute eigenvalues of $H$.
  #v(0.1em)
  - All positive eigenvalues $=>$ local minimum 
  - All negative eigenvalues $=>$ local maximum
  - Mixed signs $=>$ saddle point
  - Zero eigenvalue(s) $=>$ test inconclusive
  ]

#pagebreak()

= Problem 2
Let $f(u, v) = u^2 + 3 u v$, $u(x, y) = x^2 - y$, and $v(x, y) = sin(x y)$.

Compute $(partial f) / (partial x)$ and $(partial f) / (partial y)$ using the multivariale chain rule.

== Solution

For this we use the multivariate chain rule for multivariable functions
#theorem(title: "Multivariate Chain Rule")[
  For $f (g, h)$, with $g(x, y)$ and $h(u, v)$, then
  #v(0.3em)
  - $display((partial f)/(partial x)=(partial f)/(partial u)(partial u)/(partial x)+(partial f)/(partial v)(partial v)/(partial x))$
    #v(1em)
  - $display((partial f)/(partial y)=(partial f)/(partial u)(partial u)/(partial y)+(partial f)/(partial v)(partial v)/(partial y))$
  ]

$ (partial f)/(partial x)=(partial f)/(partial u)(partial u)/(partial x)+(partial f)/(partial v)(partial v)/(partial x) $
#align($
(partial f)/(partial x) &= (2u+3v) dot 2x+3u dot (cos(x y) dot y) \
&=4x u +6x v + 3y u dot cos(x y)\
&=4x(x^2-y)+6x(sin(x y)) +3y(x^2-y)dot cos(x y)\
&=4x^3-4x y+6x sin(x y)+3y x^2 cos(x y)-3y^2 cos(x y)

$)
We also know that 
$ (partial f)/(partial y)=(partial f)/(partial u)(partial u)/(partial y)+(partial f)/(partial v)(partial v)/(partial y) $

So.. again
#align($
(partial f)/(partial y) &= (2u+3v) dot (-1) + 3u dot (cos(x y) dot x)\
&= -2u  -3v + 3x u dot cos(x y)\
&=-2(x^2-y)-3dot (sin(x y))+3x(x^2-y) dot cos(x y)\
&=-2x^2+2y-3dot sin(x y)+3x^3 dot cos(x y)-3x y dot cos(x y)
$)

So finally
$ (partial f)/(partial x)=4x^3-4x y+6x sin(x y)+3y x^2 cos(x y)-3y^2 cos(x y) $
$ (partial f)/(partial y)=-2x^2+2y-3dot sin(x y)+3x^3 dot cos(x y)-3x y dot cos(x y) $




#pagebreak()
= Problem 3
A rectangular box with square base and volume $V = 1"m"^3$ is to be built with the least surface area.

(a) Express the surface area $S(x, h)$ in terms of base side length $x$ and height $h$.

(b) Eliminate one variable using the volume constraint.

(c) Use calculus to find the dimensions minimizing $S$.

== Solution
(a) Since the box must have a base of $x^2$ (top and bottom) as well as 4 sides of $4x h$
$ S(x,h)=2x^2+4x h $
(b) Since volume is constrained, we know that $1"m"^2=x^2h$

becuase of this $ h = 1/x^2 $

then substitute h in the function for $S(x,h)$

this gives
$ S(x) = 2x^2+4/x $
(c) Take the derivative of $S(x)$ and solve for $(partial S)/(partial x)S(x) = 0 $


$ (partial S)/(partial x)S(x) =4x-4x^(-2) = 4x-4/(x^2)$
$ 0 =4x-4/(x^2) $
$ 4/4=x^3 = 1 $
$S$ must have an critical point on $x = 1$

Now to find $h$ 
$ h = 1/((1)^2) = 1 $

To conclude. $x = 1$ and $h = 1$

Note that
$ S(1) = 2(1)^2+4/(1^2) = 2+4 = 6"m"^2 $

#pagebreak()
= Problem 4
Two products are manufactured in quantities $q_1$ and $q_2$ and sold at prices $p_1$ and $p_2$, respectively.  
The cost of producing them is given by

$ C = 2q_1^2 + 2q_2^2 + 10 $

(a) Find the maximum profit that can be made, assuming the prices are fixed.

(b) Find the rate of change of that maximum profit as $p_1$ increases.

== Solution

+ To maximize profit, we should first define the profit function

Profit must be $ "Profit" = "Revenue" - "Cost" $
$ pi(q_1,q_2) = R(q_1,q_2) - C(q_1,q_2) $
$ pi(q_1,q_2) = p_1q_1+p_2q_2 - (2q_1^2 + 2q_2^2 + 10) $
$ pi(q_1,q_2) = p_1q_1+p_2q_2 - 2q_1^2 - 2q_2^2 - 10) $
$ pi(q_1,q_2) = -2q_1^2+p_1q_1-2q_2^2+p_2q_2 - 10) $

Now find $(partial pi)/(partial q_1)$ and $(partial pi)/(partial q_2)$
#align($
(partial pi)/(partial q_1) &=-4q_1+p_1 <==> q_1 = p_1/4
\
(partial pi)/(partial q_2) &=-4q_2+p_2 <==> q_2 = p_2/4
$)
These are the profit-maximizing quantities

So...
#align($ 
pi_"max" =P(p_1/4,p_2/4) &= -2(p_1/4)^2+p_1(p_1/4)-2(p_2/4)^2+p_2(p_2/4) - 10) 
\
&= -(p_1^2)/8 + (p_1)/4 - (p_2^2)/8 + (p_2)/4 + 10 \
&= (p_1^2)/8 + (p_2^2)/8 + 10


$)











#pagebreak()
= Problem 5
Consider the function $f(x, y) = abs(x y)$.

+ Use a computer to draw the graph of $f$. Does the graph look like a plane when we zoom in on the origin?

+  Is $f$ differentiable at $(x, y) != (0, 0)$?

+ Show that $f_x (0, 0)$ and $f_y (0, 0)$ exist.

+ Are $f_x$ and $f_y$ continuous at $(0, 0)$?

+ Is $f$ differentiable at $(0, 0)$? 

  Hint: Consider the directional derivative  
  $f_u (0, 0)$ for $u = (i + j)(sqrt(2))$.

== Solution
+  The graph of $f(x, y) = |x y|$:

  #let func(x, y) = calc.abs(x*y)
  #align(center, [
  #plot-3d-surface(
  func,
  xdomain: (-10, 10),
  ydomain: (-10, 10),
  subdivisions: 1,
  scale-dim: (0.04, 0.04, 0.004)
  ) ])

  When we zoom in on the origin, the graph does NOT look like a plane - it has sharp ridges along the coordinate axes due to the absolute value function.
  #colbreak()

+ 
  Input $(0,0)$ into the $f_x (x,y)$ and $f_y (x,y)$
  $ f_x (0,0) = lim_(epsilon->0) (f(epsilon,0)-f(0,0))/epsilon $
  $ = lim_(epsilon->0)(0-0)/epsilon=lim_(epsilon->0^+)0/epsilon=lim_(epsilon->0^-)0/epsilon =0 $
  $ f_y (0,0) = lim_(epsilon->0) (f(0,epsilon)-f(0,0))/epsilon $
  $ = lim_(epsilon->0)(0-0)/epsilon=lim_(epsilon->0^+)0/epsilon=lim_(epsilon->0^-)0/epsilon =0 $

  They both $bold("exists")$

+ First find $f_x$ and $f_y$
  $ (partial)/(partial x) f(x,y) = (partial)/(partial x) abs(x y) = abs(y)(partial)/(partial x)abs(x) = abs(y) "sgn"(x) $  $ (partial)/(partial y) f(x,y) = (partial)/(partial y) abs(x y) = abs(x)(partial)/(partial y)abs(y) = abs(x) "sgn"(y) $
  where $ "sgn"(x) = cases(
  +1 quad &"if" x > 0,
  -1 quad &"if" x < 0,
  0 quad &"if" x = 0
  ) $ 

  So now check for $(x,y) -> (0,0)$
  $ display(lim_((x,y)->(0,0))f_x (x,y) = abs(0) + "sgn"(0) = 0)  $
  $ display(lim_((x,y)->(0,0))f_y (x,y) = abs(0) + "sgn"(0) = 0)  $
 So both exists

+ Since
 $ display(lim_((x,y)->(0,0))f_x (x,y) = 0 = f_x (0,0)) $
  $ display(lim_((x,y)->(0,0))f_y (x,y) = 0 = f_y (0,0)) $
  They are both continuous at $(0,0)$

+ For the directional derivative  
  $f_u (0, 0)$ for $arrow(u) = (i + j)/(sqrt(2)) = vec(1/sqrt(2),1/sqrt(2))$.
  #align($
  f_u (x,y)&= nabla f(x,y)dot arrow(u) = (f_x (x,y),f_y (x,y)) dot vec(1/sqrt(2),1/sqrt(2))
  \
  &= f_x (x,y) dot 1/sqrt(2) + f_y (x,y) dot 1/sqrt(2)
  \
  &= (abs(y)dot"sgn"(x))/sqrt(2) + (abs(x)dot"sgn"(y))/sqrt(2) 
  \
  &= (abs(y)dot"sgn"(x)+abs(x)dot"sgn"(y))/sqrt(2)
  \
  f_u (0,0)&=(abs(0)dot"sgn"(0)+abs(0)dot"sgn"(0))/sqrt(2) = 0
  $)
  So it is differentiable at $(0, 0)$ with $f_u (0,0) = bold(0)$
  
  