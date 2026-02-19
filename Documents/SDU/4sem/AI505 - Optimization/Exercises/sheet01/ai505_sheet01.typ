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

#set heading(numbering: "1.")
#set enum(numbering: "(a)")
#set math.equation(numbering: none)
#set math.mat(delim: "[", gap: 0.3em)
#set math.vec(delim: "[", gap: 0.3em)


//----------------------
#let redmath(x) = text(fill: red, $#x$)
#let bluemath(x) = text(fill: blue, $#x$)
#let greenmath(x) = text(fill: green, $#x$)
#let small(x) = text(size: 8pt, $#x$)
#let big(x) = text(size: 16pt, $#x$)
#let large(x) = text(size: 20pt, $#x$)

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
  #text(size: 18pt, weight: "bold")[Exercises 1]
  
  #v(1em)
  
  Simon Holm \
    AI505: Optimization \
    Teacher: Marco Chiarandini
]

#pagebreak()

// Table of contents
#outline(indent: auto)

#pagebreak()

= Exercise $1^+$ Python
Show that the function $ f(x) = 8x_1 + 12x_2 + x_1^2-2x_2^2 $
only has one stationary point, and that it is neither
a maximum or minimum, but a saddle point (or inflection point). Plot the contour lines of f in Python

== Solution

Since $ nabla f(x_1,x_2) =(2x_1+8,-4x_2 + 12) $

$ nabla f(x_1,x_2) = 0 ==> (-8/2, -12/4) = (-4,3) $

Then $ H=mat((partial^2 f)/(partial x^2),(partial f)/(partial x partial y);(partial f)/(partial y partial x),(partial^2 f)/(partial y^2)) = mat(2,0;0,-4) $

For a hessian $det(mat(2-lambda,0;0,-4-lambda)) = 0$

So $ (2-lambda)(-4-lambda) = 0 => lambda = 2, -4 $

Since both negative and positive its a saddlepoint

$ #image("/assets/image.png", width: 30em) $

= Exercise $2^+$
Write the second-order Taylor expansion for the function $cos(1/x)$ around a nonzero point $x$, and the
third-order Taylor expansion of $cos(x)$ around any point $x$. Evaluate the second expansion for the specific
case of $x = 1$.

== Solution
- $cos(1/x)$

$ cos(1/x) approx cos(1/a) + (1/(a^2) sin(1/a))/(1!) (x-a) + (1/(a^3) 2 sin(1/a) - 1/a^4 cos(1/a))/(2!) (x-a)^2 $

- $cos(x)$
$ cos(x)approx cos(a) -sin(a)/1! (x-a) -cos(a)/2! (x-a)^2 + sin(a)/3! (x-a)^3 $

- at $x = 1$c1
$ cos(1) -sin(1)/1! (x-a) -cos(1)/2! (x-a)^2 $
$ 1.381773291 - 0.8414709848x - 0.2701511530(x - 1.0)^2 $



= Exercise $5^*$
Consider the function $f(x_1,x_2) = (x_1 + x_2^2)^2$. At th epoint $x_0 = [0,1]$ we consider the search direction $p = [-1,1]$. Show that p is a descent direction and find all minimizers of the problem $min_alpha f(x_0+alpha p)$.

== Solution
First $ f(x_0 + alpha p) = f(vec(0,1)+alpha vec(-1,1)) = f(vec(1-alpha,alpha)) $

Then $ f(vec(1-alpha,alpha)) = ((1-alpha)+alpha^2)^2 $

Then we can $ arg min_alpha ((1-alpha)+alpha^2)^2 $

$ f' = 2(alpha^2 - x + 1)(2alpha - 1) = 0 => alpha = 1/2 $

Since $ f(vec(1-1/2,1/2)) = (1/2+(1/2)^2)^2 =0.5625 < f(vec(1,0)) = 1 $

$p$ is a descent direction

= Exercise $6^+$
Consider the case of a vector function f : $R^n -> R^m$. The matrix J(x) of first derivatives for this function
is defined as follows:
#set text(size: 16pt)
$ J(x) = mat((diff f_j)/(diff x_i))_(j=1..m\ i=1..n) $
#set text(size: 10pt)

write the forward-difference calculations needed to compute J(x) at a given point x.

== Solution

Since $f$ is a vector function, e.g. $ (diff f)/(diff x) =  lim_(epsilon->0) (f_j (arrow(x)+epsilon arrow(e)_i) + f(arrow(x)))/epsilon $
#set text(size: 16pt)
$ J(x) = mat((f_j (arrow(x)+epsilon arrow(e)_i) + f(arrow(x)))/epsilon)_(j=1..m\ i=1..n)  $
#set text(size: 10pt)

= Exercise $7^+$
Adopt the forward difference method to approximate the Hessian of $f(x)$ using its gradient, $nabla f(x)$.

== Solution

Since $ nabla f_S (x) = (f(x+h s)-f(x))/h $
Then
$ H_f (x) = (nabla f(x+h s)-nabla f(x))/h $

= Exercise $10^*$
Draw the computational graph for $f(x, y) = sin(x + y^2)$. Use the computational graph with forward accumulation to compute $(diff f)/(diff y)$ at $(x, y) = (1, 1)$. Label the intermediate values and partial derivatives as
they are propagated through the graph.

== Solution

#let node = (x, y, label, color: white) => {
  place(
    top + left,
    dx: x,
    dy: y,
    box(
      width: 70pt, 
      height: 30pt,
      stroke: 1pt + black,
      radius: 4pt,
      fill: color,
      align(center + horizon, text(size: 9pt, label))
    )
  )
}

#let arrow_line(x1, y1, x2, y2) = line(start: (x1, y1), end: (x2, y2), stroke: 0.8pt + black)

#block(
  width: 100%,
  height: 130pt,
  {
    // Input nodes
    node(20pt, 120pt, $x$, color: rgb("#e8f4f8"))
    node(20pt, 40pt, $y$, color: rgb("#e8f4f8"))
    
    // Intermediate nodes
    node(110pt, 40pt, $y^2$, color: rgb("#fff4e6"))
    node(200pt, 80pt, $x + y^2$, color: rgb("#fff4e6"))
    node(300pt, 80pt, $sin(x + y^2)$, color: rgb("#e8f8e8"))
    
    // Edges - 4 separate arguments, not tuples
    arrow_line(90pt, 132pt, 200pt, 95pt)
    arrow_line(90pt, -90pt, 110pt, -90pt)
    arrow_line(180pt, -99pt, 200pt, -60pt)
    arrow_line(300pt, -74pt, 270pt, -74pt)
    arrow_line(370pt, -86pt, 400pt, -86pt)
    
    // Question mark
    place(
      top + left,
      dx: 410pt,
      dy: 87pt,
      text(fill: black, size: 18pt, weight: "bold", $?$)
    )

  }
)
Now to do forward accumulation on $f(x,y) = sin(x+y^2)$

First i will compute $ (diff)/(diff x) f(1,1) = cos(2), quad (diff)/(diff y) f(1,1) = 2cos(2) $

Now for the forward accumulation

$ x = 1, quad accent(x,dot) =0 $
$ y = 1, quad accent(y,dot) =1 $
$ c_1 = x, quad accent(c,dot)_1 = accent(x,dot) $
$ c_2 = y, quad accent(c,dot)_2 = accent(y,dot) $
$ c_3 = c_2^2, quad f(x) = x^2, quad accent(c,dot)_3 =(diff f)/(diff c_2) (diff c_2)/(diff y) = 2c_2accent(c,dot)_2 $
$ c_4 = c_1+c_3, quad accent(c,dot)_4 = accent(c,dot)_1 + accent(c,dot)_3 $
$ c_5 = sin(c_4), quad accent(c,dot)_5=cos(c_4) accent(c,dot)_4 $



