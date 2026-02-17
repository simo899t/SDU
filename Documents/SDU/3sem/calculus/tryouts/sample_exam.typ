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
Let $f (x) = sqrt(x^2-1)$. Please answer the following questions.
+ Write down the domain and range of f .
+ Write out expressions for $f (x + 2)$ and $f (x) + 2$.
+ Does $f (x)$ have an inverse function? Explain.


== Solution
+ Write down the domain and range of f .
  - $D(f) = (infinity^- ,-1] union [1, infinity^+)$
  
  - $R(f) = (infinity^- , infinity^+ )$
  
+ Write out expressions for $f (x + 2)$ and $f (x) + 2$.
  - $f(x+2)=sqrt((x+2)^2 -1)$
  
  - $f(x)+2 ? sqrt(x^2 -1)+2$
  
+ Does $f (x)$ have an inverse function? Explain.

  - An inverse function is a function that undoes the operation of f.
    
    It would be something like $x = sqrt((f(x))^2 +1)$
    
    So yes, $f(x)$ *does* have an inverse.


= Problem 2
For $t$ in months, a population, in thousands, is approximated by a *continuous*
function
$ P(t) = cases(
  e^(k t) quad & 0 <= t <= 12,
  100 quad & t > 12
  ) $ 
+ What is the initial value of the population?
+ What must be the value of k?

== Solution
+ What is the initial value of the population?

  - At $t = 0$
  $ e^(k dot 0) = 0 $
    initial value of population is *0*

+ What must be the value of k?

  - Since $P(x)$ is approximated by a *continuous* function
    
    This says that at $t = 12$, $e^(12k) = 100$

    So, $12k = ln(100) <=> k = ln(100)/12$

    *k must be $ln(100)/12$*
#pagebreak()

= Problem 3
Use the following table to answer the questions below
  $ #table(
  columns: 7,
  [$x$], [$0$], [$0.2$], [$0.4$], [$0.6$], [$0.8$], [$1.0$], 
  [$f(x)$], [$3.7$],[$3.5$],[$3.5$],[$3.9$], [$4.0$], [$3.9$]
  
) $
+ Using the table, estimate $f'(0.6)$ and $f'(0.5)$.
+ Estimate $f''(0.6)$
+ Where do you think the maximum and minimum values of $f$ occur in the
interval $0 =< x =< 1$?

== Solution
+ Using the table, estimate $f'(0.6)$ and $f'(0.5)$.
  
  - much like the definition of a derivative $f'(a) = display(lim_(h->0))(f(a+h)-f(a))/(h)$
  $ f(x_i) approx (Delta y)/(Delta x) = (y_(i+1)-y_i)/(x_(i+1)-x_i) $
  We can roughly estimate the derivative using only 3 points (the point and those surounding) by
  $ f'(x_3) = (f(x_4)-f(x_3))/(x_4-x_3) = (4.0-3.9)/(0.8-0.6) = 0.1/0.2 = 1/2 $
  - This can also be done to $f(0.5)$
  
  $ f'(0.5) = (f(x_4)-f(x_3))/(x_4-x_3) = (3.9-3.5)/(0.6-0.4) = 0.4/0.2 = 2 $

+ Estimate $f''(0.6)$
  - This can be done in the same way
  #align($ 
  f''(0.6) &= (f'(0.8)-f'(0.4))/(0.8-0.4)\
  f'(0.8)&=(f(1.0)-f(0.6))/(1.0-0.6) = (3.9-3.9)/(1.0-0.6) = 0\
  f'(0.4)&=(f(0.6)-f(0.2))/(0.6-0.2) = (3.9-3.5)/(0.6-0.2) = 1.0\
  f''(0.6) &= -1/(0.8-0.4) = -1/(0.4)
  $)

  
+ Where do you think the maximum and minimum values of $f$ occur in the interval $0 <= x <= 1$?
  Based on the table, 
  - Maximum would be at $f(0.8) = 4.0$
  - Minimum would be at $f(0.2) = 3.5$ or at $f(0.4) = 3.5$

#pagebreak()

= Problem 4
Match the contour diagrams (a)-(d) with the surfaces (I)- (IV).
#image("/assets/image.png")

== Solution
+ $a -> "I "$
+ $b -> "IV"$
+ $c -> "II"$
+ $d -> "III"$

= Problem 5
Your house lies on the surface $z = f (x, y) = 2x^2 - y^2$ directly above the point
$(4, 3)$ in the $x y$ plane.

+ How high above the $x y$ plane do you live?
+ What is the slope of your lawn as you look from your house directly toward
  the $z$-axis (that is along the vector $(-4, -3)$)?
+ When you wash your car in the driveway, on this surface above the point
  $(4, 3)$, which way does the water run off? (Give your answer as a two-dimensional
  vector.)
+ What is the equation of the tangent plane to this surface at your house?
  
== Solution

+ How high above the $x y$ plane do you live?
  
  - $z = f(4,3) = 2(4)^2-3^3=32-9=bold(23)$
  
+ What is the slope of your lawn as you look from your house directly toward
  the $z$-axis (that is along the vector $(-4, -3)$)?

  - for $u = (-4,-3)/norm((-4,-3))=(-4,-3)/sqrt((-4)^2+(-3)^2) = vec(-4/5,-3/5) $

  $ f_u (x,y) = nabla f(x,y)^T u = vec((partial f)/(partial x), (partial f)/(partial y))^T dot vec(-4/5,-3/5) = vec(4x,-2y)^T vec(-4/5,-3/5) = (-16x+6y)/5  $
  - At house position (4,3), $f_u (4,3) = (-16(4)+6(3))/5 = (-64+18)/5 = -46/5$
  #v(1em)

+ When you wash your car in the driveway, on this surface above the point
  $(4, 3)$, which way does the water run off? (Give your answer as a two-dimensional
  vector.)

  since $nabla f$ points in the direction of steepest ascent (uphill)

  I want to find $ -nabla f(x,y) =-vec((partial f)/(partial x), (partial f)/(partial y))= -vec(4x,-2x)=vec(-4x,2x) $
  
  at point (4,3), $vec(-4(4), 2(3)) = vec(-16, 6)$

+ What is the equation of the tangent plane to this surface at your house?
  - Since $f$ is differentiable then the tangent plane is defined as
  $ z = f(a,b) + f_x (a,b)(x-a)+f_y (a,b)(y-b) $
  at house location (4,3).
  $ z = f(4,3) + f_x (4,3)(x-4)+f_y (4,3)(y-3) $
  $ z = 2(4)^2-3^2 + 4(4)(x-4)-2(3)(y-3) $
  $ z = 23 + 16(x-4)-6(y-3) $


= Problem 6
Use polar coordinate to integrate $f(x,y) = sqrt(1/(x^2 + y^2))$ over the shaded region in
the figure below.
$ #image("/assets/image-2.png", width: 15em) $
#pagebreak()

== Solution

We wantr to find $ integral_R sqrt(1/(x^2 + y^2)) " "d""A $

where $x^2+y^2 = r^2$ so
$ integral_(theta = -pi/2)^(pi/2)integral_(r = 0)^1 1/r" "r" "d""r" "d""theta = integral_(theta = -pi/2)^(pi/2)integral_(r = 0)^1 1 " "d""r" "d""theta $
Now integrate
$ integral_(theta = -pi/2)^(pi/2) 1 " "d""theta  = bold(pi) $

= Problem 7
We want to express the volume of the the half cylinder below as an integral of
the cylindrical coordinate $integral_W f " "d V $ where $W$ is the half cylinder and $d V$ = $r" "d r" "d theta" "d y$.

$ #image("/assets/image-1.png", width: 15em) $

+ What must the function f be?
+ Write the limits of the integration.
+ Evaluate the integral.

#pagebreak()

== Solution

+ What must the function f be?
  - f must be 1
+ Write the limits of the integration.
  - $ integral_(y=0)^1 integral_(theta=-pi/2)^(pi/2)integral_(r=0)^R $
+ Evaluate the integral.
  - I want to evaluate 
$ integral_(y=0)^1 integral_(theta=-pi/2)^(pi/2)integral_(r=0)^R 1 " "r" "d""r" "d""theta" "d""y $
  #align($ integral_(y=0)^1 integral_(theta=-pi/2)^(pi/2)integral_(r=0)^R 1 " "r" "d""r" "d""theta" "d""y\
  integral_(y=0)^1 integral_(theta=-pi/2)^(pi/2)integral_(r=0)^R r " "d""r" "d""theta" "d""y\
  integral_(y=0)^1 integral_(theta=-pi/2)^(pi/2)1/2R^2" "d""theta" "d""y\
  integral_(y=0)^1 1/2pi R^2 " "d""y = 1/2pi R^2
  $)

= Problem 8
Let $C$ be the line segment connecting the points $p = (1, 2, 0)$ and $q = (0, 1, -1)$

+ Find a curve $c(t) : [a, b] -> RR^3$ that traces out C.
+ Find the arc length of $c(t)$.
+ Find $norm(p - q)$

== Solution

+ Find a curve $c(t) : [a, b] -> RR^3$ that traces out C.
  
+ Find the arc length of $c(t)$.
+ Find $norm(p - q)$

  - let $k = p-q = (1-0,2-1,0-(-1))=(-1,1,1)$
    
    Then $norm(k) = sqrt(-1^2+1^2+1^2) = sqrt(3) $

    So $norm(p - q) = sqrt(3)$

#pagebreak()

= Problem 9
Let $c(t) = (sin t, cos t, t)$ with $0 <= t <= 2pi$. 
Let $F$ be defined by $F(x, y, z) =
(x, y, z)$. 

Compute $integral_c F dot d s$

== Solution



= Problem 10
Decide whether the following statements are true or false.
== Solution