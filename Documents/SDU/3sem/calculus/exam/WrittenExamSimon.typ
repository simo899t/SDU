#import "@preview/plotsy-3d:0.2.1": plot-3d-surface
// Document setup
#set page(
  paper: "us-letter",
  margin: (left: 3cm, right: 3cm, top: 2cm, bottom: 2cm),
  header: 
"Simon Holm                                                       sihol24                                                      215751682 "
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
#set page(numbering: "1")



//----------------------
#let redmath(x) = text(fill: red, $#x$)
#let bluemath(x) = text(fill: blue, $#x$)
#let greenmath(x) = text(fill: green, $#x$)
#let int(a, b, f) = $integral_#a^#b #f$
#let limm(a,b) = $lim_(#a -> #b)$
#let summ(a,b,f) = $sum_#a^#b #f$



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
  #text(size: 18pt, weight: "bold")[Calculus written exam 2026]
  
  #v(1em)
  
  Simon Holm \
    AI503: Calculus \
    Date: January 7th 2026
]

#pagebreak()

// Table of contents
#outline(indent: auto)

#pagebreak()

= Problem 1

 Let $f : RR^3 -> R$ be defined by
 $ f(x,y,z) = x^2+y^2+z^2 $
+ Determine the domain and range of $f$
  + 
+ Describe geometrically the level set formed by $f(x, y, z) = 100$.

== Solution

+ Determine the domain and range of $f$
  
  - $D(f) = {(x,y,z):(-oo ,+oo)}$
  
  - $R(f) = [0,+oo])$
  
+ Describe geometrically the level set formed by $f(x, y, z)= x^2+y^2+z^2 = 100$.

  - $f$ represents a sphere, generally on the form $(x - h)^2 + (y - k)^2 + (z - l)^2 = r^2$
  - The sphere would have center at $0,0,0$
  - And a radius of $sqrt(100) = 10$

#pagebreak()

= Problem 2
+ Compute the following limit or show that the limit does not exist.
  $ lim_((x,y)->(0,0)) (x+y^2)/(2x+y) $
+ Is the following function continuous along
  $ f(x,y) = cases(x-1 quad & y>=0",", -2 & y<0 ) $
== Solution

+ Compute the following limit or show that the limit does not exist.
  $ lim_((x,y)->(0,0)) (x+y^2)/(2x+y) $

  #align(center)[#image("/assets/image-43.png",width: 30em)
  $f(x,y)=(x+y^2)/(2x+y)$ graphed in GeoGebra
  ]

  Then i can constant $y$ and check for x-

  $ y=0 => lim_(x->0) (x+y^2)/(2x+y) = 1/2 $
  $ y=x => lim_(x->0) (x+y^2)/(2x+y) = 1/3 $
  #v(3em)

  Since $1/2 != 1/3$

  The limit *does not* exist


+ Is the following function continuous along
  $ f(x,y) = cases(x-1 quad & y>=0",", -2 & y<0 ) $
   - *Yes*, regardless of $(x,y)$ the $f$ is defined, therefore it is continuous
= Problem 3
Let $g(x, y, z) = x^2 - 2x y^2 + a z - a$

+ Find an equation of the tangent plane to $g$ at the point $(1, 1, 1)$.
+ For which value of a does the tangent plane pass through the origin?

== Solution

+ Find an equation of the tangent plane to $g$ at the point $(1, 1, 1)$.
  
  The equation of the tangent plane at the point $(x_0, y_0, z_0)$ is given by: $ nabla g(x_0, y₀, z₀) · (x - x₀, y - y₀, z - z₀) = 0 $
  
   In our case, $(0, -4 + a, a) · (x - 1, y - 1, z - 1) = 0$ 
   
   This expands to: 

   #align(center)[
    $ 0(x - 1) + (-4 + a)(y - 1) + a(z - 1) = 0\ 
      (-4 + a)(y - 1) + a(z - 1) = 0 \
      (a - 4)y - (a - 4) + a z - a = 0 \
      (a - 4)y + a z - (2a - 4) = 0 
   $]


+ For which value of a does the tangent plane pass through the origin?
  
  For this i plug in $(0,0,0)$
  #align(center)[$ 
  (a - 4)(0) + a(0) - (2a - 4) = 0 \
  -(2a - 4) = 0 \
  2a - 4 = 0 \
  2a = 4 a = 2
  $]
#pagebreak()

= Problem 4
Below is a contour diagram of $f(x, y)$. In each of the following cases, list the marked points in the diagram (there may be none or more than one) at which

(a) $f_x < 0 $
(b) $f_y > 0$
(c) $f_(x x) > 0 $
(d) $f_(y y) < 0$



#align(center)[#image("/assets/image-44.png",width: 25em)]

Hint: what kind of function $f(x, y)$ would produce a contour plot like the one shown?
Compute its $f_x, f_y, f_(x x)$ and $f_(y y)$.



== Solution

+ For $f_x < 0 $ this means that the partial derivative is negative on the $x$-axis. Since the contour plot is showing an increase $f_(x)>0$.
  
  So $f_x<0$ is false for this contour plot 


+ As for $f_y > 0$ This means that the partial derivative is positive on the $z$-axis. Since the contour plot is showing an increase $f_(y)>0$.
  
  So $f_y<0$ is true for this contour plot.

+ $f_(x x) > 0$
+ $f_(y y) < 0$
#pagebreak()

= Problem 5
Let $F(x) = integral_0^x cos(pi t)dif t.$

+ Evaluate F(1).
+ For what values of $x$ is $F(x)$ positive? negative?
  


== Solution

+ Evaluate F(1).
  
  Since $ F(x) = integral_0^x cos(pi t)dif t= integral cos(pi x) dif x $

  and since $ F(x) =  integral cos(pi x) dif x = sin(pi x)/pi $

  $ F(1) = sin(pi)/pi = 0 $

+ For what values of $x$ is $F(x)$ positive? negative?
  
  Since $sin(a dot pi) = 0$ for any $a$
  $ F(x) = 0, quad "for any "x $ 

#pagebreak()

= Problem 6

Let $T(x, y) = e^(-x^2y^2)$
represent the heat of the point (x, y).

+ In which direction should a heat-seeking bug move from the point (-1, 1) to increase its temperature fastest?
+ Find the directional derivative of T at the point P = (-1, 1) in the  direction of $(1, 1)$.
+ Does T have a maximum or a minimum? At where? Hint: You don't need to
  compute second-order derivatives. Consider that exp is monotone increasing and how that affects the extrema of T.

== Solution
+ In which direction should a heat-seeking bug move from the point (-1, 1) to increase its temperature fastest?

  In the direction of $nabla f(-1,1)$

  $ nabla f(x,y) = (f_x (x,y), f_y (x,y)) = (-2e^(-x^2y^2)x y^2, -2e^(-x^2y^2)x^2y) $
  Then $ nabla f(-1,1) = -2e^(-(-1)^2y^2)x y^2, -2e^(-x^2y^2)x^2y = 2/e, -2/e $

+ Find the directional derivative of T at the point P = (-1, 1) in the  direction of $(1, 1)$.

  first find the unit vector of $(1,1)$
  $ u = (1,1)/sqrt(2) = (1/sqrt(2),1/sqrt(2)) $

  $ f_u = nabla f (x,y) dot u = (2/e, -2/e) dot (1,1) = 0 $


+ Does T have a maximum or a minimum? At where? Hint: You don't need to
  compute second-order derivatives. Consider that exp is monotone increasing and how that affects the extrema of T.
#pagebreak()

= Problem 7
Consider the function
$ f(x,y) = abs(x y) $
+ Is f differentiable at (1, 0)? Explain.
  
  
+ Is f differentiable at (0, 0)? Explain.

== Solution

F is differentiable on a point $(x,y)$ if $lim_((x, y) -> (a, b)) f(x, y) - f(a, b) / sqrt((x - a)² + (y - b)²)$

+ Is differentiable at (1, 0)? Explain.
  
  $ lim_((x, y) -> (1, 0)) f(x, y) - f(1, 0) / sqrt((x - 1)² + (y)²)$ 

  We check this by setting a constant

  $ "For" y = 0, quad  lim_(x->0)= 0 $

  $ "For" y = x, quad  lim_(x->0)= 1 $

  Since the limit *does not* exitst, $f(x,y)$ *is not* differentiable on poin $(1,0)$

+ Is f differentiable at (0, 0)? Explain.
  
  $ lim_((x, y) -> (0, 0)) f(x, y) - f(0, 0) / sqrt((x)² + (y)²) = 0$

  Since the limit *does* exitst, $f(x,y)$ *is* differentiable on poin $(1,0)$
#pagebreak()

= Problem 8
Use polar coordinate to evaluate $integral_R sqrt(x^2+y^2)$ where $R$ is given in the following figure.

#align(center)[#image("/assets/image-45.png",width: 25em)]

== Solution

We wantr to find $ integral_R sqrt(x^2 + y^2) " "d""A $

where $x^2+y^2 = r^2$ so

and $theta$ must be half a circle: $pi$
$ integral_(theta = 0)^(pi)integral_(r = 0)^1 r" "r" "d""r" "d""theta = integral_(theta = 0)^(pi)integral_(r = 0)^1 r^2 " "d""r" "d""theta $
Now integrate
$ integral_(theta = 0)^(pi) 1/3 " "d""theta  = bold(pi) = pi/3 $

#pagebreak()

= Problem 9
Evaluate the following integral by using cylindrical coordinate or spherical
coordinate:

$ integral.triple_W z/(x^2+y^2)^(2/3) dif V, quad W={(x,y,z):0<=x^2+y^2<=4,0<=z<= 4} $

== Solution
in this case this is:

$ 0= z/(x^2+y^2)^(2/3) => z = (x^2+y^2)^(2/3) $

$ integral.triple_W z dif V $

Because the of the cone, polar coordinates are easier to work with
$ x = r cos(theta), quad y = r sin(theta), quad underbrace(z = z, "figure on "z), quad dif V = r dif z dif r dif theta  $

In this instance $theta$ is the whole circle around the z axis so $0<=theta<= 2pi$

To find $r$ we can use the definition of the circle at $z =4$

Because of $4 = (x^2+y^2)^(2/3) => underbrace(x^2+y^2 = root(2/3,4), "circle at "z=4)$ where $root(2/3,4) = r^2$, So $r = 2sqrt(2)$

This means that $0<=r<=4$

To find z we look at the cones height from $z = 0$ to $z = 4$. 

This means that $root(2/3,x^2+y^2) <=z<=4 quad ==> r<=z<=0$

Finally we can put it all together


.. didnt finish



= Problem 10
Decide whether the following statements are true or false.
+ If a function is differentiable, it must be continuous.

+ $integral_0^1 integral_0^x f(x,y) dif x dif y = integral_0^1 integral_0^y f(x,y) dif y dif x$

+ If $integral_R f dif A = 0$ then $f$ is zero at all points in R.
(c) 
(d) If $f$ and $g$ are two functions continuous on a region $R$, then
  $ integral_R f dot g dif A = integral_R f dif A dot integral_R g dif A $

(e) A local maximum of $f$ can only occur at the critical points.

== Solution
.. didnt finish