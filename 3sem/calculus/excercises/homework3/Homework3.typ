// Homework 1 - Typst Document
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
#set heading(numbering: "1.")

#let mathred(x) = text(fill: red, $#x$)
#let mathblue(x) = text(fill: blue, $#x$)
#let mathgreen(x) = text(fill: green, $#x$)

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
  #text(size: 18pt, weight: "bold")[Homework 3]
  
  #v(1em)
  
  #text(size: 14pt)[
    Simon Holm \
    AI503: Calculus \
    Teacher: Shan Shan
  ]
]



#pagebreak()

// Table of contents
#outline(
  title: [Table of Contents],
  indent: auto
)

#pagebreak()

= Problem 1

A drug is injected into a patient's blood vessel. The function $c = f (x, t)$ represents
the concentration of the drug at a distance x mm in the direction of the blood flow
measured from the point of injection and at time t seconds since the injection. What are
the units of the following partial derivatives? What are their practical interpretations?
What do you expect their signs to be?

- $(partial c)/(partial x)$

- $(partial c)/(partial t)$

== Solution

- For $(partial c)/(partial x)$:
  - Units: concentration per distance (e.g., mm/s/mm)
  - Interpretation: Rate of change of drug concentration with respect to distance along the blood vessel
  - Expected sign: Negative (concentration decreases as distance from injection point increases)

- For $(partial c)/(partial t)$:
  - Units: concentration per time (e.g., mm/s/s)
  - Interpretation: Rate of change of drug concentration with respect to time
  - Expected sign: Negative (concentration decreases over time as drug is metabolized/cleared)
= Problem 2
Is there a function f which has the following partial derivatives? If so what is it? Are
there any others?


Let
$ f_x (x,y) = 4x^3 y^2 - 2y^4 $

$ f_y (x,y) = 2x^4 y - 12x y^3 $

== Solution

If we intergrate the functions $f_x(x,y)$ and $f_y(x,y)$

$ F_x = integral f_x d x = x^4y^2-2y^4x+c(y) $

$ F_y = integral f_y d y = x^4y^2-3 y^4x+c(x) $

Since $F_x!=F_y$ , f is *not* a function.

#pagebreak()

= Problem 3
Find the rate of change of $f (x, y) = x^2 + y^2$ at the point $(1, 2)$ in the direction of the
vector $arrow(u) = vec(0.6, 0.8)$.

== Solution

We start by calculating the gradient of f:
$ ∇f(x, y) = (f_x, f_y) = (2x, 2y) $

since we know that $||u|| = 1$, we can find the directional derivative in the direction of $u$:

If it was not a unit vector $underbrace(arrow(v_u) = 1/norm(v)arrow(v), "devide each component of v with the norm") $

$ f_arrow(u)(x,y) = nabla f dot arrow(u) = (2x, 2y) dot (0.6, 0.8) = 2x(0.6) + 2y(0.8) $

At the point (1, 2):

$ f_arrow(u)(1,2) = 2(1)(0.6) + 2(2)(0.8) = 1.2 + 3.2 = 4.4 $

Thus, the rate of change of $f$ at the point (1, 2) in the direction of the vector $arrow(u)$ is 4.4.

== Another solution
For $f(x,y)$, *unit* vector $vec(u_1,u_2)$ and point $(a,b)$ $ f_(arrow(u))(a,b)=lim_(h->0)(f(a+h u_1,b+h u_2)-f(a,b))/h $

Then insert point and vector in to equation $f(x,y)=x^2+y^2$
#align($
f_(arrow(u))(a,b)&= lim_(h->0)(f((1+h(0.6)),(2+h(0.8)))-f(1,2))/h\
&= lim_(h->0)((f(1+1.2h+0.36h^2)+(4+3.2h+0.64h^2))-5)/h\
&=lim_(h->0)(4.4h+h^2)/h =lim_(h->0)(h (4.4+h))/h = 4.4


$)
#pagebreak()

= Problem 4
Let $f(x, y) = x^2y^3$. At the point $P(-1, 2)$, find a vector that is

#enum(
  numbering: "(a)",
  [in the direction of maximum rate of change],
  [in the direction of minimum rate of change], 
  [in a direction in which the rate of change is zero]
)

== Solution
To find the required vectors, we first need to compute the gradient of the function $f(x, y) = x^2y^3$:

$ nabla f(x, y) = (f_x, f_y) = (2x y^3, 3x^2y^2) $

At the point $P(-1, 2)$, we have:

$ nabla f(-1, 2) = (2(-1)(2^3), 3(-1)^2(2^2)) = (-16, 12) $

Now we can find the required vectors:

(a) The direction of maximum rate of change is given by the gradient vector itself (by definition):

$ "Max rate of change direction" = nabla f(-1, 2) = arrow(v) = vec(-16, 12) $

(b) The direction of minimum rate of change is in the opposite direction of the gradient:

$ "Min rate of change direction" = -nabla f(-1, 2) = arrow(v) = vec(16, -12) $

(c) We now look for the normal vector to the gradient, which will give us a direction in which the rate of change is zero. One such vector can be found by swapping the components of the gradient and changing one sign:

We can find this because $f_u arrow(a)=0 <==> nabla f(arrow(a))dot arrow(u)=0$ when the vectors are orthogonal.

so.. 

$ (-16,12) dot (x,y) = 0 $
$ -16x + 12y = 0 $
We can se that..
$ 16x = 12y $
and then
$ x = (12y)/16 =3/4y$ 

Thus, the normal vector is $ arrow(v) = vec(3, 4) $.

#pagebreak()
= Problem 5
Find a unit vector normal to the surface S given by $z = x^2y^2 + y + 1$ at the point (0, 0, 1).

== Solution
We start by stating that $f(x, y, z) = x^2y^2 + y + 1 - z = 0$.

Then, we compute the gradient of F:
$ nabla f(x, y, z) = (f_x (x,y,z), f_y (x,y,z), f_z (x,y,z)) = (2x y^2, 2x^2y + 1, -1) $

At the point (0, 0, 1), we have:
$ nabla f(0, 0, 1) = (0, 1, -1) = arrow(v) $

Now we convert this vector into a *unit* vector:
$ arrow(v)_u = 1/(norm(arrow(v)))arrow(v)=  1/sqrt(0^2 + 1^2 + (-1)^2)(0,1,-1)=1/sqrt(2)(0,1,-1) $ 

The *unit* vector is then $ arrow(v)_u = (0, 1/sqrt(2), -1/sqrt(2)) $

= Problem 6
A student was asked to find the directional derivative of $f (x, y) = x^2 e^y$ at the point
$(1, 0)$ in the direction of $v = 4i + 3j$. The student's answer was
$ f_v(1, 0) = nabla f (1, 0) · v = 8/(5)i + 3/(5)j $
At a glance, how do you know this is wrong?

== Solution
The student's answer is incorrect because there is no way that those two expressions are equal. the directional derivative should be a scalar value, not a vector.

The correct approach is to first normalize the vector v to get the unit vector u in the direction of v:
$ norm(v) = sqrt(4^2 + 3^2) = 5 $
$ arrow(u) = (4/5, 3/5) $

The correct answer would be $f_v(1, 0) = nabla f(1, 0) dot arrow(u) = 2 dot (4/5) + 1 dot (3/5) = 8/5 + 3/5 = 11/5 = 2.2$.

#pagebreak()
= Problem 7
Find the equation of the tangent plane at the given point.
$ f(x,y)=ln(x+1)+y^2 $
at the point $(0,3,9)$

== Solution
We start by calculating the gradient of the function $f(x, y) = ln(x+1) + y^2$:
$ nabla f(x, y) = (f_x, f_y) = ((partial f)/(partial x)ln(x+1)+y^2,(partial f)/(partial y)ln(x+1)+y^2) $
$ nabla f(x, y) = (1/(x+1), 2y) $

now we find the gradient at the point (0, 3):
$ nabla f(0, 3) = (1/(0+1), 2 dot (3)) = (1, 6) $

The equation of the tangent plane at the point $(x_0, y_0, z_0)$ is given by:
$ z - z_0 = f_x (x_0,y_0)(x - x_0) + f_y (x_0,y_0)(y - y_0) $
Substituting the values we have:
$ z - 9 = 1(x - 0) + 6(y - 3) $
Simplifying this, we get:
$ z - 9 = x + 6y - 18 $
$ z = x + 6y - 9 $

#pagebreak()

= Problem 8
Are the following statements true or false?

+ If $f(x, y)$ has $f_y (x, y) = 0$ then $f$ must be a constant.

+ If $f$ is a symmetric two-variable function, that is $f(x, y) = f(y, x)$ then $f_x(x, y) = f_y(x, y)$.

+ For $f(x, y)$, if $(f(0.01, 0) - f(0, 0))/0.01 > 0$, then $f_x (0, 0) > 0$.

== Solution

+ False, $f_y (x,y) = 0$ only says that there is a point where the the gradient is equal to 0

+ False, by contradiction $f(x,y)=x^2y^2$ for this: $ f_x (x,y)=2x y^2 $ and $ f_y(x,y) = 2x^2y $
  this is not the same, therefore it is a contradiction.


+ False. We know that $ lim_(h->0)(f(0+h,0)-f(0,0))/h = f_x(0,0) $ in this instance $h = 0.01$ (fixed) so we cant say anything for certain.

#pagebreak()
= Problem 9
Given a function defined as following.
$ L(theta_1,theta_2,b) = -1/m sum_(i=1)^(m)[y^(i)log(hat(y)^(i))+(1-y^(i))log(1-hat(y)^(i))] $
where
$ hat(y)^(i)=f(x_1^(i),x_2^(i)) $
and
$ f(x_1,x_2)=1/(1+e^(-(theta_1x_1+theta_2x_2+b))) $
Suppose $x_1^(i),x_2^(i),y^(i)in RR,i=1,dots,m$ are given
Use the fact that
$ sigma(z)=1/(1+e^(-z)), #h(1cm) sigma'(z)=sigma(z)(1-sigma(z)) $
to prove the following is true

$ (partial L)/(partial theta_1)=1/m sum_(i=1)^m (hat(y)^((i))-y^((i)))x_1^((i)) $

$ (partial L)/(partial theta_2)=1/m sum_(i=1)^m (hat(y)^((i))-y^((i)))x_2^((i)) $

$ (partial L)/(partial b)=1/m sum_(i=1)^m (hat(y)^((i))-y^((i))) $.

#pagebreak()
== Solution
To simplify $display(L=sum_(i=1)^m)l^i$ where $l^i = [y^(i)log(hat(y)^(i))+(1-y^(i))log(1-hat(y)^(i))]$

#v(1em)
+ $display((partial L)/(partial theta_1)=1/m sum_(i=1)^m (hat(y)^((i))-y^((i)))x_1^((i)))$
  
  So we need to find $(partial l)/(partial theta_1)$  
  
  $ 
  (partial l)/(partial theta_1) &= -[(partial)/(partial theta_1)y log(hat(y))+(partial)/(partial theta_1)(1-y)log(1-hat(y ))]
  \ 

  &= -[y 1/hat(y)(partial hat(y))/(partial theta_1)+(1-y)1/(1-hat(y))(partial (1-hat(y)))/(partial theta_1)]&wide "(apply ing chainrule)"
  $ 
  since $(partial (1-hat(y)))/(partial theta_1) =(partial 1)/(partial theta_1)-(partial hat(y))/(partial theta_1)  = 0 -  (partial hat(y))/(partial theta_1)$ \

  $ 
  &= -[y 1/hat(y)(partial hat(y))/(partial theta_1)+(1-y)1/(1-hat(y))(partial hat(y))/(partial theta_1)]\ 
  &= -[y 1/hat(y)(partial hat(y))/(partial theta_1)+(1-y)/(1-hat(y))(partial hat(y))/(partial theta_1)] 
  $ 
  Then factor out $(partial hat(y))/(partial theta_1)$  
  $ 
  &= -(y/hat(y)+(1-y)/(1-hat(y)))dot (partial hat(y))/(partial theta_1)\  
  $ 

  This can be simplified by:  
  $   
  y/hat(y)-(1-y)/(1-hat(y)) = (y mathred((1-hat(y))))/(hat(y)mathred((1-hat(y)))) - (mathred(hat(y))(1-y))/(mathred(hat(y ))(1-y))=(y(1-hat(y))-(hat(y)(1-y)))/(hat(y)(1-y))\

  (y- y hat(y)-(hat(y)-hat(y)y))/(hat(y)(1-y))=(y- y hat(y)-hat(y)+hat(y)y)/(hat(y)(1-y)) = (y-hat(y))/(hat(y)(1-y))  
  $ 
  So now$ (partial l)/(partial theta_1) = -(y-hat(y))/(hat(y)(1-y))(partial hat(y))/(partial theta_1). $  


  Since $sigma(x)=1/(1+e^(-x)), sigma'(x)=sigma(x)(1-sigma(x)) " and by chain-rule"$  

  Then for $f(z) = 1/(1+e^z)$ $ (partial hat(y))/(partial theta_1) => (partial hat(y))/(partial z)(partial z)/(partial theta_1) = hat(y)(1-hat(y))dot x_1 $

  So now $ (partial l)/(partial theta_1) = -(y-hat(y))/(hat(y)(1-y))(partial hat(y))/(partial theta_1) = (y-hat(y))/(hat( y)(1-y))hat(y)(1-hat(y))dot x_1 =  $
  $ (partial l)/(partial theta_1) (hat(y)-y)x_1 ==>bold((partial L)/(partial theta_1)=1/m sum_(i=1)^m (hat(y)^((i))-y^((i))) x_1^((i))) $




  #v(2em)
+ $display((partial L)/(partial theta_2)=1/m sum_(i=1)^m (hat(y)^((i))-y^((i)))x_2^((i)))$

  Following the same procedure as (a) but differentiating w.r.t. $theta_2$ instead of $theta_1$

  So. 
  $ (partial hat(y))/(partial theta_2) => (partial hat(y))/(partial z)(partial z)/(partial theta_2) = hat(y)(1-hat(y))dot x_2 $
  
  So now $ (partial l)/(partial theta_2) = -(y-hat(y))/(hat(y)(1-y))(partial hat(y))/(partial theta_2) = (y-hat(y))/(hat( y)(1-y))hat(y)(1-hat(y))dot x_2 =  $
  $ (partial l)/(partial theta_2) (hat(y)-y)x_2 ==>bold((partial L)/(partial theta_2)=1/m sum_(i=1)^m (hat(y)^((i))-y^((i))) x_2^((i))) $

  #v(2em)
+ $display((partial L)/(partial b)=1/m sum_(i=1)^m (hat(y)^((i))-y^((i))))$
  
  Following the same procedure as (a) but differentiating w.r.t. $b$ instead of $theta_1$

  So. 
  $ (partial hat(y))/(partial b) => (partial hat(y))/(partial z)(partial z)/(partial b) = hat(y)(1-hat(y)) $
  
  So now $ (partial l)/(partial b) = -(y-hat(y))/(hat(y)(1-y))(partial hat(y))/(partial b) = (y-hat(y))/(hat( y)(1-y))hat(y)(1-hat(y)) =  $
  $ (partial l)/(partial b) (hat(y)-y)x_2 ==>bold((partial L)/(partial b)=1/m sum_(i=1)^m (hat(y)^((i))-y^((i)))) $