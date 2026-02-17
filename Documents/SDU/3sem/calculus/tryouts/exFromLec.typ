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

#set page(numbering: "1")
#set enum(numbering: "(a)")
#set math.equation(numbering: none)
#set math.mat(delim: "[", gap: 0.3em)
#set heading(numbering: "1.")

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
  #text(size: 18pt, weight: "bold")[Exercises from Shan Shan's lectures]
  
  #v(1em)
  
  Simon Holm \
    AI503: Calculus \
    Teacher: Shan Shan
]

#pagebreak()

// Table of contents
#outline(indent: auto, depth: 2)

#pagebreak()

= Lecture 1
== Problem 1
$f(x)$ linear with $f (2) = 4, f (4) = -2.$ 
 
Find $f (x)$.

=== Solution

$f(x) = -3x + 10$

== Problem 2
$g (x)$ linear with slope $6$, $g (5) = 9$. 

Find g (x).

=== Solution

$9 = 6 dot 5 + c$

$c = 9- 6 dot 5 => c = 21$

$g(x) = 6x - 21$

== Problem 3
Draw the following polynomials

+ $y = x^2 - x + 2$
+ $y = x^3+x^2-2x-6$
+ $y = x^4 - x^3 - 7x^2 - 2x - 6$
+ $y = x^5 - x^4 - 5x^3 + 2x - 6$
#pagebreak()

=== Solution

+ $y = x^2 - x + 2$
  #image("/assets/image-3.png", width: 25em)
+ $y = x^3+x^2-2x-6 $
  #image("/assets/image-4.png", width: 25em)
+ $y = x^4 - x^3 - 7x^2 - 2x - 6$
  #image("/assets/image-5.png", width: 25em)
+ $y = x^5 - x^4 - 5x^3 + 2x - 6$
  #image("/assets/image-6.png", width: 25em)

#pagebreak()

== Problem 4
Draw the graph of the following functions
+ $y = (x + 1)(x - 1)^2$
+ $y = -x^2(x - 1)$

Are these functions polynomials? Why?

What changes about the behavior of the tails of the graph if its leading coefficient is negative?

=== Solution
+ $y = (x + 1)(x - 1)^2$
  #image("/assets/image-7.png", width: 20em)
+ $y = -x^2 (x - 1)$
  #image("/assets/image-8.png", width: 20em)

Both are still polynomials. Any power function is a polynomials

Negaive flips the entire graph

= Lecture 2

== Problem 1
Show $f (x) = x^3$ is continuous at every $c in RR$.

=== Solution
This is the same as:

show that for every $c$
$ lim_(x->c)f(x)=c^3 $

Notice that 
$ |f(x)-f(c)| = |x^3-c^3| = |(x-c)(x^2+c^2+x c)| $
if $|x-c| < 1$ (which is is when $lim_(x->c)$)

Then we can insert $c + 1$ instead of $x$

$ |x^2+c^2+x c| <= |x^2|+|c^2|+|x| |c| <= |(c+1)^2|+|c^2|+|c+1| |c| :=M $

We want to show that $|x^3-c^3|< epsilon$, so we choose
$ delta = min(1, epsilon/M) quad quad square $ 


== Problem 2
Show that $f (x) = x^3 + x - 1$ has a root in $[0,1]$
=== Solution
$f(0) =  0^3 + 0 - 1 = -1 quad bold(in.not[0,1])$

$f(1) =  1^3 + 1 - 1 = 1 quad bold(in [0,1])$

Since $f$ is continuous and because $f(0)=-1$ and $f(1)=1$, $f$ has to pass through $y=0$ at some point between $x = [0,1]$

*Conclusion:* f must have 1 or more roots in $[0,1]$
#pagebreak()


== Problem 3
Evaluate $f(x) = x^2$ at $x = 2$

=== Solution

#align($
f'(x) &= lim_(h->0) (f(x+h)-f(x))/h\
f'(2) &= lim_(h->0) (f(2+h)-f(2))/h\
&= lim_(h->0) ((2+h)^2-2^2)/h\
&= lim_(h->0) (4+h^2+4h-4)/h\
&= lim_(h->0) (h^2+4h)/h\
&= lim_(h->0) h +4 = 4

$)

== Problem 4
Let $f (x) = sqrt(x)$.

- Find $f'(4)$


=== Solution
We know that
$ f'(x) = 1/2x^(-1/2) $
So

$ f'(4) = 1/2 dot 4^(-1/2) = 1/2 dot 1/2 = 1/4 $


The tangent line at $x=4$ is:

$ y = f(4) + f'(4)(x-4) = 2 + 1/4(x-4) $


The error function (difference between $f(x)$ and its tangent approximation at $x=4$) is:
$$
	$ E(x) = f(x) - [f(4) + f'(4)(x-4)] $
$$
As $x$ approaches $4$, this error approaches $0$.
Using the tangent line to approximate $sqrt(4.1)$:

The tangent line at $x=4$ is $y = 2 + 1/4(x-4)$.

So, for $x = 4.1$:

$ y approx 2 + 1/4 dot (4.1 - 4) = 2 + 1/4 dot 0.1 = 2 + 0.025 = 2.025 $

The true value is $sqrt(4.1) approx 2.0249$, so the tangent line gives a good approximation for values close to $x=4$.
#pagebreak()

The image below shows $f(x)$, its derivative $f'(x)$, and the tangent line at $x=4$:


#image("/assets/image-10.png")
// graphed using GeoGebra


== Problem 5
+ Find and classify all of the local extrema of $f (x) = x^3 - 3x + 4$.

=== Solution

$ f'(x) = 3x^2-3 $
Set to $0$
#align($ 
0 &= 3x^2-3\
3 &=3x^2\
1 &=x^2\
x &= sqrt(1)  =plus.minus 1
$
)

#align(center)[
  #image("/assets/image-11.png", width: 20em)
]

$f'(x) = 0$ gives 2 extremas, $-1$ and $1$, this makes sence given the graph.
#pagebreak()


== Problem 6
True or False? If $f'(x_0) = 0$, then f has a local max or min at $x_0$. Explain.

=== Solution
Yes, this means that the rate of change is 0 (extrema), at point $x_0$
== Problem 7
Use the second derivative test to classify the critical points of $f (x) = x^3 - 3x + 4$.

=== Solution

We know that $f'(x) = 3x^2+3$ and that $f'(x) = 0 => x = plus.minus 1$

Then 

$ f''(x) = 6x $
$ f''(1) = 6 $
$ f''(-1) = -6 $

since $f''(1)>0 => bold("Local minimum")$

and $f''(-1)<0 => bold("Local maximum")$

== Problem 8
Consider $f (x) = (x^2 - 4)^7$. Find and classify all local extrema.

=== Solution

$ f'(x) = 7(x^2-4)^6 dot 2x $
then 
$ 0 = 7(x^2-4)^6 dot 2x $
Because of 0-rule either $2x = 0$ or $x^2-4 = 0$ 

so
$x = -2,0,2$

#pagebreak()

== Problem 9
Find the global max and min of $f (x) = x(x - 1)$ on $0 <= x <= 3$.

=== Solution
$ x^2-x $
$ f'(x) = 2x-1 $ 
$ 0 = 2x-1 => x = 1/2 $ 
$ f(1/2) = -1/4 $


then for the endpoints
$ f(0) = 0 $
$ f(3) = 6 $

Global minimum at $x = 1/2$ with $f(1/2) = -1/4$

Global minimum at $x = 3$ with $f(3) = 6$
== Problem 10
Find the global max and min of $f (x) = x^3 - 7x + 6$ on $-4 <= x <= 2$.

=== Solution

$ f'(x) = 3x^2-7 $
$ 0 = 3x^2-7 => x^2=7/3 => x = plus.minus sqrt(7/3) $

$ f(sqrt(7/3)) = -1.13 $
$ f(-sqrt(7/3)) = 13.13 $

then for endpoints

$ f(-4) = -30 $
$ f(2) = 0 $

Global minimum at $x = -4$ with $f(-4) = -30$

Global minimum at $x = -sqrt(7/3)$ with $f(-sqrt(7/3)) = 13.13$

#pagebreak()

== Problem 11
Find the global max and min of $g (x) = ln(1 + x^2)$ on $-1 <= x <= 2$.

=== Solution

$ f'(x) = 1/(1+x^2) dot 2x $

When $f'(x) = 0 => x = 0$

$f(0) = 0$

now for endpoints

$f(-1) = 0.69$

$f(2) = 1.61$

Global minimum at $x = 0$ with $f(0) = 0$

Global minimum at $x = 2$ with $f(2) = 1.13$
== Problem 12
Find the global max and min of $f (t) = t e^(-t)$ for $t >= 0$. (Hint: careful here, as the domain is neither closed nor finite) 

=== Solution

$ f'(x) = e^(-t)-t e^(-t) $
$ 0 = e^(-t)-t e^(-t) => t = 1 $
$ f(1) = 1/e approx 0.37 $

Then for endpoints
$ f(0) = 0 $

Since i cant do $f(oo)$, i can use L'Hopitals rule to show that 
$ lim_(t->oo) t/e^t = lim_(t->oo) 1/e^t = 0 $

so..
Global minimum at $x = 0$ with $f(0) = 0$ and $x = oo$ with $f(oo) = 0$

Global minimum at $x = 1$ with $f(1) = 0.37$

#pagebreak()
= Lecture 3
== Problem 1
Write the definition of $lim_(x->c) f (x) = L$

=== Solution
$lim_(x->c) f (x)$ is when we get $f(x)$ close enogh to $L$ so that

For any small $epsilon>$, there is $delta >0$ such that if $ |x-c| < delta $ then

$ |f(x)-L|<epsilon $

note that $x != c$
== Problem 2
Write the definition of a function $f$ continuous at $x = c$

=== Solution
$f$ is continous at point $x=c$ if 
$ lim_(x->c) f(x) = f(c) $

Note that, if $f$ is continous in $[a,b]$, then $f$ is continous on all points within $[a,b]$


== Problem 3
Write the definition of the derivative of $f$ at a point $x = a$

=== Solution

for a point $a$, then 

$ f'(a) = lim_(h->0) (f(x+h)-f(x))/h $

If this limit exists, $f$ is differentiable at $a$.

== Problem 4
Write the definition of the second derivative



=== Solution

Since $f''(x) = f'(f'(x))$
$ f''(a) = lim_(h->0) (f'(x+h)-f'(x))/h $
#pagebreak()

== Problem 5
Use the second derivative test to classify the critical points of $f(x) =x^3-3x+4$

We know that $f'(x) = 3x^2+3$ and that $f'(x) = 0 => x = plus.minus 1$

Then 

$ f''(x) = 6x $
$ f''(1) = 6 $
$ f''(-1) = -6 $

$f''(1)>0 => bold("Local minimum")$

and $f''(-1)<0 => bold("Local maximum")$




=== Solution

== Problem 6
Consider the function below:
- Identify the local minima and maxima. 
- Identify the global minimum and maximum.
#image("/assets/image-13.png",width: 15em)

=== Solution
*Local minima* at $x approx 0.8$ with $f(0.8) approx 2$

*Local maxima* at $x approx 2.5$ with $f(2.5) approx 7$

*Local minima* at $x approx 3.5$ with $f(3.5) approx 6$

*Global minimum* at $x approx 0.8$ with $f(0.8) approx 1.9$

*Global maximum* at $x approx 0$ with $f(0) approx 10.8$
#pagebreak()

== Problem 7
Draw the vector $(2, 4, 4)$ on the following graph and compute its length.
#image("/assets/image-14.png", width: 20em)

=== Solution

#image("/assets/image-15.png", width:  20em)
$ norm((2, 4, 4)) = sqrt(2^2+4^2+4^2)=sqrt(68) $
#pagebreak()

== Problem 8
Let $a = vec(1, 1)$.
- Draw $a$, $2a$, and $-a$.
- How can $-a$ be normalized to a unit vector?
=== Solution
  - #image("/assets/image-16.png")
  
  - Normalized to a unit vector $(-a)_u = (-a)/norm(-a) =(-1,-1)/sqrt((-1)^2+(-1)^2) =vec(-1/sqrt(2),-1/sqrt(2))  $
#pagebreak()

== Problem 9
Draw $bold(b) - bold(a)$

=== Solution
#image("/assets/image-17.png")

#pagebreak()

== Problem 10
Let $ bold(v)=(3,4) quad bold(w)=(-4,3) $
- (1) Compute $bold(v) dot bold(w)$ ($bold(v^T w)$) 
- (2) Use (1) to deduce that the vectors form a right angle
- Draw the two vectors on the plane to confirm (2)

=== Solution
- $bold(v) dot bold(w) = 3 dot (-4) + 4 dot 3 = -12+ 12 = 0$
- When dot product $=0$, angle is between them is 90
  $ cos(90) norm(bold(v))norm(bold(w))= 0 = bold(v) dot bold(w) $
  Since neither $norm(bold(v))$ or $norm(bold(w))$ is 0 $cos(theta) = 0$, so $theta = 90$
- #image("/assets/image-18.png")
- 

== Problem 11
Can you give the standard basis vectors in $RR^n$?

=== Solution
for any vector $a$ in $RR^n$
$ a = a_1 e_1 +  a_2 e_2 +  dots + a_n e_n  $

Then the standart basis vectors are
$ e_1 = vec(1,0, dots.v, 0) , e_2 = vec(0,1, dots.v, 0), quad dots quad, e_n = vec(0,0, dots.v, 1) $

== Problem 12
What are the domain and range for each of these functions?
- $f (x, y ) = x^2 + y^2$
- $f(x,y)=(x^2+y^2)log(x y)$
- $f(x,y,z) = log(z)x^2 y^2$
=== Solution
- $f (x, y ) = x^2 + y^2$
  - $D(f) = RR^2$
  - $R(f) = [0,oo)$
  
- $f(x,y)=(x^2+y^2)log(x y)$
  - $D(f) = {(x,y) in RR^2 | x y > 0}$
  - $R(f) = RR$
  
- $f(x,y,z) = log(z)x^2 y^2$
  - $D(f) = {(x,y,z) in RR^3 | z > 0}$
  - $R(f) = RR$

#pagebreak()

= Lecture 4
== Problem 1
Can you describe in words the graphs of the following functions?
- $g (x, y ) = x^2 + y^2 + 5$
- $h(x, y ) = -x^2 - y^2 $
- $q(x, z) = x^2 + (z - 1)^2$
=== Solution
- $g (x, y ) = x^2 + y^2 + 5$
  
  upward circular paraboloid (bowl) that is shifted up by 5
  #image("/assets/image-19.png", width: 20em)

- $h(x, y ) = -x^2 - y^2 $
  
  downward circular paraboloid (bowl)
  #image("/assets/image-20.png", width: 20em)
#pagebreak()

- $q(x, z) = x^2 + (z - 1)^2$
  
  upward circular paraboloid (bowl) that is shifted + 1 in the direction of z
  #image("/assets/image-21.png", width: 20em)

== Problem 2
Describe the cross-sections of the function $g (x,y)=x^2-y^2$ with $y$ fixed and then with $x$ fixed. Use these cross-sections to describe the shape of the graph of $g$.

=== Solution

- with $x^2-y^2$ and $y=c$ is an upwards parabola
- with $x^2-y^2$ and $x=c$ is an downwards parabola
- together = saddle surface

#pagebreak()

== Problem 3
Describe the level sets of the functions

-  $f(x, y , z) = x^2 + y^2 + z^2$ 
-  $f (x, y , z) = x^2 + y^2 - z^2$
=== Solution
-  $f(x, y , z) = x^2 + y^2 + z^2$ 
  
  Level sets $x^2+y^2+z^2=c$
    
  For $c = 0$ a single point at $(0,0,0)$
  #image("/assets/image-23.png", width: 20em)

  For $c > 0$ a 3D-sphere with $r=c$
  #image("/assets/image-24.png", width: 20em)

  For $c > 0$ a single point at $(0,0,0)$
  
  Undefindes as $(-1)^2 =1$

#pagebreak()

-  $f (x, y , z) = x^2 + y^2 - z^2$
  
  For $c = 0$ a double cone with center at $(0,0,0)$
  #image("/assets/image-25.png", width: 20em)

  For $c < 0$ a hyperboloid of one sheet
  #image("/assets/image-26.png", width: 20em)
  For $c > 0$ a hyperboloid of one sheets
  #image("/assets/image-27.png", width: 20em)

#pagebreak()

== Problem 4
Find the equation of the plane passing through the points $(1, 0, 1), (1, -1, 3), "and" (3, 0, -1)$.


=== Solution

The equation for the plane $ a(x-x_0)+b(y-y_0)+c(z-z_0)=0 $ 

with $ "point" P(x_0,y_0,z_0) "and normal vector" N(a,b,c) $

i will use the tree points to make 2 vectors in the plane
$ v = (1, 0, 1) - (1, -1, 3) = vec(0,1,-2) $
and
$ w = (3,0,-1) - (1, -1, 3) = vec(2,1,-4) $

Now by using cross product $N = v times w $
$ N= v times w = vec(-2,-4,-2) $

with point $P(1,0,1)$ the plane becomes

$ -2(x-1) -4(y-0) -2(z-1) = 0 $
$ -2x+2-4y-2z+2= 0 $
$ -2x-4y-2z+4 = 0 $
#pagebreak()


== Problem 5
What is the minimum number of points required to uniquely determine a plane?

=== Solution
3

== Problem 6
In one-variable calculus, a limit exists at x = a if the left-hand and right-hand limits match:
$ lim_(x->a^+)f(x) = lim_(x->a^-)f(x) $

Does this make sense for multivariate functions?

=== Solution
Yes, but this must be true for all paths. (impossible task in practice)

So to show that a multivariate limit does not exist, you look for counterexamples:

$ (2x y)/(x^2-y^2) $

$ y=0 => lim_(x->0) (2x y)/(x^2-y^2) = 0 $
$ y=x => lim_(x->0) (2x^2)/(2x^2) = 1 $

Limit does not exits since $0 != 1$

== Problem 7
Does the following limit exists?
$ lim_((x,y)->(0,0)) (x y)/(x^2+y^2) $

=== Solution

$ y=0 => lim_(x->0) (x y)/(x^2+y^2) = 0 $
$ y=x => lim_(x->0) (x^2)/(2x^2) = 1/2 $

DNE
#pagebreak()

== Problem 8
Does the following limits exists?
- $lim_((x,y)->(0,0)) e^(-x-y)$
- $lim_((x,y)->(0,0)) x^2 +y^2$
- $lim_((x,y)->(0,0)) (x^2 y)/(x^2+y^2)$

=== Solution
- $lim_((x,y)->(0,0)) e^(-x-y)$
  
  - $y=0 => lim_(x->0) e^(-x-y) = 0$
  
  - $y=x => lim_(x->0) e^(-x-y) = 1/2$
  
- $lim_((x,y)->(0,0)) x^2 +y^2$
  
  - this is just 0
  
- $lim_((x,y)->(0,0)) (x^2 y)/(x^2+y^2)$
  
  We want to prove that $lim_((x,y)->(0,0)) (x^2 y)/(x^2+y^2) = 0$

  By definition 
$ forall epsilon>0,exists delta >0 | underbrace((sqrt(x^2+y^2)), "(x,y) to (0,0)") <delta ==> abs((x^2 y)/x^2+y^2)<epsilon $
$quad$Since 
$ (x^2 abs(y))/(x^2+y^2)<=(x^2 +y^2)/(x^2+y^2)abs(y) $
$quad$ then
$ (x^2 abs(y))/(x^2+y^2)<=abs(y) <= sqrt(x^2+y^2)<= delta $
$quad$ choose $epsilon = delta$ then

$ (x^2abs(y))/(x^2+y^2)<= epsilon $

$quad$So $ lim_((x,y)->(0,0)) (x^2 y)/(x^2+y^2) = 0 quad square $

#pagebreak()

== Problem 9
Is the function continuous in the given region?
- $ 1/(x^2+y^2)$ on $-1<=x, y<=1$
- $ 1/(x^2+y^2)$ on $1<=x, y<=2$
- $ y/(x^2+2)$ on $x^2+y^2<=1$

=== Solution
- $ 1/(x^2+y^2)$ on $-1<=x, y<=1$
  
  at $x =y = 0$, $1/(x^2+y^2)$ is undefined

- $ 1/(x^2+y^2)$ on $1<=x, y<=2$
  
  Since $1 <= x$, $x^2+y^2$ can never be 0, so yes it is continuous

- $ y/(x^2+2)$ on $x^2+y^2<=1$
  
  Because of $x^2$, $x^2+2$ can not become 0, so yes it is continuous


== Problem 10
Imagine an unevenly heated thin rectangular metal plate lying in the $x y$-plane. Temperature at $(x, y )$ is $T (x, y)$ 

#align(center)[
  #image("/assets/image-29.png",width: 35em)
]
How does T vary near the point (2, 1)?

=== Solution
Its increasing towards $x=3$ and decreasing towards $x=1$

Also its increasing towards $y=0$ and decreasing towards $y=2$
#pagebreak()

== Problem 11
#align(center)[#image("/assets/image-30.png",width: 20em)]
- What is the sign of $f_x (0, 5)$?

- What is the sign of $f_x (0, 5)$?

=== Solution
I am assuming that the direction of the derivative is towards the x and y axis (which are the positive ends)
- What is the sign of $f_x (0, 5)$?
  
  negative

- What is the sign of $f_x (0, 5)$?
  
  positive


== Problem 12
Let $ f(x,y) = (x^2)/(y+1) $


Find $f_x(3,2)$
=== Solution
$ f_x(x,y) = partial/(partial x)(x^2)/(y+1) = (2x)/(y-1) $
$ f_x(3,2) = (2 dot 3)/(2-1)=6/1 =6 $

#pagebreak()

== Problem 13
Find tangent plane to $f(x,y)=x^2+y^2$ at $(3,4)$

=== Solution

If $f$ is differentiable, then $ z = f(a,b)+ f_x(a,b)(x-a) + f_y(a,b)(y-b) $

Find the derivatives

$ f_x(x,y) = 2x quad f_y(x,y) = 2y $

Now plug it into the tangent equation
$ z = 3^2 + 4^2 + (2 dot 3) (x-3) + (2 dot 4) (y-4) $
$ z = 25 + 6(x-3) + 8(y-4) $
$ z = 25 + 6x-18 + 8y-32 $
$ z = 6x + 8y-25 $
#pagebreak()

== Problem 14
Let $f (x, y ) = x^2 + y^2$ and call the tangent plane you computed at $(3,4)$ $L(x, y)$. We define the error function
$ E(x,y)= f(x,y) - L(x,y) $
The error near $(3,4)$ is given by $E(3 + h, 4 + k)$ where $h, k$ are small.

What is the distance between $(3 + h, 4 + k)$ and $(3, 4)$?

Compare $abs(E(3 + h, 4 + k))$ with $h^2 + k^2$.

Observe that: $ abs(E(x,y))/(h^2+k^2) approx c $

What can you say about the error near $(3, 4)$?
=== Solution

- What is the distance between $(3 + h, 4 + k)$ and $(3, 4)$?
  
$ "dist" = sqrt(h^2+k^2) $

- Compare $abs(E(3 + h, 4 + k))$ with $h^2 + k^2$.
  
$ E(3 + h, 4 + k) = f(3+h,4+h) - L(3+h,4+h) $
$ 25+6h+8k+h^2+k^2-25+6h+8k = h^2+k^2 $
$quad$So actually we see that
$ abs(E(3 + h, 4 + k)) = h^2+k^2 $

Now $ abs(E(x,y))/(h^2+k^2) =1 $

- What can you say about the error near $(3, 4)$?
  The error is quadratic in the distance from $(3,4)$

  When $(h,k)->(0,0)$, $sqrt(h^2+k^2)->0$ faster than $h^2+k^2$ 
  $ (h^2+k^2)/sqrt(h^2+k^2) = sqrt(h^2+k^2) ->0 $

  This means that the error goes to 0 faster than the distance itself

  For the tangent, this means that even if you move a little bit away (small $h,k$), the value of the tangent plane is almost the same as the function itself.

#pagebreak()
= Lecture 5
== Problem 1
Compute partial derivatives by treating all other variables as constants
$ f(x,y,z)=x^2y z $

=== Solution

Using
$ (partial f)/(partial x_i) (x_1,dots,x_n) = lim_(h->0) (f(x_1,dots,redmath(x_i+h),dots, x_n)-f(x_1,dots,redmath(x_i),dots, x_n))/h $

$ (partial f)/(partial x)=2x y z quad (partial f)/(partial y)=x^2z quad (partial f)/(partial z)=x^2y $

== Problem 2
Compute the partial derivatives of
$ f(x_1,dots,x_n) = sum_(i=1)^n x_i^2 $



=== Solution
$ (partial)/(partial x_1)sum_(i=1)^n x_i^2, (partial)/(partial x_2)sum_(i=1)^n x_i^2, dots , (partial)/(partial x_n)sum_(i=1)^n x_i^2  $
$ 2x_1, 2x_2,dots, 2x_n $

== Problem 3
When $u = i$ or $u = j$, what is $f_u (a, b)$?

=== Solution

This means that either $u_1=1 "and" u_2 = 0$ or $u_1=0 "and" u_2 = 1$  $ u=1 i + 0j = i quad or u=0 i + 1j = j $
Because of this we only stepping either $a+h$ or $b+h$

This means that
$ f_bold(u)= lim_(h->0) ((a,b) = f(a+h, b)-f(a,b))/h $
or
$ f_bold(u)=lim_(h->0) ((a,b) = f(a, b+h)-f(a,b))/h $

This is exactly the same as taking the partial derivative with respect to the variable in that direction (partial derivatives)
so 
$ f_i (x,y) = f_x (x,y) $
$ f_j (x,y) = f_y (x,y) $


== Problem 4
For $f_u (x,y)$ What happens when u is not a unit vector?

=== Solution
We take larger steps and therefore the $f_u (x,y)$ is scaled by some number

Let $u$ not be a unitvector, then


$ f_u (x,y) = k dot f_hat(u) (x,y) quad "where" hat(u) =u/norm(u) $

Where $k = norm(u), "when" u!=0 $

To get the true rate of change per unit distance, you divide by $norm(u)$ (i.e., use a unit vector).

== Problem 5
Calculate the directional derivative of $f (x,y) = x^2 + y^2$ at $(1, 0)$ in the direction of $i + j$.

=== Solution
$ u = i + j = vec(1,0)+vec(0,1) = vec(1,1)  $

$ hat(u) = (1,1)/norm(u)= (1/sqrt(2),1/sqrt(2)) $

#align(center)[$
f_bold(u)(x,y) &= lim_(h->0)(f(a+h/sqrt(2), b+h/sqrt(2))-f(a,b))/h\
f_bold(u)(1,0) &= lim_(h->0)(f(1+h/sqrt(2), h/sqrt(2))-f(1,0))/h\
&= lim_(h->0)((1+h/sqrt(2))^2+(h/sqrt(2))^2 -1)/h = sqrt(2)
//simplify with maple
$]


#pagebreak()

== Problem 6
- Approximate $f (a_1 + h u_1, dots, a_n + h u_n)$ with the first-order approximation at $(h u_1, dots , h u_n)$

- What happens when you simplify the limit in the previous definition?
=== Solution
- Approximate $f (a_1 + h u_1, dots, a_n + h u_n)$ with the first-order approximation at $(h u_1, dots , h u_n)$

For $x = (x_1,dots,x_n)$ near $a = (a_1,dots,a_n)$

$ f(x) approx L(x) = f(a) + sum^n_(x=1) (partial f)/(partial x_i)(a)(x_i-a_i) $


For $x = (a_1+h u_1,dots,a_n + h u_a)$ near $h =(h u_1,dots,h u_n)$

$ f(x) approx L(x) = f(h) + sum^n_(x=1) (partial f)/(partial x_i)(h)a_i $
or
$ f(a_1+h u_1,dots,a_n + h u_a) approx L(a_1+h u_1,dots,a_n + h u_a) = \ f(h u_1,dots,h u_n) + sum^n_(x=1) (partial f)/(partial x_i)(h u_1,dots,h u_n)a_i $
- What happens when you simplify the limit in the previous definition?
   
  For the previous definition #redmath("(in the notes)")

  $ f_bold(u)(a) = lim_(h->0)(f(a_1+h u_1,dots, a_n+h u_n)-f(a_1,dots,a_n))/h $

  $ f_bold(u)(a) approx lim_(h->0) (f(h) + sum^n_(x=1) (partial)/(partial x_i)f(h)a_i-f(a))/h $

  $ f_bold(u)(a) approx sum^n_(x=1) (partial)/(partial x_i)f(h)(a_i)/h $

  since $(a_i)/h = u_i$

  $ f_bold(u)(a) a= sum^n_(x=1) (partial)/(partial x_i)f(h)u_i = nabla f(a) dot u $

#pagebreak()

== Problem 7
Use the preceding formula ($nabla f(a) dot u$) to calculate the directional derivative of $f (x, y ) = x^2 + y^2$ at $(1, 0)$ in the direction of $i + j$.


=== Solution

Now that we know that $f_u (x,y) = nabla f(a) dot u$

$ u = i + j = vec(1,0)+vec(0,1) = vec(1,1) $
$ hat(u) = (1,1)/norm(u)= (1/sqrt(2),1/sqrt(2)) $

$ f_hat(u) (x,y) = nabla f(x,y) dot u = vec(2x,2y)dot vec(1/sqrt(2),1/sqrt(2)) = (2x)/sqrt(2)+(2y)/sqrt(2)  $
$ f_hat(u) (1,0)  = vec(2,0)dot vec(1/sqrt(2),1/sqrt(2)) = (2)/sqrt(2)+(0)/sqrt(2) =2/sqrt(2) =sqrt(2) $


== Problem 8
Find the gradient of $f (x, y ) = x + e^y$ at $(1, 1)$. Use it to compute the directional derivative in the direction of $i + j$.

=== Solution
$ u = i + j = vec(1,0)+vec(0,1) = vec(1,1) $
$ hat(u) = (1,1)/norm(u)= (1/sqrt(2),1/sqrt(2)) $

$ f_hat(u) (x,y) = nabla f(x,y) dot u = vec(1,e^y)dot vec(1/sqrt(2),1/sqrt(2)) = (1)/sqrt(2)+(e^y)/sqrt(2)  $

$ f_hat(u) (1,1) = (1)/sqrt(2)+(e^1)/sqrt(2) = (1+e)/sqrt(2) $

#pagebreak()


= Lecture 6
== Problem 1

We computed the directional derivative by the inner product with the gradient vector
$ f_u (a) = nabla f(a) dot u $
Recall that the inner product measures the alignment of two vectors:
$ nabla f(a) dot u = norm(nabla f(a)) dot underbrace(norm(u), "1") dot cos(theta) = norm(nabla f(a)) dot cos(theta) $
Think, what does this tell us about the gradient vector?

=== Solution

Because of $ f_u (a) = norm(nabla f(a)) dot cos(theta) $

As long as $norm(f) != 0$

- When looking for the *maximum* increase. Assume that $theta = 0$

  Then $f_u (a)$ is maximized, when the direction $u$ points along of $nabla f(a)$ (i.e. when $theta = 0$)
  $ f_u (a) = norm(nabla f(a)) $

- We can also *minimize* by $theta =180$

  Then $f_u (a)$ is minimized, when the direction $u$ points opposite of $nabla f(a)$ (i.e. when $theta = 180$)
  $ f_u (a) = -norm(nabla f(a)) $

- What about *no change*?
  
  Then, when the direction $u$ is perpendiculat ($perp$) to $nabla f(a)$ (i.e. when $theta = 90$)
  $ f_u (a) = 0 $
  
#pagebreak()
== Problem 2
In the following picture, mark the gradient vector of f at (a, b).
#align(center)[
  #image("/assets/image-31.png",width: 30em)
]
=== Solution
#align(center)[
  #image("/assets/image-32.png",width: 29em)
]
#pagebreak()

== Problem 3
Let's consider a simple classification example where we need to classify whether a student passes an exam based on two features:
- $x_1 = "hours studied"$
- $x_2 = "hours slept"$
  
The goal is to make a binary classification such that
- $y = 1$ if the student passes (positive class).
- $y = 0$ if the student passes (positive class).

Suppose a data set ${x^((i))_1 , x^((i))_2 , y^((i))}$ with $i = 1,dots , m$ is given.

The logistic regression model is defined by
$ f(x_1,x_2) = 1/(1+e^(-(theta_1 x_1+theta_2 x_2+b))) $

where $θ_1$ and $θ_2$ are parameters (weights) for the features. and $b$ is some bias term.

The predicted outcome using the logistic model is
$ y^((i)) = f(x_1^((i)),x_2^((i))) $

Think: What is the range of $f$?

The amount of error we made is characterized by the following function. Let us define

$ L(theta_1,theta_2,b)=sum_(i=1)^m  [y^(i)log(hat(y)^(i))+(1-y^(i))log(1-hat(y)^(i))] $

What is the gradient vector?

=== Solution
- What is the range of $f$?
  
  The range of the general logistic function $sigma(z)=(1/(1+e^(-z)))$ is:

  - As $z->+oo, sigma(z) ->0 $
  - As $z->-oo, sigma(z) ->1 $

#pagebreak()

- What is the gradient vector?
$ nabla f(theta_1,theta_2,b) = ((partial L)/(partial theta_1),(partial L)/(partial theta_2),(partial L)/(partial b)) $

To simplify $display(L=sum_(i=1)^m)l^i$ where $l^i = [y^(i)log(hat(y)^(i))+(1-y^(i))log(1-hat(y)^(i))]$

#v(1em)
+ $display((partial L)/(partial theta_1)=1/m sum_(i=1)^m (hat(y)^((i))-y^((i)))x_1^((i)))$
  
  So we need to find $(partial l)/(partial theta_1)$  
  
  $ 
  (partial l)/(partial theta_1) &= -[(partial)/(partial theta_1)y log(hat(y))+(partial)/(partial theta_1)(1-y)log(1-hat(y ))]
  \ 

  &= -[y 1/hat(y)(partial hat(y))/(partial theta_1)+(1-y)1/(1-hat(y))(partial (1-hat(y)))/(partial theta_1)]&wide "(applying chainrule)"
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
  y/hat(y)-(1-y)/(1-hat(y)) = (y redmath((1-hat(y))))/(hat(y)redmath((1-hat(y)))) - (redmath(hat(y))(1-y))/(redmath(hat(y ))(1-y))=(y(1-hat(y))-(hat(y)(1-y)))/(hat(y)(1-y))\

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

$ nabla f(theta_1,theta_2,b) = (1/m sum_(i=1)^m (hat(y)^((i))-y^((i))) x_1^((i)), 1/m sum_(i=1)^m (hat(y)^((i))-y^((i))) x_2^((i)),1/m sum_(i=1)^m (hat(y)^((i))-y^((i))) ) $
$ nabla f(theta_1,theta_2,b) = 1/m sum_(i=1)^m (hat(y)^((i))-y^((i)))( x_1^((i)), x_2^((i)),1 ) $
#pagebreak()

== Problem 4
A function $f (x_1, dots, x_n)$ has n first-order partial derivatives
$ (partial f)/(partial x_i), quad i = 1,dots,n  $
How many second-order partial derivatives does it have?

=== Solution
Since
$ (partial^2 f)/(partial x_i^2), (partial^2 f)/(partial x_i partial x_j) quad i,j = 1,dots,n, quad i !=j  $

$n^2$ total mixed derivatives. So $ n + n(n-1) = n^2 $


== Problem 5
Compute the second-order partial derivatives of
$ f(x_1,x_2) = x_1x_2^2+3x_1^2 e^(x_2) $

=== Solution
There should be $n^2 =2^2 = 4$ possible second-order partial derivatives of $f(x_1,x_2)$

These are 
$ (partial^2 f)/(partial x_1^2), quad (partial^2 f)/(partial x_2^2),quad (partial^2 f)/(partial x_1 partial x_2),quad  (partial^2 f)/(partial x_2 partial x_1) $
//      (partial)/(partial x_1)
Now compute them all
$ (partial^2 f)/(partial x_1^2) = (partial)/(partial x_1)(x_2^2+6 x_1 e^(x_2)) = 6e^(x_2) $
$ (partial^2 f)/(partial x_2^2) = (partial)/(partial x_2) (2x_1 x_2 + 3x_1^2 e^(x_2)) = 2x_1+3x_1^2 e^(x_2) $
$ (partial^2 f)/(partial x_1 partial x_2) = (partial)/(partial x_1) (2x_1 x_2 + 3x_1^2 e^(x_2)) = 2x_2 + 6x e^(x_2) $
$ (partial^2 f)/(partial x_2 partial x_1) = (partial)/(partial x_2)(x_2^2+6 x_1 e^(x_2))= 2x_2 + 6x e^(x_2) $
#pagebreak()

== Problem 6
Find and analyze the critical points of 
$ f(x,y) = x^2-2x+y^2-4y+5 $

=== Solution
Then $ H=mat((partial^2 f)/(partial x^2),(partial f)/(partial x partial y);(partial f)/(partial y partial x),(partial^2 f)/(partial y^2)) = mat(2,0;0,2) $

For a hessian $det(mat(2-lambda,0;0,2-lambda)) = 0$

So $ (2-lambda)(2-lambda)-0 dot 0 = 0 => lambda = 2 $

So local mimimum

#align(center)[#image("/assets/image-35.png",width: 30em)]

As shown here, the critical point is also the global min

$ 0 = (partial f)/(partial x) => x = 1 $
$ 0 = (partial f)/(partial y) => y = 2 $

gobal min is at $x=1,y=2 -> f(1,2) = 0$


== Problem 7
Find and analyze any critical points of

$ f(x,y)=-sqrt(x^2+y^2) $

=== Solution

$ f_x (x,y) = -x/(sqrt(x^2+y^2)), quad f_y (x,y) = -y/(sqrt(x^2+y^2)) $
$ f_x (0,0) = 0/0 = "DNE", quad f_y (0,0) = 0/0 = "DNE" $

f is a cone though, where its global maximum is $f(0,0) = 0$

#align(center)[#image("/assets/image-33.png",width: 30em)]
#pagebreak()


== Problem 8
Find and analyze any critical points of
$ f(x,y) = x^2+y^2 $

=== Solution
Then $ H=mat((partial^2 f)/(partial x^2),(partial f)/(partial x partial y);(partial f)/(partial y partial x),(partial^2 f)/(partial y^2)) = mat(2,0;0,2) $

For a hessian $det(mat(2-lambda,0;0,2-lambda)) = 0$

So $ (2-lambda)(2-lambda)-0 dot 0 = 0 => lambda = 2 $

So local mimimum

#align(center)[#image("/assets/image-34.png",width: 30em)]

The minimum must also be the global min

$ 0 = (partial f)/(partial x) => x = 0 $
$ 0 = (partial f)/(partial y) => y = 0 $

gobal min is at $0,0 -> f(0,0) = 0$

#pagebreak()

== Problem 7
Classify the critical points of
$ f(x,y) = x^4+y^4 $

=== Solution

Then $ H=mat((partial^2 f)/(partial x^2),(partial f)/(partial x partial y);(partial f)/(partial y partial x),(partial^2 f)/(partial y^2)) = mat(12x^2,0;0,12y^2) $

For a hessian $det(mat(12x^2-lambda,0;0,12y^2-lambda)) = 0$

So $ (12x^2-lambda)(12y^2-lambda) = 0 => lambda = 2 $

Because of $lambda_1,lambda_2 < 0$, this indicates local mimimum

#align(center)[#image("/assets/image-36.png",width: 30em)]

The local minimum must also be the global min

$ 0 = (partial f)/(partial x) => x = 0 $
$ 0 = (partial f)/(partial y) => y = 0 $

gobal min is at $0,0 -> f(0,0) = 0$



#pagebreak()

== Problem 8
Classify the critical points of
$ f(x,y) = -x^4-y^4 $

=== Solution

Then $ H=mat((partial^2 f)/(partial x^2),(partial f)/(partial x partial y);(partial f)/(partial y partial x),(partial^2 f)/(partial y^2)) = mat(-12x^2,0;0,-12y^2) $

For a hessian $det(mat(-12x^2-lambda,0;0,-12y^2-lambda)) = 0$

So $ (12x^2-lambda)(12y^2-lambda) = 0 => lambda = -12*x^2 $
Since $lambda_1, lambda_2 <=0$, I cannot directly conclude anything, only based on the eigenvalues from the hessian matrix. But since for $x,y !=0$, all eigenvaleus are strictly negative, this is most likely a local maximum

#align(center)[#image("/assets/image-37.png",width: 30em)]


The local max is clearly shown above.
The local max must also be the global maxima

$ 0 = (partial f)/(partial x) => x = 0 $
$ 0 = (partial f)/(partial y) => y = 0 $

gobal min is at $0,0 -> f(0,0) = 0$

#pagebreak()

== Problem 9
Classify the critical points of
$ f(x,y) = x^4-y^4 $



=== Solution
Then $ H=mat((partial^2 f)/(partial x^2),(partial f)/(partial x partial y);(partial f)/(partial y partial x),(partial^2 f)/(partial y^2)) = mat(12x^2,0;0,-12y^2) $

For a hessian $det(mat(12x^2-lambda,0;0,-12y^2-lambda)) = 0$

So $ (12x^2-lambda)(12y^2-lambda) = 0 => lambda = -12x^2, 12y^2 $

Since $lambda <=0>=lambda$ and , I cannot directly conclude anything, only based on the eigenvalues from the hessian matrix. But since for $x,y !=0$, the eigenvaleus have mixed signs, this is most likely a sadle point

#align(center)[#image("/assets/image-38.png",width: 30em)]

The saddlepoint can be seen above.

$ 0 = (partial f)/(partial x) => x = 0 $
$ 0 = (partial f)/(partial y) => y = 0 $

The saddle point is at $0,0 -> f(0,0) = 0$

#pagebreak()

= Lecture 7
 
== Problem 1
Do all functions have global extrema? Can you think of a counterexample?
=== Solution
The global max is the the largest value $f(x)$ takes.

But if $f(x) -> +oo$, then $f(x)$ does not have a global extrema

== Problem 2
Does the function $ f(x,y)=1/(x^2+y^2) $ have global maxima and minima on the region $R$ given by $0<x^2+y^2<=1$?

=== Solution

First inspect $f$
#align(center)[#image("/assets/image-39.png",width: 30em)]

Since $R$ is $(0,1]$, $R$ is not closed, therfore $f$ does not have a global max

It does however has a global min at $1/(x^2+y^2) = 1)$
#pagebreak()

== Problem 3
Does the function
$ f(x,y) = x^2y^2 $

have global maxima and minima in the xy -plane?

=== Solution

The, range for $f$ is $[0, oo)$ as $abs(x)$ and $abs(y)$ grows arbitrarily large, and $f(x,y)->oo$

So global minima at $f(x,y) = 0$

And no global maxima

== Problem 4
Suppose that $z = f(x,y) = x sin(y)$, where $x=t^2$ and $y = 2t+1$

Compute $f'(x,y)$ directly and using the chain rule.

=== Solution

$ partial/(partial t)f(x,y) = (partial f)/(partial x) dot (partial x)/(partial t) + (partial f)/(partial y)dot (partial y)/(partial t) $

Then

$ partial/(partial t)f(x,y) = sin(y)dot 2t + x cos(y) dot 2 $

Then substitute $x$ and $y$

$ partial/(partial t)f(x,y) = 2 sin(2t+1)dot t + 2t^2 cos(2t+1) $


== Problem 5
If $f , g , h$ are differentiable and if $z = f (x, y )$, with $x = g (u, v )$ and $y = h(u, v )$

What is $(∂z)/(∂u)$ and $(∂z)/(∂v)$ ?
=== Solution

$ (partial z)/(partial u) = (partial z)/(partial x) dot (partial x)/(partial u) + (partial z)/(partial y)dot (partial y)/(partial u) $

$ (partial z)/(partial v) = (partial z)/(partial x) dot (partial x)/(partial v) + (partial z)/(partial y)dot (partial y)/(partial v) $
#pagebreak()

= Lecture 8
== Problem 1
Show that if $f$ is differentiable with the local linearization


=== Solution

$ L(x) = f(a)-sum_(i=1)^n m_i (x_i-a_i) $
Then
$ m_i = (partial f)/(partial x_i)(a) $

since $ f(x) approx L(x) = f(a)+sum_(i=1)^n (partial f)/(partial x_i) (x_i-a_i) $

== Problem 2
Consider the function
$ f(x,y) = sqrt(x^2+y^2) $

is f differentiable at the origin?
=== Solution

To check this
$ f_x (x,y) = lim_(h->0) (f(x+h,y)-f(x,y))/h $
$ f_x (0,0) = lim_(h->0) (f(h,0)-f(0,0))/h = lim_(h->0) sqrt(h^2)/h =lim_(h->0) abs(h)/h= "DNE" $

$ f_y (x,y) = lim_(h->0) (f(x,y+h)-f(x,y))/h $
$ f_y (0,0) = lim_(h->0) (f(0,h)-f(0,0))/h = lim_(h->0) sqrt(h^2)/h =lim_(h->0) abs(h)/h= "DNE" $

$f$ is *not* differentiable at the origin
#pagebreak()

== Problem 3
Consider the function
$ f(x,y) =x^(1/3)y^(1/3) $
Compute the partial derivatives at $(x, y ) = (0, 0)$. 

Is $f$ differentiable at $(0, 0)$?
=== Solution
To check this
$ f_x (x,y) = lim_(h->0) (f(x+h,y)-f(x,y))/h $
$ f_x (0,0) = lim_(h->0) (f(h,0)-f(0,0))/h = lim_(h->0) (h^(1/3)dot 0^(1/3))/h= lim_(h->0) 0/h =0 $

$ f_y (x,y) = lim_(h->0) (f(x,y+h)-f(x,y))/h $
$ f_y (0,0) = lim_(h->0) (f(0,h)-f(0,0))/h = lim_(h->0) (0^(1/3) dot h^(1/3))/h= lim_(h->0) 0/h =0 $

Note that to prove differentiability f must also be continuous on a small disk centered at the point a (local linearization)

For differentiability:
$ lim_(h->0) E(x)/norm(h) =0 $

Lets do that

$ lim_((h_1,h_2)->(0,0)) (f(x)-L(x))/sqrt(h_1^2+h_1^2) $

$ lim_((x,y)->(a,b)) (f(x)-L(x))/sqrt(h_1^2+h_1^2) $

Since 
$ L(0,0)= f(0,0) + f_x (0,0) (x-0)+ f_y (0,0) (y-0) = 0+0x+0y=0 $ 
then

$ lim_((x,y)->(0,0)) (x^(1/3)y^(1/3))/sqrt((x)^2+(y)^2) = "DNE" $
// limit with maple

$f$ is *not* differentiable at $(0,0)$
#pagebreak()

== Problem 4
Show that the function $f (x, y ) = ln(x^2 + y^2)$ is differentiable everywhere in its domain.
=== Solution

The domain of $f$ is $(x,y) in RR^2 | (x,y)!= (0,0)$

If the partial derivatives, $f_x$ and $f_y$ , of a function $f$ exist and are continuous on a small disk centered at the point $a$, then $f$ is differentiable at $a$.

Because of this, we want to show that $f_x$ and $f_y$ both exist and are continous on the whole domain

$ f_x (x,y) = (2x)/(x^2+y^2), quad f_y (x,y) = (2y)/(x^2+y^2)  $

These are continuous on th whole domain, since $(x,y)!= (0,0)$

== Problem 5
Use the definition of the definite integral to compute
$ integral_a^b c dif x $

=== Solution

The definition of the definite integral is:

$ integral_a^b f(x) dif x =lim_(n->oo) sum^n_(k=1) f(x_k^*)Delta x $

Where $x_k =a+k dot Delta x, Delta x=(b-a)/n$ and $x_k^* in [x_(k-1), x_k ]$

So then $ integral_a^b c dif x =lim_(n->oo) sum^n_(k=1) c (b-a)/n = lim_(n->oo) n dot  c dot (b-a)/n = c(b-a) $

#pagebreak()

== Problem 6
Use the definition of definite integral to simplify
$ integral_a^b c f(x) dif x quad"for a constant c." $



=== Solution
The definition of the definite integral is:
$ integral_a^b f(x) dif x =lim_(n->oo) sum^n_(k=1) f(x_k^*)Delta x $

Where $x_k =a+k dot Delta x, Delta x=(b-a)/n$ and $x_k^* in [x_(k-1), x_k ]$
$ integral_a^b c f(x) dif x =lim_(n->oo) sum^n_(k=1) c f(x_k^*)Delta x $
$ lim_(n->oo) c dot sum^n_(k=1)  f(x_k^*)Delta x = c dot lim_(n->oo) sum^n_(k=1) f(x_k^*)Delta x = c dot integral_a^b f(x) " "d x   $

== Problem 7
Use the definition of definite integral to show that
$ integral_a^b f(x)+g(x) dif x = integral_a^b f(x) dif x +integral_a^b g(x) dif x  $

=== Solution
#align(center)[
  $
  integral_a^b f(x)+g(x) dif x &= lim_(n->oo) sum^n_(k=1) f(x_k^*)+g(x_k^*) Delta x\
  &= lim_(n->oo) sum^n_(k=1) f(x_k^*) Delta x + lim_(n->oo) sum^n_(k=1) g(x_k^*) Delta x\
  &= integral_a^b f(x) dif x +integral_a^b g(x) dif x
  
  $
]

#pagebreak()

== Problem 8
Evaluate $ integral^3_0 2x dif x $
as a limit of Riemann sums.

=== Solution

#align(center)[$
 integral^3_0 2x dif  x &=lim_(n->oo) sum^n_(k=1) f(x_k^*) Delta x \

  &=lim_(n->oo) sum^n_(k=1) 2x  dot (3)/n \
  $#align(left)[Since $x = k Delta x = (3k)/n$]$\
  &=lim_(n->oo) sum^n_(k=1) (6k)/n  dot (3)/n\
  &=lim_(n->oo) sum^n_(k=1) (18k)/n^2\
  &=lim_(n->oo) (18)/n^2 dot  sum^n_(k=1) k\
  &=lim_(n->oo) (18)/n^2 dot n(n+1)\
  &=lim_(n->oo) (18(n+1))/(2n)\
  &=lim_(n->oo) (9(n+1))/(n)\
  &=lim_(n->oo) 9+1/n =9\
$]

== Problem 9
Compute
$ integral^3_0 2x" "d x $
Using the fundamental theorem of calculus.

=== Solution

$ integral^b_a f(x)dif  x x  = F(b)-F(a) $
$ integral^3_0 2x dif  x x = F(3)-F(0) $
Since $ integral 2x = x^2 $
$ F(3)-F(0) = 9 $


== Problem 10

Find
$ d/(d x) [integral_2^x cos(t)dif  x] $

=== Solution

Since $ d/(d x) [integral_a^x g(t)dif x] = g(x) $

by second FTC


then $ d/(d x) [integral_2^x cos(t)dif x] = cos(x) $

== Problem 11

Let 
$ g(x) = integral^x_1 sqrt(1+t^2) dif x $
- Find $g'(x)$
- Find $g(x^3)$
- Compute $d/(d x) g(x^3)$.

=== Solution
- Find $g'(x)$
  Since 
$ d/(d x) [integral_a^x g(t)dif x] = g(x) $

$quad$by second FTC


$quad$then $ d/(d x) [integral_1^x sqrt(1+t^2) dif x] = sqrt(1+x^2)  $
#pagebreak()

- Find $g(x^3)$
  
I want to express $ g(x^3) = integral^(x^3)_1 sqrt(1+t^2) dif x $

- Compute $d/(d x) g(x^3)$.

  Since 

$ d/(d x) [integral_a^x g(t)dif x] = g(x) $

$quad$by second FTC

$quad$and 
$ g(x^3) = integral^(x^3)_1 sqrt(1+t^2) dif x $

$quad$then $ d/(d x)g(x^2) = d/(d x) [integral_1^(x^3) sqrt(1+t^2) dif x] $

$quad$This is done by chainrule $ d/(d t)f(x) = (d f)/(d x) dot (d x)/(d t) $

$quad$So

$ d/(d x) [integral_1^(x^3) sqrt(1+t^2) dif x] $
$ d/(d x) [integral_1^(x^3) sqrt(1+t^2) dif x] = sqrt(1+x^6) dot 3x^2 $

#pagebreak()

= Lecture 9

== Problem 1
Compute $ integral^(oo)_0 e^(-5x) dif x $

=== Solution

Since $ integral^(oo)_a f(x) dif x = lim_(b->oo)integral^(b)_a f(x) dif x $
So $ integral^(oo)_0 e^(-5x) dif x =lim_(b->oo)integral^(b)_0 e^(-5x) dif x $

By first FTC
$ lim_(b->oo)F(b)-F(0) = lim_(b->oo) -(e^(-5b))/5 - (-(e^(-5(0)))/5) = 0-(-1/5) = 1/5 $

== Problem 2
Investigate the convergence of

$ int(0,2,1/(x-2)^2 dif x). $

=== Solution
By looking at the graph i can see that has assymtotic behvaior close to $x=2$

This makes sense, because $f$ is undefined at $x = 2$. Therfore
$ lim_(b->2) int(0,b,1/(x-2)^2 dif x) $

Then using first FTC
 $ lim_(b->2) int(0,b,1/(x-2)^2 dif x) = lim_(b->2) F(b)-F(0) $

Since $F(x) = -1/(x - 2)$

 $ lim_(b->2) F(b)-F(0) = lim_(b->2) (-1/(b - 2) -(-1/(- 2))) = lim_(b->2) (-1/(b - 2) - 1/2)   $

Since $f$ diverges from $2^-$

 $ lim_(b->2^-) (-1/(b - 2) - 1/2) = +oo $

== Problem 3
Compute $ int(2,3,2x sin(x^2) dif x) $
Hint: Let $u=x^2$, then $d u=2x dif x$. After substitution, the integral becomes
$ int(u(a),u(b),sin(u) dif u) = int(2^2,3^2,sin(u) dif u) $

=== Solution

$ int(3,9,sin(u) dif u) = F(9) - F(3) = cos(9)-cos(3) approx 0.26 $

== Problem 4
Evaluate $ int(0,1,x(x-1)^4 dif x) $


=== Solution
Let $u=x$, then $d u=x dif x$. After substitution, the integral becomes
$ int(u(a),u(b),u^4 dif u) = int(0-1,1-1,u^4 dif u) $

Then
$ int(-1,0,u^4 dif u) = F(0)-F(-1) $
Since $ F(x) = x^5/5 $
$ F(0)-F(-1) = 0^5/5-(-1)^5/5 = 1/5 $
#pagebreak()

== Problem 5
Evaluate $ integral sin^2(x) dif x $


=== Solution
$ integral sin^2(x) dif x = -(sin(x)cos(x))/2 + x/2 + C $
// done with maple
== Problem 6
Evaluate $ int(0,1,x e^x dif x) $

=== Solution

For definite integrals
$ int(a,b,f(x)g'(x) dif x) = (f(b)g(b)-f(a)g(a)) - int(a,b,f'(x)g(x) dif x) $

For this.
$ int(0,1,x e^x dif x) = (1 dot e^1 - 0 dot e^0 - int(0,1,1 dot e^x dif x) $

$ int(0,1,x e^x dif x) = e - int(0,1,e^x dif x) = e- (e^1-e^0) = 1 $
#pagebreak()

== Problem 7 
Evaluate $ int(1,2,ln(x) dif x) $


=== Solution
Since doint $integral ln(x) dif x$ is not very intuative

Let
$ int(1,2,ln(x) dif x) = int(1,2,ln(x) dot 1 dif x) $

Now for definite integrals
$ int(a,b,f(x)g'(x) dif x) = (f(b)g(b)-f(a)g(a)) - int(a,b,f'(x)g(x) dif x) $

For this.
$ int(1,2,ln(x) dot 1 dif x) = (ln(2) dot 2 - ln(0) dot 0)- int(1,2, 1/x dot x dif x) $

Since $ int(1,2, 1/x  dot x dif x) = int(1,2, 1 dif x) = 0 $

$ int(1,2,ln(x) dot 1 dif x) = 2ln(2) approx 1.39 $

== Problem 8
Let $M_(i j)$ and $L_(i j)$ denote the max and min of $f$ on the $i j$-th rectangle. If $(u_(i j) , v_(i j) )$ is any point in the $i j$-th subrectangle:

$ <= summ(i=1,n,summ(j=1,m,f(u_(i j),v_(i j)) Delta x Delta y)) $

- How does $summ((i,j), ,  f (u_(i j) , v_(i j) )Delta x Delta y)$ look geometrically?
- What happens as $n, m -> oo$?

=== Solution
- How does $summ((i,j), ,  f (u_(i j) , v_(i j) )Delta x Delta y)$ look geometrically?
  
  An area by $f$ aproximated by rectangles size $Delta x dot Delta y$

- What happens as $n, m -> oo$?

  As$n, m -> oo$ each rectangle gets increasingly smaller, and the numeber of rectangles because larger. So large that the approximation becomes the area

== Problem 9
If $g (x, y ) = 1$ what is $integral_R g dif  A$



=== Solution

Since $ integral_R 1 dif  A= integral_a^b integral_c^d 1 dif y dif x = (b-a) dot (c-d) $

Then

$ integral_R g dif  A = "Areal of "R $

#align(center)[#image("/assets/image-40.png",width: 30em)]

== Problem 10
How do we compute the average value of $f$ over $R$?

=== Solution
As for single variable intergration
$ f_"avg" = 1/(b-a) dot int(a,b,f(x) dif x) ==> "Area"/"Length of interval" $

Then for two variables

$ f_"avg" = 1/(integral.double_R 1 dif) dot integral.double_R f(x) dif A ==> ("Volume under surface "f(x,y))/"Area of R" $


#pagebreak()

== Problem 11
Let $f(x,y) =  x^2 y, R= [0,1] times [0,1]$. 

Compute $integral_R f dif A$
$ int(0,1,int(0,1,x^2 y  dif x dif y)) $


=== Solution
First $ int(0,1,x^2 y  dif x) = (1^3dot y)/3  = y/3 $
Then
$ int(0,1,y/3 dif y) = 1^2/6 = 1/6 $


== Problem 12
What if we switch the order of integration?

$ int(0,1,int(0,1,x^2 y  dif y dif x)) $
=== Solution

First $ int(0,1,x^2 y  dif y) = (x^2y^2)/2  = x^2/2 $
Then
$ int(0,1,x^2/2 dif y) = 1^3/6 = 1/6 $

*Same answer*
#pagebreak()

== Problem 13
A building is $8 "m"$ wide and $16 "m"$ long. It has a flat roof $12 "m"$ high at one corner, and $10 "m"$ high at each adjacent corner. 

Find the volume of the building.

=== Solution

I assume that the highest corner of the roof is on point $P(0,0,12)$

Based on the height of each corner i can know that the surface of the roof, has vector $a = (8,0,-2)$ and $b = (0,16,-2)$

i can the find $a(x-x_0)+b(y-y_0)+c(z-z_0)=0$ with a normal vector $N$.

Now by using cross product $N = v times w $
$ N = v times w = vec(32,16,128) $

with point $P(0,0,12)$ the plane becomes

$ 32(x-0)+16(y-0) +128(z-12) = 0 $
$ 32x + 16y + 128z - 1536 = 0 $
$ z = -x/4 - y/8 + 12 $

Then i can $ int(0,8,int(0,16,-x/4 - y/8 + 12 dif y dif x)) $

Since $ int(,,-x/4 - y/8 + 12 dif y) = -1/4x y - 1/16y^2 + 12y $

Then $ F(16)-F(0) = -4x + 176 $

Now since $ int(,,-2x^2 + 176x dif x) = -2x^2 + 176x $

Then
$ F(8)-F(0) = 1280 $

Volume of the building is *$1280 " m"^3$*
#pagebreak()

== Problem 14
The density at $(x, y )$ of a triangular metal plate is $delta (x, y )$. Express its mass as an iterated integral.
#align(center)[#image("/assets/image-41.png",width: 15em)]

=== Solution

We wish to express $ m = integral.double_R delta(x,y) dif A $

Where $R = {(x,y) : 0<=x<=1, 0<=y<=2-2x} $

Hence
$ m= int(0,1,int(0,2-2x,delta(x,y)) dif y dif x) $


#pagebreak()

== Problem 15
A city occupies a semicircular region of radius $3 "km"$ bordering the ocean. Find the average distance from points in the city to the ocean.


=== Solution

We can use polar coordinates where $0<=3$ and $0<=theta <=pi$

Since it is a semicircle of radius $R$, the area is $(1/2)pi R^2 = (1/2)pi(3²) = (9/2)pi$.

We know that $f_"avg" = "Distance"/"Area of R"$

Hence

$ f_"avg" = display(integral.double "distance" dif A) / ("Area of R") = display(int(0,pi,int(0,R, r^2 dif r dif theta)))/((1/2)pi R^2) $

Since $int(0,R,r^2 dif r) = R^3/3 $

$ f_"avg" = display(int(0,pi,R^3/3 dif theta))/((1/2)pi R^2) $

Since $int(0,pi,R^3/3 dif theta) = R^3pi/3$

Then

$ f_"avg" = (R^3pi/3)/((1/2)pi R^2) $


Now substitute $R$ for $3$.

$ f_"avg" = ((3^3pi)/3)/((3^2pi)/2) = (9pi)/(9/2pi) = 2 $
#pagebreak()

== Problem 16
Compute
$ integral_W (1+x y z) dif V $
over the cube $0 <= x, y , z <= 4$.

=== Solution

We know that $W = {(x,y,z) : 0<=x,y,z<=4}$

This means that $W = [0,4] times [0,4] times [0,4]$

And therefore

$  integral_W (1+x y z) dif V = integral.triple_W (1+x y z) dif V = int(0,4,int(0,4,int(0,4,(1+x y z) dif x dif y dif z)))  $

Since $integral (1+x y z) dif x =1/2x^2y z + x $

Then $ F(4)-F(0) = 8y z + 4 $

And $  integral_W (1+x y z) dif V = int(0,4,int(0,4,(8y z + 4)dif y) dif z)  $

Since $integral (8y z + 4) dif y =4y^2z + 4y $

Then $ F(4)-F(0) = 64*z + 16 $

And $  integral_W (1+x y z) dif V = int(0,4,64z + 16 dif z)  $

Since $integral (64z + 16) dif z =32z^2 + 16z $

Then $ F(4)-F(0) = 576 $

And $  integral_W (1+x y z) dif V = bold(576)  $


#pagebreak()

== Problem 17
Set up an iterated integral for the mass of a solid cone bounded by $z = sqrt(x^2+y^2)$ and $z = 3$, with density $delta (x, y , z) = z$.


=== Solution

The cone is given by $z = sqrt(x^2+y^2)$ and is cut of by the plane $z = 3$ meaning $z<=3$
#align(center)[#image("/assets/image-42.png",width: 25em)
$z=sqrt(x^2+y^2), quad "where" z<=3$]


Then $ m = integral.triple_W delta(x,y,z) dif V $

in this case this is:
$ integral.triple_W z dif V $

Because the of the cone, polar coordinates are easier to work with
$ x = r cos(theta), quad y = r sin(theta), quad underbrace(z = z, "cone is on "z), quad dif V = r dif z dif r dif theta  $

In this instance $theta$ is the whole circle around the z axis so $0<=theta<= 2pi$

To find $r$ we can use the definition of the circle at $z = 3$

Because of $3 = sqrt(x^2+y^2) => underbrace(x^2+y^2 = 9, "circle at "z=3)$ where $9 = r^2$, So $r = 3$

This means that $0<=r<=3$

#pagebreak()
To find z we look at the cones height from $z = 0$ to $z = 3$. 

For any $(r, theta)$ we know that $z = sqrt(x^2+y^2)=r$

This means that $sqrt(x^2+y^2) <=z<=3 quad ==> r<=z<=0$

Finally we can put it all together
$ int(0,2pi,int(0,3,int(r,3,delta(x,y,z)r dif z dif r dif theta))) = int(0,2pi,int(0,3,int(r,3,sqrt(x^2+y^2)r dif z dif r dif theta))) $


#pagebreak()

= Lecture 10
*No new problems*

= Lecture 11

== Problem 1
Evaluate $ int(0,1,x e^x^2 dif x) $
(Recall: set $u = x^2$, $dif u = 2x dif x$.)

=== Solution

When $u = x^2$ and $dif x = 2x$

Then $ int(0,1,x e^x^2 dif x) = int(0,1,1/2dot e^u dif u) $

Now
$ int(0,1,1/2dot e^u dif u)=F(1)- F(0) $

Since $integral 1/2 dot x^2 dif x = x^3/6$

$ F(1)- F(0) = 1^3/6 = 1/6 $
Making $ int(0,1,x e^x^2 dif x) = 1/6 $

#pagebreak()

== Problem 2
Let $D_* = [0, 1] times [0, 2pi]$ with coordinates $(r , θ)$ and
$ T(r,theta) = (r cos(theta), r sin(theta)) $ 

What is $T(D_*)$?

=== Solution

$D_*$ describe all points with distance to the center $(0,0)$  of $0<="distance"<=1$

As a transformation $T$ this can be described as a disk with center $(0,0)$ 

This means that $sqrt(x^2+y^2)<1$

$T(D_*) = {(x,y): sqrt(x^2+y^2)<=1}$


== Problem 3
$T(u,v) = ((u+v)/2,(u-v)/2)$ on $D_* = [-1,1]^2$.

Find $T(D_*)$


=== Solution

$D_*$ describes all points within a $2times 2$ square with center in $(0,0)$

The transformation $T(D_*)$ transforms it to a stilted square with a digonal of $2$

$T(D_*)={(x,y): }$


== Problem 4
Use polar coordinates to compute
$ int(oo,oo,e^(-x^2)dif x) $
Standard trick: square the integral and use polar coordinates to obtain $sqrt(pi)$

=== Solution

By squaring $int(oo,oo,e^(-x^2)dif x)$

I can fit this equation to use polar coordinates

$ (int(oo,oo,e^(-x^2)dif x))^2 = int(oo,oo,int(oo,oo,e^(-x^2) e^(-y^2) dif x) dif y), quad "where" x = y $


Now $x = r cos(theta)$ and $dif x dif y = r dif r dif theta$
$ int(0,2pi,int(0,oo,r e^(-r^2(cos(theta)+sin(theta))^2)dif r)dif theta)=int(0,2pi,int(0,oo,r e^(-r^2)dif r)dif theta) $ 
#pagebreak()

Then use u-subsitution where $u = r^2$ and $dif u = 2r dif r => 1/2 dif u = r dif r$
$ int(0,2pi,int(0,oo,e^(-u)dif u)dif theta) $

since $integral e^(-u) dif u = -e^(-u)$
$ F(oo)-F(0) = -e^(-oo) -(-e^(0)) = 1/2 (0-(-1)) =1/2 $

Then 
$ int(0,2pi,1/2 dif theta) = 1/2 2pi = pi $

Then $ (int(oo,oo,e^(-x^2)dif x))^2 = pi quad => quad int(oo,oo,e^(-x^2)dif x) = sqrt(pi
) $


== Problem 5
Let $x = r cos θ, y = r sin θ$ and $z = z$. Let $W_*$ consists of points in the form $(r , θ, z)$ where $0 <= r <= 1, 0 <= θ <= 2π, 0 <= z <= 1$. 

Find the image set and compute its Jacobian.


=== Solution

This transformation

$ T(x,y,z) = (r cos(theta), r sin(theta), z)) $

Transforms $W_* = {(r,theta,z):0<=r<=1,0<=theta<=2pi,0<=z<=1}$ (a cylinder, $r=1,h = 1$)

To $V = {(x,y,z):-1<=x<=1,-1<=y<=1,0<=z<=1}$ (a cuboid, $w=2, d=2,h = 1$)

The Jacobian for 3D is:
$ J(r,theta,z) = mat((partial x)/(partial r),(partial x)/(partial theta),(partial x)/(partial z);(partial y)/(partial r),(partial y)/(partial theta),(partial y)/(partial z);(partial z)/(partial r),(partial z)/(partial theta),(partial z)/(partial z)) = mat(cos(theta),sin(theta),0;-r sin(theta),r cos(theta), 0;0,0,1) $

#pagebreak()

== Problem 6
Let $x = rho sin(phi) cos(theta), y = rho sin(phi) sin(theta) "and" z = rho cos(phi)$. Compute its Jacobian and use this to derive the change of variable spherical cooridnate formula.

=== Solution
The Jacobian for 3D is:

$ J(rho,phi,theta) = mat((partial x)/(partial rho),(partial x)/(partial phi),(partial x)/(partial theta);(partial y)/(partial rho),(partial y)/(partial phi),(partial y)/(partial theta);(partial z)/(partial rho),(partial z)/(partial phi),(partial z)/(partial theta)) = mat(sin(phi)cos(theta),rho cos(phi)cos(theta),-rho sin(phi)sin(theta);sin(phi)sin(theta),rho cos(phi)sin(theta), -rho sin(phi)cos(theta);cos(phi),-rho sin(phi),0) $

Now i can take the determinant
$ abs(J(rho,phi,theta)) = rho^2sin(phi) $

This means that any shape that is transformed by $ T(x,y,z) = (rho sin(phi)cos(theta),rho sin(phi)sin(theta),rho cos(phi)) $

Is then also scaled by $rho^2sin(phi)$


