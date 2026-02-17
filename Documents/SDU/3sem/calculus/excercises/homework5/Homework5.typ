#import "@preview/cetz:0.2.2"
#import "@preview/cetz-plot:0.1.0": plot

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
#set math.equation(numbering: none)

#let evaluated(expr, size: 100%) = $lr(#expr|, size: #size)$

// Title page
#align(center)[
  #text(size: 18pt, weight: "bold")[Exercises 5]
  
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
1. Evalueate 
$ integral^3_0 (3x+7)d x $
as a limit of Riemann sums.

== Solution

We know that the Riemann sums for a single variable is defined as $ integral^b_a f #h(0.1cm) d x =lim_(n->infinity)sum^n_(k=1)f(x_k) Delta x $ 
We know that for each $x$: $x_k = a + k Delta x $ and that $Delta x = (b-a)/n = (3-0)/n = 3/n$

So therefore $ x_k = (3k)/n $
#align($
integral_a^b f#h(0.1cm)d x &= lim_(n->infinity)sum^n_(k=1) (3x+7) dot 3/n
\
&= lim_(n->infinity)sum^n_(k=1) (3dot (3k)/n+7) dot 3/n
\
&=lim_(n->infinity)sum^n_(k=1) ((27k)/n^2+21/n)
\
&=lim_(n->infinity)(sum^n_(k=1)(27k)/n^2+sum^n_(k=1)(21)/n)
\
&=lim_(n->infinity)((27)/n^2sum^n_(k=1)k+(21)/n sum^n_(k=1)1)
$)
We know the formulas: $sum^n_(k=1) = (n(n+1))/2$ and $sum^n_(k=1)1) = n$

So...
#align($
&=lim_(n->infinity)((27)/n^2 (n(n+1))/2+(21)/n n)
\
&=lim_(n->infinity)((27(n+1))/2n+21)
\
&=lim_(n->infinity)((27(n+1))/2n+21)
\
&=lim_(n->infinity)((27n)/(2n)+27/(2n)+21)
\
&=lim_(n->infinity)(27/2+27/(2n)+21) = 27/2+0+21 = 27/2+42/2 = 34.5
$)



#pagebreak()
= Problem 2
Why does the Fundamental Theorem of Calculus not imply that
$ integral^1_(-1)1/(x^2)d x=evaluated(-1/(x))_(-1)^1 =(-1/1-(-1/(-1)=-2)) $

== Solution
Lets use the fundemental theorem of falculus on this.
$ integral^b_a f(x)#h(0.1cm)d x = F(b)-F(a) $
since $1/(x^2) = x^(-2)$, then $integral 1/(x^2) = x^(-2+1)/(-2+1)=x^(-1) = x^(-1)/(-1)=-1/x$

then
$ integral^1_(-1)1/(x^2)d x=evaluated(-1/(x))_(-1)^1 =(-1/1-(-1/(-1)=-2)) $
This does not make sense because $f$ has a assymtote.
$ f(x) -> oo "as" x->0^- "or " x->0^+ $ 

#pagebreak()
= Problem 3
Find a function $g(x)$ such that $g'(x)$ = $sqrt(1+x^2)$ and $g(2) = 0$.


== Solution

$ g(x) = integral sqrt(1+x^2)" "d x $
$ integral sqrt(1+x(u)^2)dot (d u)/(d x) " "d x $
$ integral sqrt(1+sinh^2(u))dot cosh(u) " "d x  $
$ integral sqrt(cosh^2(u))dot cosh(u) " "d x  $
$ integral cosh(u) dot cosh(u) " "d u $
$ integral cosh^2(u) " "d u $
since $cos^2(x) = 1/2(cos(u)-1)$
$ integral cosh^2(u)= 1/2integral (cosh(u)-1)" "d u $
$ 1/2integral cosh(u)-integral 1" "d u $


= Problem 4
Find $d/d x integral^3_(x^2)sin(t)/t" "d t$.

== Solution

we know that
$ d/(d x) integral^3_(x^2)sin(t)/t" "d t = -d/(d x) integral^(x^2)_3sin(t)/t" "d t $

Use u substitution

$ u=x^2 "     " d u =2x $
then 
$ -d/(d u)(integral^u_3sin(t)/t" "d t)dot (d u)/(d x) $
since $ d/(d x)integral^x_c f(t)" "d t = f(u)$
$ sin(t)/t" "d t dot (d u)/(d x) = sin(x^2)/x^2 2x = (2sin(x))/x  $
$  $

= Problem 5
True or False? Give reasons for your answer.

#enum(
  numbering: "(a)",
  [The double integral $integral_R f" "d A$ is always positive],
  [If $f(x,y) =k$ for all points in a region $R$, then $integral_R f " "d A=k dot "Area"R$], 
  [If $integral_R f " "d A = 0$ then $f(x,y) = 0$ at all points of $R$]
)
== Solution
#enum(
  numbering: "(a)",
  [Yes, with a function $f$ inside the $integral$ sign anything is possible],
  [Yes, since f is a constant $k$, then $k integral_R f" "d A$], 
  [No, it can be 0 but doesnt have to.]
)

= Problem 6
Sketch the region of integration
#enum(
  numbering: "(a)",
  [$integral^2_0 integral^(y^2)_0 y^2x""d y ""d x$],
  [$integral^(pi)_0 integral^(x)_0 y sin(x)""d y ""d x$]
)


== Solution
#enum(
  numbering: "(a)",
  [We know that $0<=x<=y^2$ and that $0<=y<=2$
  #image("plot6a.png")
  #colbreak()
  #colbreak()],
  [We know that $0<=y<=x$ and that $0<=x<=pi$
  #image("plot6b.png")]
)

= Problem 7
A city occupies a semicircular region of radius 3 km bordering on the ocean. Find the
average distance from points in the city to the ocean.


== Solution
This was done in class

#pagebreak()
= Problem 8
Sketch the region of integration for the iterated integral
$ integral^6_0 integral^(2)_(pi/3)x sqrt(y^3+1)""d y""d x $
Then reverse the order of integration.


== Solution
We know that $x/3<=y<=2$ and that $0<=x<=6$
#image("plot8a.png", width: 35em)

#pagebreak()
To reverse the orde, first swich the boundarie $x/3$ beacuse it depends on x.
so... $ y = x/3 <=> x=3y $
Now we know that $0<=y<=2$ and that $0<=x<=3y$
#image("plot8b.png")
#pagebreak()

= Problem 9
Find the area of the crescent-moon shape with circular arcs as edges and the dimensions
shown in figure below.
#align(center)[
  #image("moon.png", width: 30em)
]

== Solution
We know that 
$ A = A_("small")-A_("segm") $
$ A = 1/2pi 4^2-A_("segm") $
So we have to find find the segment of the bigger circle

We know that $A_"seg"=1/2theta R^2- 1/2a d$. (from $$)

and since that $R^2 = d^2 +(a/2)^2 = (R-h)^2+(a/2)^2$
$ R^2 = R^2-2h R+h^2+(a^2)/4 ==> R=1/(2h)(h^2+(a^2)/4)=1/2(h+(a^2)/(4h)) $
since $sin(theta/2) = (a/2)/R$
$ theta = 2sin^(-1)(a/(2R)) $
$ A_"seg" = sin^(-1)((a h)/(h^2+(a^2)/(4)))dot (h/2+(a^2)/8h)-1/2a((a^2)/(8h)-h/2) $
$ A = 1/2pi 4^2-12.68 $