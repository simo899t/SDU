#import "@local/tempst:0.1.0": *

#show: exercise.with(
  title: "Homework 1",
  //subtitle: "",       // optional
  course: "AI503: Calculus",
  author: "Simon Holm",
  date: "2025",
  // group: "Group 4",                    // optional
  //supervisor: "Prof. firstname lastname",        // optional
  university: "University of Southern Denmark",
)
= Problem 1
What are the domain and range of the following functions?

#enum([
  $g(x)=1/x$
], [
  $f(x) = sqrt(x-3)$
])

== Solution
#enum([
  since $g(x)="DNE"$ when $x=0$ 

  Therefore the domain is $x in RR\\{0}$
], [
  I assume that $f(x)$ is only defined in non-complex numbers (not $i$)
  
  The domain is therefore: $x in [3,infinity)$
])

= Problem 2
Let $ f(x)=cases(x-5 quad & x>=1",", -3x & x<1 ) $
Evaluate $lim_(x->1)f(x)$
== Solution 
To evaluate the limit check right hand limit $x->1^+$ and left hand limit $x->1^-$
$ lim_(x->1^+) f(x)-> 1-5 = -4 $ 
$ lim_(x->1^-) f(x)-> -3dot 1 = -3 $
And since $-4!=-3$ the limit $lim_(x->1)f(x)$ *does not exits*
= Problem 3
Compute: $ lim_(x->2)(x^2-4)/(x-2) $
== Solution 
On the surface, this looks complicated but since

$ (x^2-4)/(x-2) = (x^2+2x-2x-4)/(x-2)=((x-2)(x+2))/(x-2) = x+2 $

So actually $ lim_(x->2)x+2 = 4 $
= Problem 4
Evaluate $ lim_(x->0)(sin(5x))/x $
== Solution 
Since $sin(5 dot 0)/0$ is undefined. Use L'Hôpital's rule that is

#theorem(title: "Theorem: L'Hôpital's Rule")[
  Let $f$ and $g$ be functions that are differentiable on an open interval $I$ containing 
$a$, except possibly at 
$a$ itself

Assume that $ lim_(x->a) f(x) = 0 quad "and" quad lim_(x->a) g(x) = 0 $
or that $ lim_(x->a) f(x) = plus.minus infinity quad "and" quad lim_(x->a) g(x) = plus.minus infinity $
Then also assume that $g'(x)!=0, forall x != a$

The the limit is $ lim_(x->a) f(x)/g(x) =lim_(x->a) (f'(x))/(g'(x)) $
]

by theorem 
$ lim_(x->0)(sin(5x))/x = lim_(x->0)(cos(5x))/1 = (5dot cos(0))/1=5 $
#pagebreak()

= Problem 5
Determine whether $ f(x)=cases(sin(x)/x quad & x != 0",",1 & x = 0) $
is continuos at $x=0$


== Solution 
Yes it is, since $f(0) = 1$


= Problem 6
Find all points where $g(x)=(x^2-1)/(x-1)$
is continuos.



== Solution 
Since $g(0)$ is undefined

This is continuos on all points exept $1$, so $(-infinity,1) union (1, infinity)$
#pagebreak()

= Problem 7
Differentiate

#enum([
  $f(x)=x^3 sin(x)$
], [
  $g(x)=ln(sqrt(1+x^2))$
])


== Solution 
$d/(d x)a x^n +b$

#enum([
  Use product rule
  $ d/(d x)(f(x)g(x))=f'(x)g(x)+f(x)g'(x) $
  so..
  $ d/(d x)f(x)=3x^2 dot sin(x) + x^3 dot cos(x) $
], [
  Use chain rule
  $ d/(d x)(f(g(h(x)))) = f'(g(x))dot (g'(h(x))dot h'(x)) $
  so..
  $ d/(d x)g(x)= 1/(sqrt(1+x^2))dot (1/2(1+x^2) dot 2x) $
])

#pagebreak()
= Problem 8
Find the equation of the tangent line to
$ y = e^(2x) $
at $x=0$
== Solution 
Use chainrule again
$ d/(d x)e^(2x)=e^(2x)dot 2 $
at $x = 0$ 
$ a=2 $
This is the slope of the tangent

Now find the tangent intersect at $x=0$
$ y(0) = e^(2dot 0) =1 $

So point $(0,1)$ is the intersect

Now for the tagentline $y-y_0=a(x-x_0)$

This equals to $ y-1=2(x-0) ==> bold(y=2x+1) $

= Problem 9
Consider $f(x)=(x^2-4)^7$. Find and classify all local extrema
== Solution 
Use chainrule
Find $f'(x)=0$
$ 0=7(x^2-4)^6 dot 2x = 14x(x^2-4)^6 $
Use 0 rule. either $14x$ or $(x^2-4)^6$ equals 0 for the result to equal 0

So..
$ 14x(x^2-4)^6 =>x=0 quad "or" quad x^2-4 => x = plus.minus 2 $

So critical points are $=-2,0,2$.

#pagebreak()
= Problem 10
Find the global max and min of $f(x)=x^3-7x+6$ on $-4<=x<=2$.

== Solution 

For this find $f'(x)$ and choose max and min.

$ f'(x)=3x^2-7 $
Now solve for $f'(x) = 0$
$ x = plus.minus sqrt(7/3)  $

So see which of these are the max and min
$ f''(x)=6x => 6 dot sqrt(7/3) >0 quad "and" quad 6 dot (-sqrt(7/3)) <0 $


