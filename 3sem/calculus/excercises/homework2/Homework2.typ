#set document(title: "Homework 2", author: "Simon Holm")
#set page(
  paper: "us-letter",
  margin: (left: 3cm, right: 3cm, top: 2cm, bottom: 2cm)
)
#set text(font: "New Computer Modern", size: 11pt)
#set math.equation(numbering: "(1)")
#set heading(numbering: "1.")

// Set matrix delimiters to square brackets globally
#set math.mat(delim: "[")

#let evaluated(expr, size: 100%) = $lr(#expr|, size: #size)$

// Custom box function for easy use
#let box(content) = block(
  fill: rgb("#f0f0f0"),
  inset: 10pt,
  radius: 4pt,
  content
)
// Preset for alphabetical enumeration (a), (b), ...
#set enum(numbering: "(a)")

// Title page
#align(center)[
  #text(size: 18pt, weight: "bold")[Exercises 2]
  
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

A company's profit function is given by
$ P(x)= -2x^2+12x-5 $
where $x$ is the number of units produced. Find the production level that maximizes profit.



== Solution
Find $f'(x)$

$ f'(x) =-4x+12 $

Then $ 0 = -4x+12 => x = 12/4  = 3 $

Since $f''(x)=-4 <0$ graph is a concave down everywhere

Because of this $x=3$ must be the maximum
 
= Problem 2

Let $arrow(u) = vec(1,2,3)$ and $arrow(v) = vec(4,-1,2)$. Compute:
#enum([
  $arrow(u) + arrow(v)$
], [
  $2arrow(u) - 3arrow(v)$
], [
  $arrow(u) bullet arrow(v)$
])

== Solution
#enum([
  $vec(1,2,3) + vec(4,-1,2) = bold(vec(5,1,5))$
  #v(1em)
], [
  $2vec(1,2,3)-3vec(4,-1,2) = vec(2,4,6)-vec(12,-3,6) = bold(vec(-10,7,0))$
  #v(1em)
], [
  $arrow(u)bullet arrow(v) = 1 dot 4 +2 dot (-1)+3 dot 2 =bold(8)$
])

= Problem 3

Find the length (norm) of the vector $arrow(w) = vec(2, -1, 2, 2)$. And normalize it to a unit vector.
== Solution

The length of vector is definde as
$ norm(arrow(w)) = sqrt(2^2+(-1^2)+2^2+2^2) = sqrt(13) $



To normalize $arrow(w)$, set the length to 1 by:
$arrow(w)_u = 1/sqrt(13) dot vec(2,-1,2,2)=vec(2/sqrt(13),-1/sqrt(13),2/sqrt(13),2/sqrt(13))$

Now $norm(arrow(w)) = sqrt((2/sqrt(13))^2+(-1/sqrt(13))^2+(2/sqrt(13))^2+(2/sqrt(13))^2) = 1 $

= Problem 4
  Let $a = (1, 0, -1)$ and $b = (2, 3, 1)$. Compute the angle between $a$ and $b$.


== Solution
Dot product is defined as $ a bullet b =norm(a)norm(b)dot underbrace(cos(theta),angle(a,b)) $
So. $ theta = cos^(-1)((a bullet b)/(norm(a)norm(b))) $

Now to find $theta$ between $a$ and $b$

$ theta = cos^(-1)((2 -1)/(sqrt(2)dot sqrt(14))) $

= Problem 5
Find a linear function whose graph is the plane intersects the $x y$-plane along the line $y=2x+2$ and contains the point $(1,2,2)$


== Solution
The plane equation in 3d:
$ a x + b y + c x = d $

Where $y =2x+2$ and $z$ is fixed to $z=0$ at 

So then $ a x + b (2x+2) + c(0) = a x + 2b x+b 2)=d $

This can be rewritten to $ (a+2b)x + b 2=d $

Since d is fixed: $a+2b = 0$, $a = -2b$ and $d = 2b$

Now to choose some arbitrary b-value $b = 1$ (for simplicity)

Then $ -2x+y +c z = 2 $

This plane is not fixed in the point yet, lets do that and find c to fix the plane to the point

$ (1,2,2) => -2(1)+2+c 2 = 2 => c 2 = 2 => c=1 $
Finally 
$ bold(-2x+y + z = 2) $

= Problem 6
Determine if $z$ is a function of $x$ and $y$:
#enum([
  $6x - 4y + 2z = 10$
], [
  $x^2 + y^2 + z^2 = 100$
], [
  $3x^2 - 5y^2 + 5x = 10 + x + y$
])

== Solution
#enum([
  #align($
  6x - 4y + 2z &= 10\
  2z &=-6x+4y+10\
  bold(z&=-3x+2y+5) quad checkmark
  $)
  #v(1em)
], [
  #align($
  x^2 + y^2 + z^2 = 100\
  z^2 = 100-x^2-y^2\
  bold(z=sqrt(100-x^2-y^2)) quad checkmark
  $)
  #v(1em)
], [
  #align($
  3x^2 - 5y^2 + 5x = 10 + x + y\
  5z = -3x^2-5y^2+10+x+y\
  bold(z = (-3x^2-5y^2+10+x+y)/5) quad checkmark
  $)
])



= Problem 7
Show that the following limit does not exits
$ lim_((x,y)->(0,0)) (x+y^2)/(2x+y) $
== Solution
Try different paths $x = 0$ and $y = 0$

First $x = 0$
$ lim_(x->0)(x)/(2x) =1/2 $

Then $y = 0$ 
$ lim_(y->0)(y^2)/(y) =0 $

Since $1/2!=0$ The limit does not exist


= Problem 8
Find the following limit
$ lim_((x,y)->(0,0)) (x+y)/(sin(y)+2) $

== Solution
Try different paths $x = 0$ and $y = 0$

First $x = 0$
$ lim_(x->0)y/2=0 $

Then $y = 0$
$ lim_(y->0)(x)/2= 0 $
