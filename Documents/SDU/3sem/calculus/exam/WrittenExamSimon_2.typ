#import "../../../../../temp.typ": *
#show: exam.with(
  title:         "Written Exam",
  // subtitle:      "Re-exam",                    // optional
  course:        "AI503 — Calculus",
  author:        "Simon Holm",
  date:          "March 2nd 2026",
  student-id: "sihol24",
  username:      "",                    // optional — shown in page header
  student-number: "sihol24",                 // optional — shown in page header
  duration:      "2 hours",                    // optional
  allowed-aids:  "All written materials",      // optional
  university:    "University of Southern Denmark",
  outline:       false,
)

= Problem 1

Use the following figure about $f '(x)$ to answer the questions:

$ #image("assets/image-66.png") $

== Solution

(a) Over what intervals is $f$ increasing? Decreasing?
- increasing in $x > 0$
- decreasing in $x<0$

(b) Does $f$ have local maximum or local minimum? If so, which and where?

$f$ is a polynomia likely somthing like $x^2$ with a local minimum at $x=0$



= Problem 2


== Solution

(a) Determine whether the following limit exists.
It does
$ lim_((x,y)->(0,0)) (x y)/(x^2+y^2)) = -1/2 .. 1/2 $

(b) Is the function continuous along $y = x$

No, there are massive jumps

$ #image("assets/image-60.png", width: 20em) $


#pagebreak()

= Problem 3
A plane has equation $z = 5x - 2y + 7$.
== Solution

(a)

NOT done #emoji.face.sad

(b) Find a value of a so that the point $(a + 1, a, a - 1)$ lies on the plane.
$ a - 1 = 5(a + 1) - 2a + 7 => a=-6.5 $

$ #image("assets/image-64.png",width: 20em) $

= Problem 4

== Solution

Tangent of point (1,0,9) is $ 0= 3x + 6 + y - z $

= Problem 5

== Solution

(a) What is the rate of change of $f (x, y) = 3x y + y^2$ at the point (2, 3) in the direction
(3, -1)?

Directional derivative is $7y+3x $
at (2,3) that is $5$

(b) What is the direction of maximum rate of change of f at (2, 3)?

That is $nf([2,3]) = 9,12 $

(c) What is the maximum rate of change?

$ [3y,2x+2y] $

= Problem 6
We want to express the volume of the quarter sphere below as an integral of the spherical coordinate $integral_W f dif V$ where $W$ is the quarter sphere and $dif V = rho 2 sin(ϕ) dif rho dif phi dif theta$.
 
$ #image("assets/image-65.png", width: 20em) $

== Solution

(a) What must the function $f$ be?
Function must be $f=1$

(b) Write the limits of the integration.

$ integral_(theta = 0)^(pi/2) integral_(phi=-pi/2)^(pi/2)integral_(rho=-pi/2)^(pi/2) $

(c) Evaluate the integral.

$ integral_(theta = 0)^(pi/2) integral_(phi=-pi/2)^(pi/2)integral_(rho=-pi/2)^(pi/2) 1 dif rho dif phi dif theta = pi^3/2 $


= Problem 7

Integrate the function $f (x, y) = x + y$ over the shaded region below.

$ #image("assets/image-61.png", width: 20em) $

== Solution

$ int(0,2,int(0,1,x+y dif x) dif y) = 17/3  $
#pagebreak()

= Problem 8

Let $F(x) = int(0,x,sin(pi t) dif t)$

== Solution

(a) Evaluate $F(1)$

$ F(1) = int(0,1,sin(pi*t) dif t) = 2/pi $

(b) For what values of $x$ is $F (x)$ positive? negative?

$ #image("assets/image-62.png", width: 20em) $
Aways $0<=F(x)$
#pagebreak()

= Problem 9
Find the volume of the solid that lies under the paraboloid $z = 4 - x^2 - y^2$ and above the xy-plane.

== Solution

Plot it
$ #image("assets/image-63.png", width: 18em) $


Then
$ int(0,5,int(-3,-3,int(-3,-3,4-x^2-y^2))) dif x dif y dif z = 360 $



= Problem 10
Decide whether the following statements are true or false.

== Solution
(a)
*TRUE*

(b)
*FALSE*

(c)
*TRUE*

(d)
*TRUE*

(e)
*FALSE*
