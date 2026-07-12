#import "@local/tempst:0.1.0": *

#show: exercise.with(
  title: "Exercise sheet 1",
  course: "AI505 - Optimization",
  author: "Simon Holm",
  date: "April, 2026",
)
#set heading(numbering: none)

= Exercise 1
Implemented in python

#figure(
  image("assets/image.png"),
  caption: [$f(x)$ and contour plot]
)

= Exercise 2
$ f(x+h) = f(x) + (f'(x))/(1!) h + (f''(x))/(2!) h^2 $
Let $f(x) = cos(x^(-1))$
$ f(x+h) = cos(x^(-1)) + (x^(-2) sin(x^(-1)))/(1!) h - (2x^(-3) cos(x^(-1)) - x^(-4) cos(x^(-1)))/(2!) h^2 $

Let $g(x) = cos(x)$
$ g(x+h) = cos(x) - sin(x)/(1!)h - cos(x)/(2!)h^2 + sin(x)/(3!) h^3, $

then $g(x) = 0.5394605649$
= Exercise 3
The function is convex if $f: RR^n -> RR$ and if $forall x,y in RR^n, alpha in [0,1]$ it holds that

$ f(alpha x + (1-alpha)y) <= $

= Exercise 4

= Exercise 5

= Exercise 6

= Exercise 7

= Exercise 8

= Exercise 9

= Exercise 10

= Exercise 11

= Exercise 12
