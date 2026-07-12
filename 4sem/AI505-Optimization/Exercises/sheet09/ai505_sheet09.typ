#import "@local/tempst:0.1.0": *

#show: exercise.with(
  title: "Exercise sheet 7",
  course: "AI504 — Knowledge Representation",
  author: "Simon Holm",
  date: "April, 2026",
)

= Exercise 9

A pharmaceutical company produces three types of drugs: A, B, and C. These drugs require raw materials,
chemical processing time, and packaging units. The goal is to maximize profit, taking into account restrictions
due to resource availability and production balance.

Drug A contributes 60 DKK per unit to the profit, drug B contributes 100 DKK per unit, drug C contributes 80
DKK per unit. The consumptions per units of drug of raw materials, chemical processing time, and packaging
units are given in Table [tab] together with the quantities available. While consumptions of raw material and
processing time cannot exceed the amount available, excess of packaging units is allowed. Extra packaging units
can be bought but each extra unit reduces profit at 5 DKK per unit while packaging units left can be reused
in the next production period and hence contribute positively to the profit with the same amount of DKK per
unit. Due to contractual obligations, Drug B must be produced in exactly twice the amount of Drug A.
Formulate the problem in linear programming terms. Write first the instantiated model and then the abstract,
general model separating model from data.

#figure(
  image("assets/image.png", width: 20em),
  caption: [Data from the pharmaceutical company]
)


== Solution

#align($ max_x c^T x \
s.t quad \
5x_1 + 8x_2 + 6x_3 &<=600\
3x_1 + 4x_2 + 5x_3 &<=400\
2x_1 + 3x_2 + x_3 &=200 + x_4\
x_2 &= 2x_1\
x &>=0\
x in &NN^(4),\

$)
#pagebreak()

= Problem 11
Consider the following linear programming problem:

#align($ maxi(x) Z = 60x_1 + 100x_2 + 80x_3 -5x_4 \
s.t quad \
5x_1 + 8x_2 + 6x_3 <=600\
3x_1 + 4x_2 + 5x_3 <=400\
2x_1 + 3x_2 + x_3 =200 + x_4\
x_2 = 2x_1
\
x_1,x_2,x_3 >=0 \
x_4 in RR\
$)

Your tasks:
- Transform the problem in standard form
- Transform the problem in equality form
- Solve the problem numerically with `scipy.optimize.linprog`.

== Solution

$ c = mat(60;100;80;-5), quad D = mat(5,8,6,0;3,4,6,0;2,3,1,-1;-2,-3,-1,1;0,1,0,0) "and" b = mat(600;400;200;-200;-2x_1) $
#align($ max_x c^T x \
s.t quad \
D x &<= b\
x &>= 0\

x in &NN^(4),\
c in &XX^4,\
b in &ZZ^5\
D in &ZZ^(3times 5)\
$)
