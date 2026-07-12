#import "@local/tempst:0.1.0": *

#show: exercise.with(
  title: "Exercise sheet 7",
  course: "AI504 — Knowledge Representation",
  author: "Simon Holm",
  date: "April, 2026",
)

= Exercise $1^*$
Consider the Boolean objective function:
$ f(x) = x_1 and (x_2 or not x_3) and (not x_1 or not x_2) $

Formulate the problem as an integer linear program. Can any Boolean satisfiability problem be formulated as an integer linear program?

== Solution

$ max_x f(x) \
st A x =b \ x in ZZ^n $

= Exercise $2^*$

Consider  $max{c^top x | A x = b, x in ZZ^n}.$ Sometimes the solution of the linear relaxation is already integral. Can you find a sufficient condition for the matrix $A$ for that to happen?

==

$ max_x c^top x \
st A x =b \ x in RR^n $

Firstly $A$ must only have integer entries 

Also for $ A_B x_B + A_N x_N = b imp x = A_B^(-1)b $

Since for $x in ZZ$ then 
$ A^(-1) = 1/det(A) adj(A) $

For this $A_B$ must have $det(A) = 0,-1,1$

"A should be a totally unimodular matrix"

#pagebreak()

= Exercise $3^*$
Consider the following problem:
$ max_x x_1 - x_2 \
st 2/3 x_1 + 1/2 x_2 <= 1 \
x_1,x_2 >= 0\ 
x_1,x_2 in ZZ $

Derive a Chvatal-Gomory cut.

Since $x^*_"LP" = (3/2,0)$

The cut should be $x_1 <= 1$

= Exercise $4^*$
The 0-1 knapsack problem is a combinatorial optimization problem that can be described as follows: Given a set of items, each with a size/weight and a value, the problem is to choose the items that maximize the total value under the condition that the total size/weight is below a certain threshold.

Design and apply by hand a dedicated branch and bound algorithm to the following instance of the 0-1 knapsack problem: values $v = [9, 4, 2, 3, 5, 3]$, weights $w = [7, 8, 4, 5, 9, 4]$ and capacity $W = 20$.

You will find useful the following observation that can be proven true: The relaxed knapsack problem can be efficiently solved with a greedy approach. Items are added one at a time by selecting the next item with the greatest ratio of value to weight. If there is enough remaining capacity, the item is fully assigned with $x_i = 1$.
If not, a fractional value is assigned such that the remaining capacity is saturated and all remaining items have $x_i = 0$.

#figure(
  image("assets/image-1.png"),
  caption: [table form exercise]
)
#pagebreak()

== Solution
$ max sum_(i) v_i x_i \ 
st sum_i w_i x_i <= W \
x_i in  $

This means that $"obj" = 17$

sort by $v/w$

Then we do greedy search to fit for 17

= Exercise $6^*$
A medley relay is a team swimming event where each swimmer on the team swims a different stroke in a specific order: backstroke, breaststroke, butterfly, and freestyle. This event can vary in distance, typically ranging from 100 to 400 meters, depending on the competition. Consider the problem of selecting students for a swimming medley relay team. In @table show times for each swimming style of five students.

#figure(
  image("assets/image.png"),
  caption: [table from exercise]
)<table>

We need to choose a student for each of the four swimming styles such that the total relay time is minimized. Try first to do this task by hand. Then, formulate the problem as a MILP and solve it in Python. Finally, compare the solution with the one obtained by hand.

== Solution

$ "backstroke: "A quad
  "breaststroke: "C quad
  "butterfly: "D quad
  "freestyle: "B  $
$ 43.5 + 39.1 + 44.5 + 36.8 = 163.9 $


Let $j in {1,2,3,4}$ and $i in {A,B,C,D,E}$

then $A = mat()$

$ min  sum_(i j) t_(i j) x_(i j) \
st summ(i=1,5, x_(i j))= 1, quad forall j\
summ(j=i,4,x_(i j) <= 1) quad forall i\
x_(i j) in {0,1} $