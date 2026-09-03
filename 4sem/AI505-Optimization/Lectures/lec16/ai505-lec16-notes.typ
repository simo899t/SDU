#import "@local/tempst:0.1.0": *
#show: note.with(
  title: "Lecture 15: Discrete Optimization",
  author: "Simon Holm",
  date: "April - 2026"
)

// Your content starts here

= Dynamic Programming
This is applied to problem with *optimal substructure* and *overlapping subproblems*

= Overlapping subproblems
A problem has overlapping subproblems when solving it recursively involves solving the same subproblems multiple times rather than always generating new ones.

#example(title:[Example: padovan_naive],[
#figure(
image("assets/image-6.png")
)
#figure(
  image("assets/image.png")
)
Notice here that some computations are done multiple times]) of overlapping subproblems

= Optimal substructure
The *Principle of Optimality* (known as Bellman Optimality Conditions):

A problem has optimal substructure if an optimal solution to the whole problem contains optimal solutions to its subproblems.

#example([If the shortest path from A to D goes $ A → B → C → D,$  
then:
- $B → C → D$ must be the shortest path from B to D
- $C → D$ must be the shortest path from C to D])


#example(title: [Example 2: The knapsack problem], [
  #figure(image("assets/image-2.png"))
  #figure(image("assets/image-1.png"), caption: [])
])

= Constraint programming
#example(title: [Example: Number Circle Puzzle],[
  #figure(
    image("assets/image-3.png")
  )
  _Note the symmetry in the this problem_
])

There are many parts to solving problems like this.

== The core of constraint computation
Modelling (deciding on variables/domains/constraints)
- Inference/propagation
- Heuristics
- Symmetry
- Backtracking
#figure(image("assets/image-4.png"))

#definition(title: [Definition of Variables, Domains, Constraints],[
The *domain* of a variable $x$, denoted $D(x)$, is a finite set of elements that can be assigned to $x$.

A *constraint* $C$ on $X$ is a subset of the Cartesian product of the domains of the variables in $X$ , $ie$, $C subset D(x_1) times dots times D(x_k) $. A tuple $(d_1, dots, d_k ) in C$ is called a solution to $C$.

Equivalently, we say that a solution $(d_1, dots, d_k ) in C$ is an assignment of the value $d_i$ to the variable $x_i$ for all $1 <= i <= k$, and that this assignment satisfies $C$.

If $C = emptyset$, we say that it is *inconsistent*.

*Extensional*: specifies the good (or bad) tuples (values)
*Intensional*: specifies the characteristic function
])

#definition(title: [Definition: Constraint Satisfaction Problem (CSP)],[
A CSP is a finite set of variables $X$ with domain extension $D = D(x_1) times dots times D(x_n)$, together with a finite set of constraints C, each on a subset of $X$. A solution to a CSP is an assignment of a value $d in D(x)$ to each $x in X$, such that all constraints are satisfied simultaneously.
])

#definition(title: [Definition: Constraint Optimization Problem (COP)],[
A COP is a CSP $cal(P)$ defined on the variables $x_1, dots, x_n$, together with an objective function $f : D(x_1) times dots times D(x_n) -> Q$ that assigns a value to each assignment of values to the variables. An optimal solution to a minimization (maximization) COP is a solution $d$ to $P$ that minimizes (maximizes) the value of $f(d)$.
])

