#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 15: Discrete Optimization",
  author: "Simon Holm",
  date: "April - 2026"
)

// Your content starts here

= Randomized Optimization Heuristics

Until now we have mostly seen #u("complete") methods, which explore all possibilities. #u("incomplete") methods use heuristics to approach "good enough" solution.

These include
- effective rules without theoretical support (such as genetics)
- trial and error

Overall the use of heuristics is a basis on empirical evidence rather than mathematical logic. Getting things done in the given time.

== Examples to illustrate concepts

#example(title: [Example The Knapsack Problem], [
  *Given*: A ground set of items with weights and values. A knapsack with a weight capacity.

  *Task*: Find the subset that maximizes the total value
  #figure(image("assets/image.png"))

  *Solution* Set: ${1, 2, 7}$ or Incidence vector: $[1, 1, 0, 0, 0, 0, 1, 0]$

  Total weight: 15; Total value: 26 
])

#example(title: [Example The Traveling Salesman Problem], [ *Given*: A graph $G = (V , E)$ and a weight function $omega : E → RR$.

  *Task*: Find the shortest Hamiltonian tour.
  #figure(image("assets/image-1.png"))
  This extends a partial solution until either a complete solution is found or the solution is good enough.
])
#pagebreak()

#example(title: [Example The Single Machine Total Weighted Tardiness], [
   *Given*: a set of $n$ jobs ${1, dots , n}$ to be processed on a single machine and for each job $j$ a processing time $p_j$ , a weight $w_j$ and a due date $d_j$ 

  *Task*: Find a schedule that minimizes the total weighted tardiness $ summ(j=1,n,w_j dot T_j)$, where $T_j = max{C_j - d_j, 0}$ ($C_j$ completion time of job $j$)

  #figure(image("assets/image-2.png"))
  #figure(image("assets/image-3.png"))
])

#example(title: [Example The Graph Vertex-Coloring], [
   *Given*: A graph $G$ and a set of colors $Gam$. A *proper coloring*: : each vertex receives a color and no two adjacent vertices receive the same color.

  *Task*: Find a proper coloring of $G$ that uses the minimal number of colors (chromatic number).
  #figure(image("assets/image-4.png"))
])

= Search Methods
#definition(title: [Problem statement], [
  Constrained Optimization Problem: $min{f(s)|s in cal(F)}$
  - $cal(F) subset cal(S)$ set of *feasible solutions*
  - $cal(S)$ set for *candidate solutions* (*combinatorial structures*)
])

The concept of feasibility is flexible and a design choice.
Most typically, it implies satisfying the constraints of the problem.

Guiding rules:
- if it has an objective function value, then it is a feasible solution
- constructive search algorithms work with partial, infeasible solutions

== Constraint Handling
*Constraints* in heuristic methods are handled:
- *Implicitly* in the definition of the *combinatorial structures* that constitute the search states: assignments, permutations, (sub)sets, partitions, (sub)graphs, sequences, set of sequences, ...
 - As *one-way* constraints between variables
 - As *soft constraints* ie, relaxed in the evaluation function as penalty components with large weights or as lexicographically more important components

== A Classification
*White box* optimization: 
- models can be expressed mathematically

*Gray box* optimization:
- internal information about objective function computation is often available models that have a mathematical expression but may need data to determine them (eg, neural networks)

*Black box* optimization:
- no mathematical expression is available
- We assume a zeroth order oracle that takes as input a point s ∈ S and outputs a dual bound if infeasible or the value of f at s.

== Approaches to ROHs
*White/Gray* box: 
- representation (modelling) + reasoning (search) constraint based local search, comet, local solver (Hexaly)

*Black* box:
- a different approach, framework separating problem from solvers and defining the interface specification

#figure(image("assets/image-5.png"),
caption: [#link("https://roar-net.eu")[https://roar-net.eu]])

#definition(title: [Definition (Search or Optimization Algorithm)], [
  *Goal formulation*: we want to find the minimum with respect to some criterion from a set of candidate elements.

  *Problem formulation*: Given a description of the states, an initial state and actions necessary to reach the goal, find a sequence of actions to reach the goal.
  
  *Search*: the algorithm simulates sequences of actions in the model of the goal, searching until it finds a sequence of actions that reaches the goal. The algorithm might have to simulate multiple tentative answers that do not meet the goal, but eventually it reaches a solution, or it will find that no solution is possible.
  ])
== Components of a Search Algorithm
(valid for complete or heuristic and constructive or perturbative algorithms)

- *State* (_or candidate_) solution: A definition of states of the search.
- *Search Space*: The set of possible states
- *Initial State*: Can be specific state ($x_0 = [0,0]$) or a random initialized state
- *Goal*: A set of one or more goal states. Sometimes there is one goal state sometimes there is a small set of alternative goal states
- *Evaluation function*: $"f"(s)$ assesses the distance from a potential goal. _(note: different from "objective", it can also include penalties due to constraint violations)_.
- *Action type $t$*: available to the algorithm.
- A finite set of *actions* of type $t$ that can be executed in $s$, $"Actions"(t, s)$.
- A *transition model* that describe what each action $t$ does. $"Result"(s, a)$ returns the state that results from doing action $a in "Actions"(t, s)$ in state $s$.
- An action-cost function, $"Action-Cost"(s, a, s′)$, that gives the numeric cost of applying action a in state $s$ to reach state $s′$. 

= Constructive Search 
Recall: Complete Graph Search Methods
- uninformed settings
  - Breadth-first search
  - Uniform-cost search
  - Depth-first search
- informed settings
  - Greedy best-first search
  - $A^star$ search

== Complete Tree Search
#definition(title: [Search Space], [
  Tree with branching factor at the top level $n d$ at the next level the branching factor is $(n − 1)d$.
The tree has $n! dot d n$ leaves even if only dn possible complete assignments.
])

== Exploiting Information
#definition(title: [Assessment of partial, infeasible solutions], [
  The priority assigned to a node $x$ is determined by the function
  $ fx = gx + hx $
  $gx$: cost of the path so far
  $hx$: : heuristic estimate of the minimal cost to reach the goal from $x$
])

A *greedy best-first search* uses $h$ to decide while a $Astar$ best-first search, is cost optimal when $hx$ is an 
- admissible heuristic: never overestimates the cost to reach the goal
- consistent: $h(n) <= x(n,a,n') + h(n')$ _(consistent $imp$ admissible, only necessary in graph search)_

== Greedy algorithms


== Incomplete Search algorithms
These can either be implemented on the 'backtracking' framework or outside. If they are implemented on the backtracking framework, we can move around truncating paths, and given enough time, this will complete. This is not guaranteed by those implemented outside.

== Bounded backtrack
#figure(image("assets/image-6.png"), caption: [])


== Incomplete Search Ideas

== Construction-Based Metaheuristics
