#import "@local/tempst:0.1.0": *
#show: note.with(
  title: "Lecture 15: Discrete Optimization",
  author: "Simon Holm",
  date: "April - 2026"
)

// Your content starts here

= The ROAR-NET API: Constructive Search

== Community Detection Problem
A fully-connected undirected graph
 - vertices $v$ must represent users
 - Weighted edges represent the intensity of some attribute of their interaction
  
Positive weight indicate affinity between users, while negative edge weights indicate lack of affinity. For this groups of users connected mostly by positively weighted edges suggest the existence of a community involving those individuals.

#figure(image("assets/image-6.png"))

*Goal* Partition the vertices into subsets while maximizing the total weight of the edges within the groups (cliques)
- *Clique-partitioning problem*

We need to define the problem and the API functions to be implemented
#figure(image("assets/image-7.png"))
Given a (combinatorial) optimization problem $Pi$ and one of its instances $pi$

= Local Search Algorithms
Local search in a nutshell
1. Visit neighbors of a current solution
2. Decide whether to reject them or to accept one as the next solution
3. Repeat

At each solution one can then generate possible moves
- Enumeration (enumerate through the environment so and find all possible moves)
- Random sampling
  - With replacement
  - Without replacement

== Search space
$ S(pi) $
- The search space is the set of all candidate solutions that may be visited during the solving process
- Only feasible solutions are considered in local search. The objective function is defined on the whole search space
- Where solution set $S'(pi) subset S(pi)$
- Initial feasible solution(s) may be:
  - Generated at random
  - Obtained by constructive search
  - Obtained heuristically in other ways

== Evaluation function
  $ f_pi : S(pi) -> RR $
- Handles *soft constraints* and the objective function
  some thing like $ min f(x) - h(x) $  

== Initialization function
$ #[`init`]: emptyset -> S(pi) $

- Can be seen as a probability distribution $P(S(pi) times M(pi))$ over initial search positions and memory states

== Neighborhood function
$ N_pi S -> 2^(S(pi)) $

For each solution $s in S(pi)$ define a set of solutions that are in some sense close to $s$

== Transition model
$ #[`step`]: S(pi) times M(pi) -> S(pi) times M(pi) $
- Takes a step
- Can be seen as a probability distribution $P(S(pi) times M(pi))$ over subsequent, neighboring search positions and memory states

== Termination predicate
$ #[`terminate`]: S(pi) times M(pi) -> {top, bot} $
- Determines the termination state for each search position and memory state

= Metaheuristics
How to avoid getting trapped in bad local optima?
- Restart
- more complex neighborhood functions:
  - Variable Neighborhood Search and Large Scale Neighborhood Search
  - diversified neighborhoods + incremental algorithmics
- allow non-improving moves
  - Tabu Search: Online learning of moves, Discard undoing moves, Discard inefficient moves, Improve efficient moves selection
  - Simulated annealing

#figure(
  image("assets/image-8.png")
)

== Evaluation function
#figure(image("assets/image-9.png"))

== Constraint-based local search
If infeasible solutions are allowed, we count violations of constraints.

- decomposition-based violations
  - count number of violations, eg: `alldiff`
- variable-based violations
  - min number of variables that must be changed to satisfy $c$
- value-based violations
  - for constraints on number of occurrences of values
- arithmetic violations
- combinations of these

=== alldiff
`alldiff(x_1,...,x_n`
Let $a$ be an assignment with $V ) {a(x-1), dots, a(x_n)}$ and $c_v = \#_a(v,x)$ be the number of violations in $a$
Then possible definitions for violations are

#figure(image("assets/image-10.png"))

Arithmetic constraints:
#figure(image("assets/image-11.png"))

== Fundamental principles
#figure(image("assets/image-12.png"))

== Reservoir sampling
Store the best neighbor/neighbors.
Iterate through neighborhood and replace with better neighbors. If another neighbor is equally good, replace at probability.