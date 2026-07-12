#import "@local/tempst:0.1.0": *
#show: note.with(
  title: "Lecture 14",
  course: "AI504 - Knowledge Representation",
  date: "May/2026"
)
// content starts here

/*
Come by Siddarth's office or via email
bro is dropping problem set 7 (everyone automatically gets 12)
*/

= quick exercise
Formalize the following statement.

- There are infinity many prime numbers


Lets work in $NN$
Assume then that we have that

`prime(n), <`

$forall n in NN, exists m in NN : n<m and "prime"(m) $

- define prime$(n)$

$ "prime"(n) bi n>1 and forall x in NN. x | n => (x=1 or x = n) $
There are many legitimate ways to express this
$ "prime"(n) bi forall a,b in NN. space (n|a dot b => (n|a or n|b)) $

= The proof theory of first-order logic
- Many different styles of proof calculi
- Here is one, described somewhat informally

  A proof in first-order logic is a tree, each node in the tree is labeled by two sets of formulas.
$ "What we know"qquad"and"qquad"what are the goals" $
At each node we pick a formula (either known/goal) and simplify according to the following rules, the other formulas are inherited from above.

#figure(
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $"known" quad "goal"$
            - $"known" quad "goal"$
              - $"known" quad "goal"$
              - $"known" quad "goal"$
                - $"known" quad "goal"$
                  - $"known" quad "goal"$
                  - $"known" quad "goal"$

        ]
)#pagebreak()


#example(title:[Rules],[
  Let the goal be $P imp Q$
  Then we would Assume $P$ and the new goal is $Q$
  #figure(
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $"known": "______" quad "goal": (P->Q)$
            - $"known": P quad "goal": Q$

        ]
)
Let $P, P imp Q$ be known, then
#figure(
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $"known": P,P imp Q quad "goal": "______"$
            - $"known": P, Q quad "goal": "______"$

        ]
)
Let $P or Q$ be known
#figure(
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $"known": P or Q quad "goal": "______"$
            - $"known": P quad "goal": "______"$
            - $"known": Q quad "goal": "______"$

        ]
)
Let $P and Q$ be the goal
#figure(
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $"known": "______" quad "goal": P and Q$
            - $"known": "______" quad "goal": P,Q$


        ]
)
Let $P or Q$ be the goal
#figure(
  grid(columns: 2,column-gutter: 2em,
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $"known": "______" quad "goal": P or Q$
            - $"known": "______" quad "goal": P$
           
        ], tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $"known": "______" quad "goal": P or Q$
            - $"known": "______" quad "goal": Q$
            
        ])
)
#v(7em)
Let $P and Q$ be known
#figure(
  grid(columns: 2,column-gutter: 2em,
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $"known": P and Q quad "goal": "______"$
            - $"known": P quad "goal": "______"$
           
        ], tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $"known": P and Q quad "goal": "______"$
            - $"known": Q quad "goal": "______"$
           
        ])
)
$forall$ is known. "for all $x$, x has property $phi$"
#figure(
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $forall x phi$
            - $underbrace(phi(t), "term "t" for ""x")$


        ]
)

$forall$ is goal. "for all $x$, x has property $phi$"
#figure(
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $forall x phi$
            - $phi$


        ]
)

$exists$ is known. "there exists an $x$, such that x has property $phi$"
#figure(
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $exists x phi$
            - $underbrace(phi(t), "term "t" for ""x")$


        ]
)
$exists$ is goal. "there exists an $x$, such that x has property $phi$"
#figure(
  tree(
          spacing: (20pt, 30pt),
          node-inset: 7pt,
          shape: "rectangle"
        )[
          - $exists x phi$
            - $phi$


        ]
)
])