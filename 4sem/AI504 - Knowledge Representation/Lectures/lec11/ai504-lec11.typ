#import "../../../../temp/temp.typ": *
#show: project.with(
  title: "Lecture 11: Tableau-style proofs",
  course: "AI504 - Knowledge Representation",
  date: "April/2026"
)
// content starts here

/*
Come by Siddarth's office or via email
*/

= Definition
#definition(title: [Definition of an analytic tableau],[
  A #underline("tableau") (for propositional logic) is a tree where each node is labeled by two sets of formulas 
  (a $T$-set and a $F$-set) and each non-leaf node has an #underline("active") connective, which is the topmost connective in on one the formulas subject to the following rules:
  #figure(  // large figure for the rules of tableau expansion
    grid(columns: 3,
  rows: 2,
  column-gutter: 2em,
  row-gutter: 2em,
  tree(
  shape: "rectangle",
  spacing: (40pt, 20pt),
  node-inset: 4pt
)[
  - $T:alpha or beta$
    - $T:alpha$
    - $T:beta$
],
tree(
  shape: "rectangle",
  spacing: (40pt, 20pt),
  node-inset: 4pt
)[
  - $T:alpha and beta$
    - $T:alpha, beta$
],
tree(
  shape: "rectangle",
  spacing: (40pt, 20pt),
  node-inset: 4pt
)[
  - $T:not alpha$
    - $F:alpha$
],
tree(
  shape: "rectangle",
  spacing: (40pt, 20pt),
  node-inset: 4pt
)[
  - $F:alpha or beta$
    - $F: alpha, beta$
],
tree(
  shape: "rectangle",
  spacing: (40pt, 20pt),
  node-inset: 4pt
)[
  - $F:alpha and beta$
    - $F:alpha$
    - $F:beta$
],
tree(
  shape: "rectangle",
  spacing: (40pt, 20pt),
  node-inset: 4pt
)[
  - $F:not alpha$
    - $T:alpha$
]
  ), gap: 1.5em, caption: [Rules for tableau expansion (propositional logic)]
  )

])

= Example
Consider the following formula $ (p or q) and not (not p or r) $
== Can we find a satisfying model $model$ for this formula?

Lets see this as the following tree.

#figure(
  tree(
  spacing: (40pt, 20pt),
  node-inset: 4pt
)[
  - $and$
    - $ or $
      - $ p $
      - $ q $
    - $ not $ 
      - $ or $
        - $ not $
          - q
        - $ r $
]
)
Write this a s a tableau-style proof. We start with the root node as $T:(p or q) and not (not p or r) $
#figure(
  tree(
  spacing: (20pt, 20pt),
  node-inset: 7pt,
  shape: "rectangle"
)[
  - $T:(p or q) and not (not p or r)$
    - $T:(p or q), not (not p or r)$
      - $T:p or q\ F: not p or r$ 
        - $T:p or q\ F: not p, r$ 
          - $T:p or q, p\ F: r$
            - $T:p,q\ F:r$
            - $T:p\ T:r$
]
)
Both of the last nodes are leaves.

== Can we a falsifying model $model$ for this formula?
Write this a s a tableau-style proof. We start with the root node as $T:(p or q) and not (not p or r) $

#figure(
  tree(
  spacing: (20pt, 20pt),
  node-inset: 7pt,
  shape: "rectangle"
)[
  - $T:(p or q) and not (not p or r)$
    - $F:(p or q) \ T: not (not p or r)$
      - $F:p,q^*$
    - $T:(p or q) \ F: not (not p or r)$
      - $T:(p or q) \ T: not p or r$
        - $T:not q$
          - $F:p$
        - $T: r$
]
)
Note that $*$ is enough for a falsifying model
#pagebreak()

== What if a model $model$ does not exits?
New sentence $ (p or q) and (not p and not q) $
This sentence is unsatisfiable and a contradiction. We can show this by a tableau-style proof.

#figure(
  tree(
  spacing: (20pt, 20pt),
  node-inset: 7pt,
  shape: "rectangle"
)[
  - $T:(p or q) and (not p and not q  )$
    - $T:(p or q), (not p and not q)$
      - $ T:p, not p and not q $
        - $ T:p, not p, not q $
          - $ T:p, not q \ F: p$
      - $ T:q, not p and not q $
        - $ T:q, not p, not q $
          - $ T:q, not p \ F: q$       
]
)
These leaves are obvious contradictions. But to show that there exits no model, we need to show that all branches result in a contradiction.

= Check formula $x$

== Satisfiability or contradiction
To check if a formula $x$ is satisfiable, we can start with the root node as $T:x$ and apply the tableau expansion rules until we reach the leaves. If all branches end in a contradiction, then $x$ is unsatisfiable. If at least one branch does not end in a contradiction, then $x$ is satisfiable by a model $model$.

== Falsifiability or tautology

Note that for $x$ to be satisfiable, does not mean that $x$ is not falsifiable. If there exists no falsifying model for $x$, then $x$ is a tautology.
#pagebreak()

= Recall the inference problem
Given a set of sentences $Gam$ and a single sentence $phi$, either produce a proof of $phi$ from $Gam$ (whatever that might be), #underline("or") a model satisfying $Gam$ but falsifying $phi$

To determine weather $Gam ent phi:$

Look for a model (via the method of tableaux) on the form $ T:Gam\ F:phi \ dots.v $

If we #underline("find") such a model in a leaf, we have our counter-model, & this means that $Gam ent.not phi$

If on the other hand every one of our leaves ends up in a contradiction, this counts as a proof that $Gam ent phi$.

Correctness of tableau method gives us a decision procedure for inference problem of propositional logic.

= Example
Let $Gam = {p or q, p or r}$ and $phi = p or (q and r)$

Does $Gam ent phi$

#figure(
  tree(
  spacing: (20pt, 20pt),
  node-inset: 7pt,
  shape: "rectangle"
)[
  - $T: p or q, p or r \ F: p or (q and r)$
    - $T:p or q, p or r \ F: p, q and r $
      - $T: p, p or r \ F: p, q and r $
      - $T: q, p or r \ F: p,q and r$  
        - $T:q,r \ F:p,q and r$
          - $T:q,r \ F: p,q$
          - $T:p,r \ F:p,r$ 
]
)
Since all leaves are contradictions, this counts as a proof of $ {p or q, p or r} prov p or (q and r) $