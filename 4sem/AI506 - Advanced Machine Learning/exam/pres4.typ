#import "@preview/slydst:0.1.5": *

#show: slides.with(
  title: "Presentation Title",
  subtitle: "AI506 — Advanced Machine Learning",
  date: "May 2026",
  authors: ("Simon Holm",),
  layout: "medium",
  ratio: 16/9,
  title-color: rgb("#1a1a2e"),
)

== Graph Neural Networks


Graphs as a data structure: nodes, edges, neighborhoods, permutation invariance
Message passing framework
Graph convolution and graph attention
Practical considerations: over-smoothing, jumping knowledge, depth


== Topic 1

Some content here.

#v(1em)

#align(center)[
  $ f(x) = sigma(W x + b) $
]

== Topic 2

More content here.

- Point A
- Point B
- Point C

== Topic 3

#grid(
  columns: (1fr, 1fr),
  gutter: 2em,
  [
    *Left column*

    Some text or diagram.
  ],
  [
    *Right column*

    Some text or diagram.
  ]
)

== Conclusion

- Key takeaway 1
- Key takeaway 2
- Key takeaway 3
