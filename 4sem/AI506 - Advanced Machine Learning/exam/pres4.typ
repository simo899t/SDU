#import "@preview/typslides:1.3.3": *
#import "../../../temp/temp.typ": *
#show: typslides.with(
  ratio: "16-9",
  theme: "bluey",
)

#front-slide(
  title: "Graph Neural Networks",
  subtitle: "AI506 — Advanced Machine Learning",
  authors: "Simon Holm",
  info: "May 2026",
)

#slide(title: "Overview")[
  - *Graphs* as a data structure: *nodes*, *edges*, *neighborhoods*, *permutation invariance*
  - Message passing framework
  - Graph convolution and graph attention
  - Practical considerations: over-smoothing, jumping knowledge, depth
]

#slide(title: "GNN (Graph neural networks)")[
  #figure($#[`nodes`] = {upright(A), upright(B), upright(C), upright(D), upright(E), upright(F), upright(G), upright(H)}, qquad #[`edges`] = {1,0,2,3} $)
  #v(1em)
  #figure(image("assets/image-18.png", width: 25em))
  
  
]

#slide(title: "Message passing framework")[

  
]
#slide(title: "Graph convolution & attention")[

  
]
#slide(title: "Practical considerations")[

  
]
#slide(title: "Over-smoothing")[

  
]
#slide(title: "Feedforward nets")[

  
]
#slide(title: "Feedforward nets")[

  
]

