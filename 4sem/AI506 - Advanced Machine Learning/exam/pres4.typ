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
  - *Message passing framework*
  - Graph *convolution* and graph *attention*
  - Practical considerations: *over-smoothing*, *jumping knowledge*
]

#slide(title: "GNN (Graph neural networks)")[
  Let
  #figure($#[`nodes`] = {upright(A), upright(B), upright(C)}, qquad #[`edges`] = {(A,B), (B,C)} $)
  
  #v(1em)
  #figure(image("assets/image-18.png", width: 13em))
  

]

#slide(title: "Message passing framework")[
  $  $
  $ h^((l+1))_i = sigma("UPD"(h^((l))_i, m^((l))_i)) $
  $ "UPD"(h^((l))_i, m^((l))_i) = W_h dot h^((l))_i + W_cal(N) m^((l))_i + b^((l)) $
  where
  $ m_i = "AGG"_(j in cal(N)(i)) g(h^((l))_i,h^((l))_j) $
  $ g = "some tranformation of "h_i "and" h_j $
]

#slide(title: "Aggregation")[
  #show math.equation: set text(30pt)
  $ "AGG"_"sum" = sum_(j in cal(N)(i)) h_j^((l)) $
  $ "AGG"_"avg" = 1/norm(cal(N)(i)) sum_(j in cal(N)(i)) h_j^((l)) $
  $ "AGG"_"max" = max_(j in cal(N)(i)) (h_j^((l)))  $
]

#slide(title: "Graph convolution")[

  #figure(image("assets/image-19.png", width: 50%))
  #show math.equation: set text(25pt)
  $ "AGG" = sum_(j in cal(i)) h_j / sqrt(norm(cal(N)(i)) dot norm(cal(N)(j))) $
]

#slide(title: "Graph attention (GAT)")[
  #show math.equation: set text(25pt)
  - Edge attributes (weight)
  $ g(h^((l))_i,h^((l))_j) = a_(i j) dot h^((l))_j $
  $ where a_(i j) = "softmax"_(cal(N)(i)) (tran(a) [W h_i pplus W h_j]) $
  Where $a$ is a learnable vector.
  $ "AGG"_"sum" = sum_(j in cal(N)(i)) a_(i j) h_j^((l)) $
  
]



#slide(title: "Over-smoothing")[
  #show math.equation: set text(25pt)
  $ "AGG"_"avg" = 1/norm(cal(N)(i)) sum_(j in cal(N)(i)) h_j^((l+1)) = 1/norm(cal(N)(i)) sum_(j in cal(N)(i)) (1/norm(cal(N)(j)) sum_(k in cal(N)(j)) h_k^((l))) $ 
  #figure(image("assets/image-26.png", width: 30em))
]


#slide(title: "Jumping knowledge")[
  #show math.equation: set text(25pt)
  Concatenation:
  $ z_i = "MLP"(h_i^((1)) pplus h_i^((2)) pplus dots pplus h_i^((k))) $
  
  Max pooling:
  $ z_i = "MLP"(h_i^"max"), qquad h_i^"max" = "MaxPool"(h_i^((1)), h_i^((2)), dots, h_i^((k))) $

  LSTM:
  $ z_i = "MLP"(h_i^"LSTM"), qquad h_i^"LSTM" = "LSTM"(h_i^((1)), h_i^((2)), dots, h_i^((k))) $
]