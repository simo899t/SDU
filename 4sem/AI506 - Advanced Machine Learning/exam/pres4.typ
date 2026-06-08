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



#slide(title: "The graph structure")[
  - Nodes: $V$
  - Node features: $V -> RR^d$
  - Edges: $E subset V times V$
  
  #figure(grid(columns: 2, column-gutter: 4em, row-gutter: 2em,
  image("assets/image-59.png",width: 9em),
  image("assets/image-60.png",width: 9em)
  ))
]

#slide(title: "Common GNN tasks")[
  #figure(image("assets/image-58.png"))
]

#slide(title: "Common GNN tasks")[
  For node classification:

  Let X = $H^0$

  $ H^((l+1)) = sigma (hat(A) H^((l)) W^((l)) + b^((l))) $

  #v(1em)
  $ Z = softmax(hat(A) ReLU(z^((l-1))) W^((l))) $

]

#slide(title: "Graph convolution")[

  #figure(image("assets/image-55.png",width: 33em))
  #figure(image("assets/image-57.png",width: 30em))
  #show math.equation: set text(25pt)

]

#slide(title: "Message passing framework")[
  $ h^((l+1))_i = sigma("UPD"(h^((l))_i, m^((l))_i)) $
  $ "UPD"(h^((l))_i, m^((l))_i) = W_h dot h^((l))_i + W_cal(N) m^((l))_i + b^((l)) $
  where
  $ m_i^((l)) = "AGG"_(j in cal(N)(i)) g(h^((l))_i,h^((l))_j) $
  $ g = "some transformation of "h_i "and" h_j $
]

#slide(title: "Aggregation")[
  #show math.equation: set text(26pt)
  $ "AGG"_"sum" = sum_(j in cal(N)(i)) h_j^((l)) $
  $ "AGG"_"max" = max_(j in cal(N)(i)) (h_j^((l)))  $
  $ "AGG"_"avg" = 1/norm(cal(N)(i)) sum_(j in cal(N)(i)) h_j^((l)) $
  $ "AGG"_"norm" = sum_(j in cal(N)(i)) h_j^((l))/(norm(cal(N)(i))  dot norm(cal(N)(j))) $
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
  $ z_i = "MLP"(h_i^((1)) pplus h_i^((2)) pplus dots pplus h_i^((l))) $
  
  Max pooling:
  $ z_i = "MAX"(h_i^((1)), h_i^((2)), dots, h_i^((l))) $

  LSTM:
  $ z_i = "LSTM"(h_i^((1)), h_i^((2)), dots, h_i^((l))) $
]