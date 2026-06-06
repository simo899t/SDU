#import "@preview/typslides:1.3.3": *
#import "../../../temp/temp.typ": *
#show: typslides.with(
  ratio: "16-9",
  theme: "bluey",
)

#front-slide(
  title: "Transformers",
  subtitle: "AI506 — Advanced Machine Learning",
  authors: "Simon Holm",
  info: "May 2026",
)


#slide(title: "The Transformer Architecture")[
  #figure(
    grid(rows: 2, row-gutter: 3em, columns: 3, column-gutter: 2em,
      [Encoder], [Decoder], [Encoder-Decoder],
      image("assets/image-40.png",width: 6em),
      image("assets/image-41.png",width: 8em),
      image("assets/image-28.png",width: 12em)))
  
]

#slide(title: "Attention")[
  #figure(image("assets/image-21.png", width: 16em))
]

#slide(title: "Self-attention")[

  $ "ATN"(X) = softmax((tran(Q)K) / sqrt(d_k)) V $
  #v(1em)
  $ Q = X W_Q quad K = X W_K quad V = X W_V $
]

#slide(title: "Cross-attention")[
  
  $ "ATN"(X, bold(Z)) = softmax((tran(Q)K) / sqrt(d_k)) V $
  #v(1em)
  $ Q = X W_Q quad K = bold(Z) W_K quad V = Z W_V $
  
]

#slide(title: "Multi-head attention")[
  #figure(
    grid(
    columns: 2,
    column-gutter: 3em,
    align: horizon,
    [
      $ "MultiHead"(X) = "head"_1 pplus dots pplus "head"_h $
      #v(1em)
      $ "head"_i = softmax(
          (tran(Q)K) / sqrt(d_k)
        ) V $
    ],
    image("assets/image-27.png"),
  ))
]

#slide(title: "Position embeddings")[
  - Absolute position
  $ v_i = W_v dot (x_i + p_i) $
  - Relative position
  $ q_m^T k_n = x_m^T W_Q^T W_K x_n + p_m^T U_Q^T U_K p_n + bold(b_(n-m)) $
]

#slide(title: "Attention masks")[
  #figure(image("assets/image-29.png",width: 23em))
]


