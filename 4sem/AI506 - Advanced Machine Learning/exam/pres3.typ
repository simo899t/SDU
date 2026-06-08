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

#slide(title: "Tokenization and encoding")[
  #figure(image("assets/image-43.png"))
]

#slide(title: "Self-attention")[

  #figure(grid(
    columns: 2, column-gutter: -7em,
    image("assets/image-45.png",width: 27em),
  $ "ATN"(X) = softmax((tran(Q)K) / sqrt(d_k)) V \  Q = X W_Q quad K = X W_K quad V = X W_V \ #v(3em) $))

  #figure(image("assets/image-51.png"))
]

#slide(title: "Cross-attention")[
  
  #figure(grid(
    columns: 2, column-gutter: -6em,
    image("assets/image-46.png",width: 28em),
  $ "ATN"(X) = softmax((tran(Q)K) / sqrt(d_k)) V \  Q = X W_Q quad K = bold(Z) W_K quad V = Z W_V \ #v(3em) $))
  
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
    image("assets/image-52.png"),
  ))
]

#slide(title: "Positional encoding")[
  - Absolute position
  $ v_i = W_v dot (x_i + p_i) $
  - Relative position
  $ q_m^T k_n = x_m^T W_Q^T W_K x_n + bold(b_(n-m)) $
]

#slide(title: "Attention masks")[
  #figure(image("assets/image-29.png",width: 23em))
]

#slide(title: "Pointwise MLP")[
  #figure(grid(columns: 1, image("assets/image-44.png", width: 30em)))
  $ "MLP"(x) = W_"out" dot sigma(W_"in" x + b_"in") + b_"out" $ 

  _Given this input, which learned patterns are relevant, and what should be added to the representation as a result?_
]


#slide(title: "Residual connections and Normalization")[
  #figure(grid(columns: 2, column-gutter: 2em, image("assets/image-42.png", width: 14em), image("assets/image-65.png", width: 3em)))

  
  $ h^((l+1)) = "LN"("MLP"(h^((l))) + h^((l))) $
  $ h^((l+1)) = "MLP"("FN"(h^((l)))) + h^((l)) $  
  $ "LN"(x) = (x - mu) / sqrt(sigma^2 + epsilon) $
]

#slide(title: "Language modeling objective")[
  - Pretraining
  $ loss_"PT" = - sum_t log p_theta (x_t | x_(<t) ) $
  - Supervised Finetuning
  $ loss_"SFT" = - sum_t log p_theta (y_t | x, y_(<t) ) $
  - RL from Human Feedback
]




