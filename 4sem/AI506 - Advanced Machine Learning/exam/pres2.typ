#import "@preview/typslides:1.3.3": *
#import "../../../temp/temp.typ": *
#show: typslides.with(
  ratio: "16-9",
  theme: "bluey",
)

#front-slide(
  title: "Convolutional & Recurrent Neural Networks",
  subtitle: "AI506 — Advanced Machine Learning",
  authors: "Simon Holm",
  info: "May 2026",
)

#slide(title: "Overview")[
  - *Parameter sharing*
  - Convolutional nets: *convolution*, *pool*, *residual connections*,* batch/layer norm*
  - Recurrent nets: basic *RNNs*, *long-range dependency* problem, *LSTMs*, *gating*
]

#slide(title: "Parameter sharing")[

  #show math.equation: set text(30pt)
  $ theta in RR^(n times m) qqquad  theta in R^(k times k) $
]

#slide(title: "Convolutional networks - Convolutional Layers")[
  The Convolutional Layer:
  #figure(image("assets/image-11.png", width: 23em))
]

#slide(title: "Convolutional networks - Pooling")[
  Pooling:
  #figure(grid(columns: 2, rows:2,
    image("assets/image-9.png"), image("assets/image-10.png")  
  ))
]

#slide(title: "Residual connections")[
  #show math.equation: set text(30pt)
  
  $ f'_1(f'_2(x)) &-> 0 \
  #v(4em)
  f'_1(f'_2(x)+1)+1 &>= 1 $
]

#slide(title: "Batch/Layer Normalization")[
  #show math.equation: set text(25pt)
  
  #figure(grid(rows: 2, columns: 2, row-gutter: 2em,
    $ tilde(x)^(k)_i = (x^(k)_i - mu^(k)_B)/sqrt((sigma^(k)_B)^2 + eps) $,
    $ tilde(x)_i = (x_i - mu_s)/sqrt((sigma_s)^2 + eps) $,
    image("assets/image-12.png",width: 80%),
    image("assets/image-13.png",width: 100%)
  ))
]

#slide(title: "RNN")[
  #show math.equation: set text(25pt)
  
  $ h_t = tanh(W_(h h) dot h_(t-1) + W_(x h) · x_t + b). $
  $ #image("assets/image-14.png", width: 28em) $
]


#slide(title: "Long-range dependency")[
  #show math.equation: set text(25pt)
  Let $gam = norm(W_h)$.
  $ gam < 0 iimp ppv(cal(L),h) -> 0 quad ("vanishing"). $
  $ gam > 0 iimp ppv(cal(L),h) -> oo quad ("exploding"). $

  #v(2em)

  $ #[*The cat*, which was sitting on the mat  near \ the window in the cold room, *was hungry*.] $
]

#slide(title: "LSTM")[
  #show math.equation: set text(25pt)
  
  $ #image("assets/image-15.png") $
]

#slide(title: "LSTM")[
  #show math.equation: set text(25pt)
  
  $ f_t = sigma(W_f dot [h_(t-1), x_t] + b_f) $
  $ i_t = sigma(W_i dot vec(h_(t-1), x_t) + b_i) $
  $ hat(C)_t = tanh(W_C dot [h(t-1), x_t] + b_C) $
  $ C_t = f_t dot C_(t-1) + i_t dot hat(C)_t $
  $ o_t = sigma(W_o dot [h_(t-1), x_t] + b_o) $
  $ h_t = o_t dot tanh(C_t) $
]


