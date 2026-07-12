#import "@preview/typslides:1.3.3": *
#import "@local/tempst:0.1.0": *
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

#slide(title: "Parameter sharing")[
 
  #show math.equation: set text(40pt)
  #figure(image("assets/image-22.png"))
]

#slide(title: "Convolutional networks - Convolutional Layers")[
  The Convolutional Layer:
  #box(width: 100%, height: 70%)[
    #figure(image("assets/image-11.png", width: 23em))
    #place(top + left, dx: 16.2em, dy: 0.6em)[
      #text(fill: blue)[Kernel]
    ]
  ]
]



// #slide(title: "Convolutional networks - Convolutional Layers")[
//   The Convolutional Layer:

//   - 1D
//   $ (f * g)[n] = sum_(k) f[k] dot g[n-k] $
//   - 2D
//   $ (f * g)[n,m] = sum_(k_1=0)^(k-1) sum_(k_2=0)^(k-1) f[k_1,k_2] dot g[n-k_1,m-k_2] $
//   Where $k in ZZ^+$
//   $ k << n,m imp abs(RR^(k times k)) << abs(RR^(n times m)) $
// ]

#slide(title: "Convolutional networks - Convolutional Layers")[
  From my project $->$ translation invariance 
  $ #image("assets/image-34.png", width: 20em) $
]

#slide(title: "Convolutional networks - Pooling")[
  Pooling:
  #figure(grid(columns: 2, rows:2,
    image("assets/image-9.png"), image("assets/image-10.png")  
  ))
]

#slide(title: "Vanishing/exploding gradients")[
  #show math.equation: set text(25pt)
  Let $gam = norm(W_h)$.
  $ gam < 1 iimp ppv(cal(L),h) -> 0 quad ("vanishing"). $
  $ gam > 1 iimp ppv(cal(L),h) -> oo quad ("exploding"). $

  #v(2em)

  
]

#slide(title: "Residual connections")[
  #show math.equation: set text(30pt)

  $ h^((l+1)) = sigma(W dot h^((l)) +b) + h^((l)) $

  #v(1em)

  $ dv(h_(t+1),h_0) = dv(h_(t+1),h_(t)) greenmath(+1) dot dv(h_(t),h_(t-1)) greenmath(+1) dot dots dv(h_1,h_(0)) greenmath(+1) >= 1 $
  #show math.equation: set text(21pt)
  For gradients $apx 0$,
  #show math.equation: set text(30pt)
  $ nabla_(h^((l))) cal(L) = nabla_(h^((l+1))) cal(L) dot 1 = underbrace(nabla_(h^((l+1))) cal(L), "unchanged") $
]

#slide(title: "Batch/Layer Normalization")[
  #show math.equation: set text(25pt)
  
  #figure(grid(rows: 2, columns: 2, column-gutter: 4em, row-gutter: 2em,
    [BatchNorm], [LayerNorm],
    $ tilde(x)^(k)_i = (x^(k)_i - mu^(k)_B)/sqrt((sigma^(k)_B)^2 + eps) $,
    $ tilde(x)_i = (x_i - mu_s)/sqrt((sigma_s)^2 + eps) $,
    
  ))
  $ #image("assets/image-69.png", width: 18em) $
]

#slide(title: "Recurrent neural networks - RNN")[
  #show math.equation: set text(25pt)
  
  $ h_t = tanh(W_(h h) dot h_(t-1) + W_(x h) · x_t + b). $
  $ #image("assets/image-14.png", width: 28em) $
]


#slide(title: "Long-range dependency")[
  #show math.equation: set text(25pt)
  Recall that for $gam = norm(W_h)$.
  $ gam < 1 iimp ppv(cal(L),h) -> 0 quad ("vanishing"). $
 

  #v(2em)

  $ #[*The cat*, which was sitting on the mat  near \ the window in the cold room, *was hungry*.] $
]

#slide(title: "Long-short term memory - LSTM")[
  #show math.equation: set text(25pt)
  
  $ #image("assets/image-15.png") $
]

#slide(title: "Long-short term memory - LSTM")[
  #show math.equation: set text(25pt)
  From my project:
  $ #image("assets/image-35.png", width: 20em) $
]




