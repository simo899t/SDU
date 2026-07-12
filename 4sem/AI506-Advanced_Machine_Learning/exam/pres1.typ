#import "@preview/typslides:1.3.3": *
#import "@local/tempst:0.1.0": *

#show: typslides.with(
  ratio: "16-9",
  theme: "bluey",
)


#front-slide(
  title: "Feedforward Networks, Loss functions & Regularization",
  subtitle: "AI506 — Advanced Machine Learning",
  authors: "Simon Holm",
  info: "May 2026",
)

#slide(title: "Feedforward nets")[

  #figure(image("assets/image-1.png", width: 15em), caption: [Feedforward net illustration])
  #show math.equation: set text(25pt)
  #align(center)[
    $ f(x) = sigma(W x + b) $
  ]
]


#slide(title: "Universal Approximation Theorem")[

  Let $f : X -> Y$
  #v(1em)
  $ #definition(title: [#align(left)[Definition: Universal Approximation Theorem]], width: 70%)[
  #align(left)[
    #set text(size: 20pt)
    #show math.equation: set text(23pt)
  For any compact continuous function $f$, an approximation function $pred(f)$ and an $eps > 0$, then
  #show math.equation: set text(25pt)
    $ abs(f(x) - pred(f)(x)) < epsilon quad forall x in cal(X) $
  ] 
] $ ]

#show math.equation: set text(30pt)
#slide(title: "Feedforward nets")[

#let imgsize = 9em
  #figure(
    grid(columns: 3, rows: 3,
    column-gutter:2em,
    row-gutter: 2em,
  [Sigmoid], [ReLU], [GeLU],
  image("assets/image-66.png"),
  image("assets/image-67.png"),
  image("assets/image-68.png"),
  
  
    )
  )
]
  
#show math.equation: set text(30pt)

#slide(title: "Loss functions - Mean Squared Error")[
  #figure(grid(
    columns: 1,
    row-gutter: 1.5em,
    [*Mean Squared Error*],
    $ ell = (hat(y) - y)^2 $,
    $ cal(L) = 1/N sum_i (hat(y)_(i) - y_(i))^2 $,
    )
  )
]
#slide(title: "Loss functions - Cross Entropy")[
  #figure(
    grid(columns: 1, rows: 4,
    column-gutter:3em,
    row-gutter: 2em,
  [Cross Entropy],
  $ ell= - y log (h_theta (x)) where y in {0,1} $,
  $ ell_c= - sum_c y_(c) log (h_theta (x_(c))) $,

  $ cal(L) = - sum_i sum_c y_(i,c) log (h_theta (x_(i,c))) $,
  
  
  )
  )
] 

#slide(title: "Loss functions - Binary Cross Entropy")[
  #figure(
    grid(columns: 1, rows: 2,
    column-gutter:3em,
    row-gutter: 2em,
  [Binary Cross Entropy],
  $ cal(L) = - sum_i [y_i log hat(p)_i + (1 - y_i) log(1 - hat(p)_i)] $
  
  )
  )
] 

#slide(title: "Loss functions - Maximum Likelihood Estimation")[
  #figure(
    grid(columns: 1, rows: 3,
    column-gutter:3em,
    row-gutter: 2em,
  [Maximum Likelihood Estimation],
  $ pred(theta) = arg max_theta underbrace(sum_i log p(pred(y)_i | x_i, theta), -loss) $,
  $ hat(theta) = arg min_theta cal(L)(theta)  = arg min_theta - sum_i log p(pred(y) | x_i, theta) $
    )
  )
]

#slide(title: "Loss functions - MLE & CE")[
  
  $ cal(L)_"MLE" &= - sum_i log p(y_i | x_i, theta) \
    &= - sum_i sum_c log p(y_(i,c) | x_i, theta) \
    &= - sum_i log product_c h_theta (x_(i,c)) since y_(i,c) in {0,1} \
    &= - sum_i sum_c y_(i,c) log h_theta (x_(i,c)) \
    &= cal(L)_"CE" $
]

#slide(title: "Regularization")[
  #figure(image("assets/image-30.png"))
]

#slide(title: "Regularization - Early stopping")[
  #figure(image("assets/image-31.png"))
]

#slide(title: "Regularization - Bagging & boosting")[
  #figure(image("assets/image-33.png"))
]

#slide(title: "Regularization - L1 & L2")[
  #figure(
    grid(columns: 2, rows: 2,
    column-gutter:3em,
    row-gutter: 2em,
  [L1 (Absolute)], [L2 (Squared)],
  $ norm(w)_"L1" = lambda sum_i abs(w_i) $,
  $ norm(w)_"L2" = lambda sum_i w_i^2 $  
    )
  )

  #v(2em)
  Let $lambda$ be a weight decay parameter and $alpha$ be the stepsize for GD. Lets apply L2.
  $ w' = (1-lambda alpha)w - alpha nabla cal(L)(w,X,y) $
]

#let imgsize = 16em
#slide(title: "Regularization - Dropout")[

  #figure(grid(columns: 2, rows: 1,
  image("assets/image-5.png", width: imgsize), 
  image("assets/image-6.png", width: imgsize)
  ))
]

#slide(title: "Regularization - Data augmentation")[

  #figure(image("assets/image-8.png",width: 30em))
]
