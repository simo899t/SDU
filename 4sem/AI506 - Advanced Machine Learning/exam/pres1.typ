#import "@preview/typslides:1.3.3": *
#import "../../../temp/temp.typ": *

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

#slide(title: "Overview")[
  - Feedforward nets: *structure*, *activation functions*, universal *approximation*
  
  - Loss functions: *CE*, *MSE*, *MLE*
  - Regularization: *L1/L2* penalty on the weights, *dropout*, *data augmentation*
]

#slide(title: "Feedforward nets")[

  #figure(image("assets/image-1.png", width: 25em), caption: [Feedforward net illustration])
]

#slide(title: "Feedforward nets")[

  #figure(image("assets/image-1.png", width: 15em), caption: [Feedforward net illustration])
  #show math.equation: set text(25pt)
  #align(center)[
    $ f(x) = sigma(W x + b) $
  ]
]
#show math.equation: set text(30pt)
#slide(title: "Feedforward nets")[

#let imgsize = 9em
  #figure(
    grid(columns: 3, rows: 3,
    column-gutter:3em,
    row-gutter: 2em,
  [Sigmoid], [ReLU], [Tanh],
  image("assets/image-4.png",width: imgsize),
  image("assets/image-2.png",width: imgsize),
  image("assets/image-3.png",width: imgsize),
  $sigma(z) = 1/(1+e^(-z))$, $sigma(z) = max(0,z)$, $sigma(x) = (e^z -e^(-z))/(e^z+e^(-z))$,
  
  
  )
  )
]
    

#slide(title: "Universal Approximation Theorem")[
  #v(1em)
  $ #definition(title: [#align(left)[Definition: Universal Approximation Theorem]], width: 70%)[
  #align(left)[
    #set text(size: 20pt)
    #show math.equation: set text(23pt)
  For any compact continuous function $f$, an approximation function $pred(f)$ and an $eps > 0$, then
  #show math.equation: set text(25pt)
    $ sup_(x in cal(X)) = f(x) - pred(f)(x) < epsilon $
  ]
]
 $
]
#show math.equation: set text(30pt)
#slide(title: "Loss functions - Cross Entropy")[
  #figure(
    grid(columns: 1, rows: 2,
    column-gutter:3em,
    row-gutter: 2em,
  [Cross Entropy],
  $ L = - sum_k y_k log pred(p)_k $
  
  
  )
  )
] 

#slide(title: "Loss functions - Mean Squared Error")[
  #figure(
    grid(columns: 1, rows: 2,
    column-gutter:3em,
    row-gutter: 2em,
  [Mean Squared Error],
  $ L = sum_k (pred(y)_k - y_k)^2 $
  
  
  )
  )
]
#slide(title: "Loss functions - Maximum Likelihood Estimation")[
  #figure(
    grid(columns: 1, rows: 3,
    column-gutter:3em,
    row-gutter: 2em,
  [Maximum Likelihood Estimation],
  $ pred(theta) = arg max_theta sum_k log p(x_k | theta) $,
  $ hat(theta) = arg min_theta cal(L)(theta) where cal(L)(theta) =  -sum_k log p(x_k | theta) $
  
  
  )
  )
]

#slide(title: "Regularization - L1 & L2")[
  #figure(
    grid(columns: 2, rows: 2,
    column-gutter:3em,
    row-gutter: 2em,
  [L1 (Absolute)], [L2 (Squared)],
  $ sum_k norm(pred(y)_k - y_k) $,
  $ sum_k (pred(y)_k - y_k)^2 $ 
  

  
  )
  )
]
#let imgsize = 16em
#slide(title: "Regularization - Dropout")[
  #align(center,[Dropout])
  #figure(grid(columns: 2, rows: 1,
  image("assets/image-5.png", width: imgsize), 
  image("assets/image-6.png", width: imgsize)
  ))
]

#slide(title: "Regularization - Data argumentation")[
  #align(center,[Data argumentation])
  #figure(image("assets/image-8.png",width: 30em))
]
