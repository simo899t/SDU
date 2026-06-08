#import "@preview/typslides:1.3.3": *
#import "../../../temp/temp.typ": *
#show: typslides.with(
  ratio: "16-9",
  theme: "bluey",
)

#front-slide(
  title: "Autoencoders & Generative Models",
  subtitle: "AI506 — Advanced Machine Learning",
  authors: "Simon Holm",
  info: "May 2026",
)

#slide(title: "Autoencoder objective")[
  #figure(image("assets/AE.png", width: 12em))
  #show math.equation: set text(26pt)
  Let $f_theta$ be the encoder and $g_phi$ the decoder, then
  $ arg min_(theta,phi) loss = norm(x - g_phi (f_theta (x))) $
]

#slide(title: "Under/over-complete AE's")[
  #show math.equation: set text(30pt)
  $ f_theta : RR^d -> RR^k $
  #v(3em)
  #show math.equation: set text(26pt)
  #figure(grid(columns: 2, column-gutter: 5em, rows: 2, row-gutter: 2em,
  $ k<d $,
  $ k>=d $,
  [undercomplete],
  [overcomplete (trivial)]))
]

#slide(title: "De-noising AE's")[
  Corrupt input $x$ with random noise
  #show math.equation: set text(26pt)
  $ tilde(x) = x + eps where eps tilde cal(N)(0,sigma^2 I) $
  #show math.equation: set text(26pt) 
  Now
  $ arg min_(theta,phi) loss = norm(x - g_phi (f_theta (tilde(x)))) $

]

#slide(title: "Sparse AE's")[
  #show math.equation: set text(26pt) 

  Recall L1 regularization $ norm(z)_1 = sum_i abs(z_i) $

  Let $z= f_theta (x)$
  #v(1em)
  $ arg min_(theta,phi) loss = norm(x - g_phi (z)) + lambda norm(z)_1 $
]

#slide(title: "Variational AE's")[
  #show math.equation: set text(26pt) 
  Instead of the regular encoder let 
  $ f: RR^d -> (RR^k, RR^k) $
  #show math.equation: set text(21pt) 
  where $mu in RR^k$ and $sigma^2 in RR^k$
  #show math.equation: set text(26pt) 
  Now $ z tilde cal(N)(mu,sigma^2) $
]

#slide(title: "Variational AE's")[
  The decoder:
  #show math.equation: set text(26pt) 
  $ pred(x) = g_phi (z) where z tilde cal(N)(mu,sigma^2) $
  #show math.equation: set text(21pt) 
  When generating $z tilde cal(N)(0,I)$
  #show math.equation: set text(26pt) 
  We use $ arg min_(theta,phi) loss = norm(x - g_phi (z)) + underbrace(D_"KL" (cal(N)(mu,sigma^2) || cal(N)(0,I)), #[KL Divergence])  $
]


#slide(title: "Challenges - Backpropagate through samples")[
  We cant backpropagate through the sampling
  
  $ z = mu + sigma dot eps, where eps tilde cal(N)(0,1) $
  
]

#slide(title: "Challenges - Variational Lower Bound")[

  One could also compute $log p(x)$

  $ log p(x) = integral_(RR^k) p(x|z)p(z) dif z, quad "where "RR^k" is uncountably infinite" $

  Maximise a lower bound instead:

  $ log p(x) >= underbrace(EE_(q_phi (z|x)) [log p_theta (x|z)], "reconstruction") - underbrace(D_"KL" (q_phi (z|x) || p(z)), "KL divergence") $

  where:
  - $q_phi (z|x) = cal(N)(mu, sigma^2)$
  - $p_theta (x|z)$
  - $p(z) = cal(N)(0,I)$
]

#slide(title: "GANs")[
  #figure(image("assets/GAN.png"))
  
]

#slide(title: "GANs")[
  Let $D_phi : RR^d -> [0,1]$


  Let $G_theta (z)  in RR^d, where z tilde cal(N)(0,I)$.
  #v(2em)

  Then the objective
  $ min_theta max_phi (G_theta, D_phi) = EE_(x in p_"data") [log D_phi (X)] + EE_(z in p(z)) [log (1-D_phi (G_theta (z)))]  $
]


