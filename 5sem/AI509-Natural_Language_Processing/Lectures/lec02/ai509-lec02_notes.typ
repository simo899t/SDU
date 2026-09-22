#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 2: ",
  course: "AI509 - Natural Language Processing",
  date: "Fall - 2026"
)

// content start here
= Mixture of Experts (MoE) 
From teh switch transformer back in 2022. A modification in the feed forward layer where we store multiple feed forward layers (experts) and a router which learns which expert is most relevant for the input. In standard MoE, parameters are not shared across transformer layers.

+ Add a routing mechanism $W_R$ to determine which expert is the best fit.

  Probability that expert i is best suited for token representation $x$
$ p_i (x) = (e^(h(x)_i))/(sum_(j=1)^(N) e^(h(x)_j)) where h(x) = W_r accent(x,dot) $

+ Only the top-k experts (feedforward modules) get to process a given example

+ Outputs of these experts are aggregated according to routing scores
$ y = sum_(I=T) p_i (x) E_i (x) $
where $E_i$ is expert number $i$ and $p_i (x)$ is the estimated probability that expert $I$ is the best fit.

= DeepSeek Sparse Attention (DSA)
Based on the 'lightning' indexer. Computes the index score $I_(t,s)$ between teh query token $h_t in RR^d$ and a preceding token $h_s in RR^d$, determining which tokens to be selected by the query token: 
$ I_(t,s) sum_(j=1)^(H^I) w_(t,j)^I dot ReLU(q_(t,j)^I dot k_(t,j)^I) $
Then the attention output $u_t$ is computed by applying the attention mechanism between the query token $h_t$ and a sparsely selected key-value entries ${c_s}$:
$ u_t = "Attn"(h_t, {c_s mid I_(t,s) in "Top-k"(I_(t,:))}) $

In training, both a warmup-stage (with no cutoff) is done, then afterwards a sparse-stage training with k-cutoff  
#pagebreak()

= Rotary position embedding #link("https://arxiv.org/abs/2104.09864")[RopE]
$ f_{q,k} (x_m,m) = R^d_(Theta,m) W_{q,k} x_m $
where
$ R_(Theta,m)^d = mat(
  cos m theta_1, -sin m theta_1, 0, 0, dots, 0, 0;
  sin m theta_1, cos m theta_1, 0, 0, dots, 0, 0;
  0, 0, cos m theta_2, -sin m theta_2, dots, 0, 0;
  0, 0, sin m theta_2, cos m theta_2, dots, 0, 0;
  dots.v, dots.v, dots.v, dots.v, dots.down, dots.v, dots.v;
  0, 0, 0, 0, dots, cos m theta_(d\/2), -sin m theta_(d\/2);
  0, 0, 0, 0, dots, sin m theta_(d\/2), cos m theta_(d\/2);
) $
Then 
$ tran(q)_m k_n = tran((R_(Theta,m)^d W_q x_m)) (R_(Theta,n)^d W_k x_n) = tran(x)W_q R^d_(Theta,n-m) W_k x_n $

= Multi-head Latent Attention
Revisit standard multi-head attention.

One could also slice one big matrix $W_q$ into $n$ heads $[q_1, q_2,dots,q_n]$

Now introduce latent variables $c_t^Q$ and $c_t^(K V)$. These variables allow to split queries and keys, so only some part of them are applied with RopE.

Because of the linkage between keys and values, we used cached latent variables specifically for $c_t^(K V)$. _you can think of the latent variables as a learnable parameter which learns which parts of the queries have position-relevant context and which have not._

This is nice, because you then do not need to recalculate the attention for $x_(>t)$ when $x_t$ appears in generation.

= Multi-token predictions

#figure(
  image("assets/image.png"),
  caption: [Multi-token predictions with multiple heads],
)

Inventors just used 1 head since it performed good enough. GLM 5 used parameter sharing in the heads.
#pagebreak()

= Muon Optimizer
Muon = MomentUm Orthogonalized by Newton-Schulz

#pseudo[
  *Algorithm* MuonClip Optimizer
  + *for* each training step $t$ *do*
    + *for* each weight $W in R^(n times m$ *do*
      + $M_t = mu M_(t-1) + G_t$
      + $O_t = "Newton-Schulz"(M_t) dot sqrt(max(n,m)) dot 0.2$
      + $W_t = W_(t-1) - eta (O_t + lam (W_(t-1)))$
    + *end for*
    + *for* each attention head $h$ in every attention layer of the model *do*
      + Obtain $S^h_max$ already computed during forward
        + *if* $S^h_max > tau$ *then*
          + $gam <- tau \/ S^h_max$
          + $W_(q c)^h <- W_(q c)^h dot sqrt(gam)$
          + $W_(k c)^h <- W_(k c)^h dot sqrt(gam)$
          + $W_(q r)^h <- W_(q r)^h dot gam$
        + *end if*
    + *end for*
  + *end for*
]
- Muon excels at token efficiency 
- But can be a bit unstable
- Therefore, every other new LLM paper proposes some small variation to Muon
- Also; Muon is only used for feedforward layer parameters. Embeddings still use AdamW.

= RMS Normalization
Assume mean is $0$ for LayerNorm, so we only normalize variance
$ y_i  = x_i / "RMS"(x) dot gam_i where "RMS"(x) = sqrt(eps + 1/n sum_(i=1)^(n) x^2_i) $

= SwiGLU
A mix of Swish and GLU
$ "FFN"(x) = ("Swish"_beta (x W_1) dot (x V))W_2 $