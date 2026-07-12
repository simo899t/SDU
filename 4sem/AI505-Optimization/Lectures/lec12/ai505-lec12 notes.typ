#import "@local/tempst:0.1.0": *
#show: note.with(
  title: "Lecture 12: Optimization in Machine Learning",
  author: "Simon Holm",
  date: "April - 2026"
)

// Your content starts here

= Theoretical Analysis - Preliminaries

We know that for 
$ F(w) = mycases(
  R_n (w) = 1/n = summ(i=1,n,f_i (w)),  "Emperical Risk",
  R(w) = EE_Xi [f(w;Xi)],               "Expected Risk"
  , word: ""
) $

#pseudo[
  *Procedure SG($dots$)*
  
  - Choose an inital iterate *$w_0$* 
  + *for* $k=0,1,dots$ *do*
    + Generate a realization of the random variable $Xi_k$;
    + Compute a stochastic vector $g(w_k, Xi;k )$;
    + Choose a stepsize $alpha_k >0$;
    + Set the new iterate as $w_(k+1) <- w_k - alpha g(w_k,Xi_k)$;
]


= Convergence Analysis - Assumptions



$ g(w_k, xi_k) = cases(
  nf((w_k;xi_k)),
  1/n_k summ(i=1, n_k, nf((w_k;xi_k))),
  H_k dot 1/n_k summ(i=1, n_k, nf((w_k;xi_k)))
) $


= Convergence Analysis - Results






= Computational Complexity Analysis






