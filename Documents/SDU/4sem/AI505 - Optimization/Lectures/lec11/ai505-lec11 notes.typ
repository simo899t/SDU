#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 10: Population Based Methods",
  author: "Simon Holm",
  date: "March - 2026"
)

// Your content starts here

= Introduction

Large-scale machine learning represents an important application area of optimization.

Two case studies:
- Logistic regression or support vector machines convex optimization problems
- deep neural networks highly nonlinear and nonconvex problems

== Text Classification via Convex Optimization
Task: determining whether a text document is one that discusses politics.
- Set of examples ${(x_1,y_1), dots, (x_n,y_n)}$ where each $i in {1,dots,n}$. $x_1$ represents the features of a text document (e.g., the words it includes) $y_i$ is a label indicating whether the document belongs $(y_i = 1)$ or not $(y_i = -1)$ to a particular class

Wi minimize the *emperical risk*

$ R_n (h) = 1/n summ(i=1,n,II[h(x_i)!= y_i]), quad "where" II[A] = cases(1 "if" A "is true,", 0 "otherwise") $

== Formalization
$ h(x; w, tau) = w^T x + tau, w in RR^d "and" tau in RR $
This is the linear discriminator.

performance measure: count how many times $sign(h(x; w,τ))$ mispredicts. Discontinuous problem.

alternatively, define a continuous *loss function* $ell$ that measures a cost for predicting $h$ when the true label is y; e.g., one may choose a log-loss function of the form $ell(h,y) = log(1 + exp (-h y))$.

We then minimize the emperical risk
$ min_((2,tau) in RR^d times RR) 1/n summ(i=1,n,ell(h(x; w, tau),y_i)+ lambda/2 norm(w)_2^2) $
Then solve for various $lambda$ (on the validation set) to the best one.

== Deep Neural Networks

Deep Neural Networks: represent hypotheses as computation graphs with tunable weights and
compute the gradient of the loss function with respect to those weights in order to fit the
training data.

- Forward accumulation: compute prediction
- Backward accumulation: compute gradient

#link("https://playground.tensorflow.org/#activation=tanh&batchSize=10&dataset=circle&regDataset=reg-plane&learningRate=0.03&regularizationRate=0&noise=0&networkShape=4,2&seed=0.37877&showTestData=false&discretize=false&percTrainData=50&x=true&y=true&xTimesY=false&xSquared=false&ySquared=false&cosX=false&sinX=false&cosY=false&sinY=false&collectStats=false&problem=classification&initZero=false&hideText=false")[playground in tensorflow link]




= Fundamentals 

One should seek to find $h$ that yields a small expected risk of misclassification over all possible inputs, i.e., an h that minimizes
$ R(h) = P[h(x) != y] = EE[II[h(x) != y]] $


which is *variational* since we are optimizing over a set of functions (the h), and is stochastic
since the objective function involves an expectation

== Choice of Prediction Function
$cal(H)$ should contain prediction functions that are able to achieve a low empirical risk over the
training set, so as to avoid bias or underfitting the data.

We want to decrease the gap between $underbrace(R(h), "exp risk") - underbrace(R_n (h), "emp risk")$

Uniform laws of large numbers and the Hoeffding inequality gurantee that with probability at least
$ 1-eta $
$ sup_(h in cal(H)) abs(R(h) - R_n (h)) <=cal(O)(sqrt(1/(2n) log(2/eta) + (d_cal(H))/n log(n/(d_cal(H))))) $

= Simplified Notation

Let $xi$ be a random seed or the realization of a single (or a set of) sample $(x,y)$
For a given $(w, xi)$ let $f (w; xi)$ be the composition of the loss function ℓ and the prediction function h
Then:
$ underbrace(R(w) = EE_xi [f(w;xi)], "Expected Risk") $

Let ${xi_[i]}^n_(i=1)$ be realizations of $xi$ corresponding to ${(x_i , y_i)}^n_(i=1)$ and $f_i (w) def f(w;xi_[i])$

Then:

$ underbrace(R_n (w) def 1/n summ(i=1,n,f_i (w)), "Empirical Risk") $

= Convex function
#figure(
  image("assets/image.png"),
  caption: [The class of convex functions satisfies the following stability properties:]
)

= Stochastic vs Batch Optimization Methods
We want to minimize $R_n$ with $w_0 in RR^d$. Deterministic problem

*Stochastic Approach*: Stochastic Gradient 
$ w_(k+1) <- w_k - alpha_k nf_i_k (w_k) $
$i_k$ is chosen randomly from ${1,dots, n}$, $alpha_k > 0$
- very cheap iteration only on one sample.
- the direction might not always be a descent but if it is a descent direction in *expectation*, then the sequence ${w_k}$ can be guided toward a minimizer of $R_n$.

*Batch Approach*: batch gradient, steepest descent, full gradient method:
$ w_(k+1) <- w_k - alpha_k nabla R_n (w_k) = w_k - (alpha_k)/n summ(i=1,n,nf_i (w_k)) $
- more expensive
- can use all deterministic gradient-based optimization methods
- the sum structure opens up to parallelization

== Stochastic Gradient
#figure(
  image("assets/image-1.png", width: 30em),
  caption: [the fast initial improvement achieved by SG, followed by a drastic slowdown after 1 or 2 epochs, is common in practice. \
SG more sensitive to $alpha_k$ and starting point. If more epochs, batch may become better]
)

== Beyond SG: Noise Reduction and Second-Order Methods
$ w_(k+1) <- w_k - (alpha_k)/abs(cal(S)_k) sum_(i in cal(S)) nf_i_k (w_k) $#pagebreak()

$ F ) mycases(
  R(w) = EE_xi [f(w;xi)], "Emperical Risk"
  R_n (w) = 1/n summ(i=1,n,f_i (w)), "Expected Risk"
) $

$ g(w_k, xi_k) = cases(
  nf((w_k;xi_k)),
  1/n_k summ(i=1, n_k, nf((w_k;xi_k))),
  H_k dot 1/n_k summ(i=1, n_k, nf((w_k;xi_k)))
) $

#figure(
  image("assets/image-2.png"),
  caption: [on horizontal axis methods that try to improve rate of convergence\ on vertical axis, methods that try to overcome non-linearity and ill-conditioning]
)
