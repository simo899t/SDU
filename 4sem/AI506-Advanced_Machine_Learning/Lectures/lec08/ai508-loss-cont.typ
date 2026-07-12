#import "@local/tempst:0.1.0": *

#note(
  title: "Lecture 8 - Loss Functions cont.",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "March, 2026",
)

= Information Theory
Shannon's Information theory (1948)

Information should have the following desirable properties
- Continuity
- Symmetry
- Maximal value
- Additive

== Surprisal
Given some distribution $P(X)$, how can we caracterize how surprised we are when we observe a particular event. Intution: the smaller the probability of the event, the larger the surprise.
$ "Surprisal"(x) = 1/p(x) $
Then for multiple events
$ "Surprisal"(x,y) = 1/p(x) dot 1/p(y) $

Then for additive property $ h(x) = log(1/p(x)) = -log(p(x)) $

== Entropy
Lets average Shannon Information
$ H(x) = EE[h(x)] = - sum p(x) log(p(x)), $
also called "uncertainty of a random variable."

#figure(
  image("assets/image.png"),
  caption: [Example: Entropy of a biased coin with probability p]
)

= Cross-Entropy
This follows the idear behind the Cross-Entropy formular

How surprised are you on average, if you assume a distribution Q, when it's actually P.
$ H(P,Q) = EE_p[h(x)] = -sum p(x) log(q(x)) $


= Back to loss functions
Lets start of with logistic regression, where
$ m(x)=theta^T x = theta_0 + theta_1 x $
and $ sigma(m(x)) = (e^(theta_0+theta_1 x))/(1+ e^(theta_0+theta_1 x)) $

- *Log likelihood*
$ log(L(theta)) = sum_i y_i log(sigma(m(x:i)))+(1-y_i)log(1-sigma(m(x_i))) $
note that flipping it gets the *Binary Cross-Entropy Loss*:
$ cal(L)_"CE"(theta) = -log(L(theta)) = -sum_i y_i log(sigma(m(x:i)))+(1-y_i)log(1-sigma(m(x_i))) $

- *Relationship between KL Divergence and Cross Entropy*
$ D_"KL"(P||Q) = H(P,Q)-H(P) $
$ D_"KL"(P||Q) =  $

== Softmax loss
For multi-class classification with K classes, assume targets follow a Categorical distribution

then softmax $ sigma(m(x))_k = (e^(m(x)_k))/summ(j=1,K,e^(m(x)_j)) $

The likelihood of a single observation with one-hot label $y_i in {0,1}^K$
$ p(y_i|x_i, theta) = prod(k=1,K,sigma(m(x_i))_k^(y_(i k))) $

Now log-likelihood over all observations:
$ log(L(theta)) = sum_i sum_k y_(i k) log(sigma(m(x_i)))_k $

Negating it then gives cross-entropy loss:

$cal(L)_"CE"(theta) = -log(L(theta)) = -sum_i sum_k y_(i k) log(sigma(m(x_i)))_k $

== Mean-squared error loss
Assume targets are Gaussian: $y_i tilde cal(N)(m(x_i),sigma^2)$

$ p(y_i|x_i, theta) = 1/sqrt(2 pi sigma^2) exp(-((y_i - m(x_i))^2)/(2sigma^2)) $

Again log likelihood over all observations:
$ log(L(theta)) = -n/2 log(2 pi sigma^2) - 1/(2sigma^2) sum_i (y_i -m(x_i))^2 $

Maximizing log likelihood is equivalent to minimizing
$ cal(L)_"MSE" = sum_i (y_i - m(x_i))^2 $
(because constants can be ignored for minimum)

= Deep Learning intuition
== Shortcut-learning
Deep neural networks tend to find the simplest possible solution, even if it is a shortcut. Shortcut-learning can break models when models are not trained on misleading data. 
#figure(
  image("assets/image-1.png"),
  caption: [Left image is classified as a person running on a beach, because the training data had no images of cows on beaches. The right image was classified as an elephant. The images is a cat shape in elephant skin-like texture, showing that, for this model, it has some texture-bias.]
)

== The Lottery Ticket Hypothesis

== Risidual connections
Residual Networks Behave Like Ensembles of Relatively Shallow Networks when unfolded

#figure(
  image("assets/image-2.png"),
  caption: []
)

== Deep Double Descent
#figure(
  image("assets/image-3.png"),
  caption: []
)
#figure(
  image("assets/image-4.png"),
  caption: []
)

== Grokking
Grokking: a model first memorizes the training data, then — long after — suddenly learns to generalize

#figure(
  image("assets/image-5.png"),
  caption: []
)

#figure(
  image("assets/image-6.png"),
  
  caption: []
)

#figure(
  image("assets/image-7.png"),
  caption: []
)