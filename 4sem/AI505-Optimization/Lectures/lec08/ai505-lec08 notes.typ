
#let title = "Lecture 8: Beyond Local Optima"
#let author = "Simon Holm"
#let date = "March - 2026"

#import "@local/sdu-notes:0.1.0": *

#show: note.with(
  title: title,
  author: author,
  date: date
)

// Your content starts here

= Benchmarking

= Stochastic Methods
- Randomness can help escape local minima
- Control over randomness and the exploration vs exploitation trade off.

== Noisy descent
Saddle points, where the gradient is very close to zero, can cause descent methods to select step sizes that are too small to be useful

We add Gaussian noise at each descent step

$ x_(k+1) = x_k + alpha nf(x_k) + eps_k, quad eps_k tilde cal(N)(0,sigma_k^2) $
- $sigma_k = 1/k$

== Stochastic Gradient Descent
We can approximately evaluate gradients using randomly chosen subsets of the training data (batches)

This is much less computationally exprensive than calculating the true gradient at every interation.
It yields approximately the same effect as noisy graident approximation.

This ensures that the positive step sizes be chosen such that:
$ summ(k=1,oo,a_k)=oo,quad summ(k=1,oo,alpha_k^2) < oo $

Allows the step sizes to decrease and allow the method to converge, but not too quickly so
as to become stuck away from a local minimum

#figure(
  image("assets/image-67.png"),
  caption: []

)
#pagebreak()

== Mesh Adaptive Direct Search
Similar to generalized pattern search but uses random positive spanning directions

Example: set of positive spanning sets constructed from nonzero directions $d_1, d_2 ∈ {-1, 0, 1}$.

#figure(
  image("assets/image.png"),
  caption: []
)

== Simulated Annealing
often used on functions with many local minima due to its ability to escape local minima.

A candidate transition from $x$ to $x'$ is sampled from a transition distribution $T$ , eg,
multivariate Gaussian

$ x' = x+eps, quad eps tilde T $

- *Metropolis acceptance criterion:*

$ p(x,x') = mycases(
  1, del <= 0,
  e^(-del/t_k), del > 0,
  word: "if"
) $


== Simulated annealing


Where $del = f(x')-f(x)$ is the change in the change in the objective value and $t_k$ is the *temperature* at iteration $k$.

We can also do variable step-size v (seperate direction components) in order to not use covariance matrix as a hyperparameter.

A cycle of random moves, one in each direction

$ x' = x + r v_i e_i $

where $r$ is randomly sampled from ${-1,1}$

Then after $n_s$ cycles, adjust the step size
$ mycases(
  v_i(1+c_i ((a_i)/(n_s)-0.6)/(0.4)), a_i > 0.6 n_s,
  v_i(1+c_i (0.4-(a_i)/(n_s))/(0.4))^-1, a_i < 0.4 n_s,
  v_i, "otherwise",
  word: "if"
) $


=== Annealing plan 
$t_k$ can decrease in different ways

- logarithmic annealing schedule
  $ t_(k+1)=t_0 dot ln(n)/ln(k+1) $
- exponential annealing schedule
  $ t_(k+1) = gam t_k $
- fast annealing
  $ t_(k+1) = t_0/k $

We can apply the decrease at different times.

Either at every proposition (either accepted or rejected), every move or at every $m_t$ iteration.

== Cross-Entropy Method
Cross-entropy is a measure of divergence between two probability distributions $p$ and $q$. We measure cross-entropy in a case where one distribution (the one of optimal solutions)
is unknown.

A model is created and then its cross-entropy is measured on the elite set to assess how
accurate the model is in predicting this set.


== Multivariate normal distribution



== Solution of Max Likelihood

== Natural Evolution Strategies

Similar to cross-entropy method, except instead of parameterizing distribution based on elite samples, it is optimized using gradient descent.

We know from statistics (Introduction to Machine Learning /w Melih) that
$ EE[f(X)] = integral_RR x dot p(x) $
then 
$ EE[f(x)] = integral_RR f(x) dot p(x|theta) $

== Covariance Matrix Adaptation Evolutionary Strategy (CMA-ES)

Uses the same approach as  natural evolution strategy and cross entropy method, but the proposal distribution is a multivariate Gaussian parameterized by a covariance matrix.

At every iteration, $m$ designs are sampled from the multivariate Gaussian:

$ x tilde cal(N)(mu, sigma^2 Sigma) $
Mean vector $mu$ and a covariance matric $Sigma$ with a step-size scalar $sigma$

The scalar ensures that the covariance only increases or decreases in a single direction witch every iteration.












== Cross Entropy Method

Cross-entropy is a measure of divergence between two probability distributions $p$ and $q$

#figure(
  image("image.png")
)

By iteratively chooseing the best candidates from a distribution, and updating that distribution a new distribution to the elite set using MLE and repeating until some convergence, the distribution moves closer and closer to the optimum.

- Limitiations
  - Only considers top fraction $->$ ignores potentially useful information from other samples
  - Can make abrupt jumps, which may be unstable or slow to adapt
== Natural Evolution Strategies
Similar to CEM but this time we divergence

$ J(theta) = EE_(x tilde)[f(x)] $

