#import "@local/tempst:0.1.0": *

#note(
  title: "Lecture 4 - Regularization",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "February, 2026",
)

= Cross Entropy Loss
Measures how well one probability distribution approximates another.
$ H(p|q) = - sum_i^K p(y_i) dot log(q(y_i)) $

= Softmax
$ "softmax"(z)_i = e^(z_i) / (sum_j^K e^(z_j)) $

= KL Divergence
$ D_("KL")(P || Q) = sum_(x in cal(X)) P(x) dot log(P(x) / Q(x)) $

= Overcoming overfitting with regularization
+ Increase capacity until overfitting
+ Regularize model to reduce overfitting

== Different regularizers
- $L^2 = Omega(theta) = 1/2 norm(w)^2_2$
  - $J_1 (w; X, y) = alpha/2 w^T w + J(w; X, y)$
  - $nabla_w J_1 (w; X, y) = alpha w + nabla_w J(w; X, y)$
  - $w' <- (1 - epsilon alpha) w - epsilon nabla_w J(w; X, y)$
- $L^1 = Omega(theta) = sum_i |w_i|$
  - $J_1 (w; X, y) = alpha norm(w)_1 + J(w; X, y)$
  - $nabla_w J_1 (w; X, y) = alpha "sign"(w) + nabla_w J(w; X, y)$

= Early Stopping
Every time the error on the validation set improves, we store a copy of the model parameters.
When the training algorithm terminates, we return these parameters rather than the latest set.

== Use of a second training step
- Early stopping requires a validation set, so some training data is not fed to the model.
- To best exploit this extra data, one can perform extra training after the initial training with early stopping has completed.
- Two basic strategies for including the validation data:
  + Reinitialize the model to the untrained state and re-train on training *and* validation sets, with the identified optimal hyperparameters — including how long to train (number of epochs).
  + Continue training with the final model on the validation set (e.g. if dealing with very large models).

= Making synthetic data
Generate new samples just by transforming inputs.
- Images are high-dimensional and include a variety of variations; many are easily simulated.
- Translating the images a few pixels can greatly improve performance.
- Rotating and scaling are also effective.
- It may be more difficult in other areas (e.g. language).

- Invariance: $S(T(I)) = S(I)$
  // TODO: image (Pasted image 20260216113210.png)
- Equivariance: $S(T(I)) = T(S(I))$
  // TODO: image (Pasted image 20260216113531.png)

= Injecting Noise
To improve robustness, train with random noise applied to the inputs.

// TODO: image (Pasted image 20260216113635.png)

= Creating new Models by Removing Units
Dropout trains an ensemble of subnetworks.

By using dropout we can average multiple subnetworks, ultimately using much the same theory as making multiple models and averaging them.
