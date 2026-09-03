#import "@local/tempst:0.1.0": *

#note(
  title: "Lecture 2b - NN Intro & Gradient-based Learning",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "February, 2026",
)

= Activation function for NN
$ sigma(X^T W + b) quad ==> quad sigma((sum x_i w_i) + b) $

Each transformation from one layer to the next requires a vector of weights.

// TODO: image (Pasted image 20260205113749.png)

= Gradient-based learning
Update the model parameters following the steepest slope of the loss function.
We use gradient descent:
$ f(x + epsilon) approx f(x) + epsilon nabla f(x) $

So with a loss function:
$ 1/N sum_X nabla_theta J(theta, x_i, y_i) = nabla_theta J(theta, X, y) $

then:
$ theta_("new") = theta_("old") - epsilon nabla_theta J(theta, X, y) $

// TODO: image (Pasted image 20260205115105.png)
