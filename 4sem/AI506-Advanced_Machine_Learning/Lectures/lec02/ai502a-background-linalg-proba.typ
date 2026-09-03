#import "@local/tempst:0.1.0": *

#note(
  title: "Lecture 2a - Background: Linear Algebra & Probability",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "February, 2026",
)

= Tensor
An array of numbers (like vectors or matrices) but with an arbitrary number of axes.
The number of axes is denoted the *rank* of a tensor.

== Example
Multiple images with colour codes:
$ T in RR^("batchsize" times h times w times c) $

= Sum rule
$ P(X = x_i) = sum_(j=1)^L P(X = x_i, Y = y_j) $
$ P(X) = sum_Y P(X, Y) $

= Product rule
$ P(X, Y) = P(Y|X) P(X) $

== Bayes' Theorem
$ underbrace(P(Y|X), "posterior") = (overbrace(P(X|Y), "likelihood") overbrace(P(Y), "prior")) / underbrace(P(X), "evidence") $

= Entropy
Degree of uncertainty.

Example: a fair die has high uncertainty (all events are equally likely);
a biased die has lower uncertainty.
