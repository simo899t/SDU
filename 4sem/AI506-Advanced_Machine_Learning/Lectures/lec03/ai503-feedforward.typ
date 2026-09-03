#import "@local/tempst:0.1.0": *

#note(
  title: "Lecture 3 - Feedforward Networks",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "February, 2026",
)

= Activation functions
- softmax
- ELU
- SELU
- softplus
- ReLU — $x = max(0, x_i)$
- sigmoid
- Tanh

// TODO: image (Pasted image 20260211153158.png)

= Common output units
- *Linear units:* no non-linearity (used for regression)
- *Sigmoid units:* each individual output is between 0 and 1 (used for many-out-of-$K$ classification)
  - can assign multiple classes
- *Softmax units:* each individual output is between 0 and 1 and all outputs together sum to 1 (e.g. used for one-out-of-$K$ classification)
  - can only choose one class
