#import "@local/tempst:0.1.0": *
#note(
  title: "Lecture 9 - RNNs",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "March, 2026",
)

= Text
Can be Can be interpreted as a sequence of words (or *tokens*)

Classic NLP tasks: Text categorization, sentiment analysis, translation

== Vocabulary construction
build words using symbols/letters, then indicise words as *tokens*

$ [underbrace("The",0),underbrace("quick",1), underbrace("blue",2), underbrace("fox",3), underbrace("is",4) ] $

Now $"'The fox is quick'" = [0,3,4,1]$

== How to obtain Embeddings?

- *Old way*: load pre-computed word embeddings (e.g. word2vec, fastText) [King-Queen]

- *Modern way*: Learn word embeddings jointly with the main task.
  - Start with random word embeddings and then learn word embeddings in the same way you learn the other weights of the neural network.

#figure(
  image("assets/image.png"),
  caption: [Embedding Table $in RR^(V times E)$]
)
Table lookup is *much faster* as it avoids *BIG* matrix multiplications

= Recurrent Neural Networks
family of neural networks for processing sequential data

- RNNs share parameters in a different way
  - Each member of output is a function of previous members of output
  - Each output produced using same update rule applied to previous outputs
  - This recurrent formulation results in sharing of parameters through a very deep computational graph

#figure(
  image("assets/image-1.png"),
  caption: [An unrolled RNN]
)

== RNNs share same weights across Time Step
To go from multi-layer networks to RNNs:
- Need to share parameters across different parts of a model
- Separate parameters for each value of input cannot generalize to sequence lengths not seen during training
- Share statistical strength across different sequence lengths and across different positions in time

== Problem of Long-Term Dependencies

2 examples

- Easy to predict last word in “the clouds are in the *sky*”
- I grew up in France...[an entire story]... I speak fluent *French*.”

In principle RNNs should handle it, but fail in practice. LSTMs offer a solution


= Backpropagation-though-time
Unfold the gomputational graph

A Computational Graph is a way to formalize the structure of a set of computations (Backpropagation in this case)
- Unfolding this graph results in sharing of parameters across a deep network structure

Say that $s^((t)) = f(s^((t-1)), theta)$
#figure(
  image("assets/image-2.png"),
  caption: [Classical dynamical system]
)

Lets include the last state as well

$ s^((t)) = f(s^((t-1)), x^((t)), theta) $

Now state now contains information about the whole past input sequence


= Three design patterns of RNNs
== Design 1
#figure(
  image("assets/image-3.png"),
  caption: [Design 1: Output: each time step; Recurrence: hidden units]
)
=== Forward Calculation

We need to specify initial state $h^((0))$, then for each time point:

$ a^((t)) = b 0 W h^((t-1)) + U x^((t)) $
$ h^((t)) = tanh(a^((t))) $
$ o^((t)) = c+V h^((t)) $
$ pred(y)^((t)) = softmax(o^((t))) $

The Loss function can simply defined as the sum of the loss at each timepoint:

$ L = sum_t L^((t)) = - sum_t log p_"model"(y^((t)) | {x^((1)), dots, x^((t))}) $

=== Gradient Calculation (backward)

Computing this loss function $wrt theta$ is very expensice

Requires forward propagation pass through the unrolled graph

Runtime is $bigo(tau)$ and cannot be reduced by parallelization


#figure(
  image("assets/image-6.png"),
  caption: [back-propagation through time or BPTT]
)

== Design 2
#figure(
  image("assets/image-4.png"),
  caption: [Design 2: Output: each time step; Recurrence: output units]
)


=== Recurrence from Output to Hidden

== Design 3
#figure(
  image("assets/image-5.png"),
  caption: [Design 3: Output: one at the end; Recurrence: hidden units]
)

= Teacher Forcing
#figure(
  image("assets/image-7.png"),
  caption: [Visualizing Teacher Forcing]
)
= LSTM (long short-term memory)

LSTMs are explicitly designed to avoid the long-term dependency problem

#figure(
  image("assets/image-9.png"),
  caption: [RNNs have the form of a repeating chain structure
]
)

#figure(
  image("assets/image-8.png"),
  caption: [LSTMs also have a chain structure]
)