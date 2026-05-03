#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 14 - Graph Neural Networks",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "April, 2026",
)
// #set heading(numbering: "1.1")

= What makes a good explanation
Effective explanations must be selective (the explanation to a phenomenon might change depending on who is the receiver)
- Application-grounded.
Explanations are evaluated where the model is deployed. (very costly for training explanations)

- Human-grounded. 
Is it useful to humans? (1 billion parameters, might be an explanation but its not useful. So theres a tradeoff)

- Functionally-grounded
Does the explanation actually have something to do with what happens in the model? (is it made up or based on anything)


= Intrinsic vs. post-hoc interpretability
- Intrinsic: The system should have integrated explanations. (like using attention to explain choices)
- Post-hoc: Interpret the model after it has been trained


= Feature attribution
Search for explanations which tell us which input features matter the most for the model's output.

following are some common post-hoc methods
== LIME
Train a linear regression (which is interpretable!) to predict the output of the studied model

This prefers local approximations

== SHAP
Test all possible combinations of inputs, and observe when the model output changes.

= Model-specific approaches
"Just look at the attention values and take those as feature
attribution."

#figure(
  image("assets/image-5.png"),
  caption: [There are many more]
)

= Linear Probes
A (linear) probe is a (linear) classifier that takes the activations of another models as input and predicts a property of interest.

Monitor parts of the model, to see what is happening. 

= Causal Interpretability
Based on Causal Mediation Analysis (Pearl): Does $X$ cause $Y$ through some intermediate variable $Z$? (_golden gate bridge example_)

$Z$ can be seen as an intermediate hidden layer representation of a neural net.

= #link("https://arxiv.org/abs/2408.01416")[The right mediator?]

#figure(image("assets/image-6.png"))

= Mechanistic Interpretability
Reverse engineering neural networks by identifying features and circuits and doing causal analysis

"Mech Interp" goes beyond input features and
seeks to figure out what mechanisms in the
model leads to the output.

== #link("https://www.lesswrong.com/posts/AcKRB8wDpdaN6v6ru/interpreting-gpt-the-logit-lens")[The logit lens]
Prematurely apply the final language modeling head to intermediate representations of a transformer.
#figure(
  image("assets/image-7.png")
)

== #link("https://aclanthology.org/2024.acl-long.820/")[Do Llamas work in english]

Multilingual language models "think" in a concept space that happens to be closest to English (dominant training language)

#figure(
  image("assets/image-8.png")
)

== #link("https://transformer-circuits.pub/2023/monosemantic-features/index.html")[Sparse Autoencoders]
Train a sparse autoencoder on the activations of another neural network to learn meaningful features.

Intuition: one can train a new (more sparse) model on the activation in hope of a more interpretable model. 

== #link("https://proceedings.neurips.cc/paper_files/paper/2023/file/34e1dbe95d34d7ebaf99b9bcaeb5b2be-Paper-Conference.pdf")[Automated Circuit Discovery]

#figure(image("assets/image-9.png"))

== #link("https://arxiv.org/abs/2403.19647")[Sparse feature circuits]
SAEs + Circuit Discovery Pipeline
#figure(
  image("assets/image-10.png")
)
