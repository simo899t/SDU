#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 5: Reasoning in Latent Space",
  course: "AI509 - Natural Language Processing",
  date: "Fall - 2026"
)

// content start here
= Key challenge
Reasoning in token space is expensive

= Recurrent depth (latent recursion)
Apply a tranformer block repeatedly to shape the residual stream.

Usually the transformer can be written notationally given an embedding function $ "embed"(dot) : RR^abs(V) to RR^d $

and a model
$ model^L (dot) = cal(T)_theta_(L-1) compose cal(T)_theta_L compose dots compose cal(T)_theta_0 (dot) $

Where each $cal(T)_theta$ is a transformer block

using a unembed function $ "unembed"(dot) : RR^d to RR^abs(V) $

A standard transformer would then be

$ "unembed" compose model^L compose "embed"(dot) $

== Looped transformers
Use shared weights when stacking transformers
#figure(
  image("assets/image-1.png", width: 70%)
)

The looped transformer essensially does
$ "unembed" compose underbrace(model^L compose model^L compose dots compose model^L,"repeat" t "times") compose "embed"(dot) $

Some also use prelude and coda
$ "unembed" compose cal(C)_L compose underbrace(model^L compose model^L compose dots compose model^L,"repeat" t "times") compose cal(P)_L compose "embed"(dot) $

= Adaptive computation (dynamic depth)
Idea: Some tasks need more reasoning than others.

This method prepares the model to dynamically adjust the depth of the reasoning by training a linear module "early-exit", which decides when to exit the recurrence based on the current hidden state.

= Soft chain-of-thought models
Assume a standart _autoregressive_ transformer

Sample $x_t$ format$ "unembed" compose model compose "embed"(x_(<t)) $
Then continue predicting $x_(t+1)$ by feeding back $x_t$ to the input (autoregressive decoding)

== Chain of continuos thought (Coconut #emoji.coconut)
Tale the latent state $z_t$ from$ model^L compose "embed"(x_(<t)) $
then continue with predicting $z_(t+1)$ from
$ model ^L (["embed" (x_(<t); z_t)]) $

_Note: no sampling, coconut switches between latent space and token space via special tokens_

#pagebreak()

== Full bandwidth tranformer
Coconuts switching between the soft chain of thought

Instead do autoregression in both latent and token space.

$ z_t = model^L compose (["embed"(x_(<t); z_(<t))]), $
and sampled token $x_t from "unembed"$

#figure(
  image("assets/image-2.png", width: 70%),
  caption: [Standard decoding vs Latent feedback decoding],
)


= System One model (Decision Models)
Many tasks do not need complex reasoning

Uses *fast*, *calibrated*, *typed decisions*
- Calibrated: The decision comes with an estimated probability that should reflect the accuracy of the model.
- Fast: Just a single forward pass.
- Typed decisions: boil tasks down to *Noul*, *Choice* and *Score*. By design the model will never violate this schema

