#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 1: Introduction",
  course: "AI509 - Natrual Language Processing",
  date: "Fall - 2026"
)

// content start here

= Introduction
- Lucas (Main teacher)
- Peter (Co-teacher)

= lectures
Lectures will stop at around week 42
+ LLM Architectures
+ LLM Pre-training
+ LLM Post-training
+ LLM Agents
+ LLM Efficiency
+ LLM Evaluation

== _special sessions_
We will also have some special session which could include something like: 
- Interpretability
- Efficiency
- Agents
- Reasoning.
- Multilingualism

= Default project: Model organism
We can select whatever project we want as long as Lukas approves, however we have a default.

1. Select a _quirky_ behavior
2. Identify what datasets to train on to induce _quirky_ behavior into the model, and what training paradigm is best suited (SFT,RLVR)
3. Define how to measure success, develop evals
4. Run the experiments  

= Natural Language Processing
- Text Classification
- Text Similarity
- Text Summarization
- Natural Language Inference
- Named entity recognition
- Grammatical Error Correction
- Machine Translation
- Dialog systems
- Question answering
- $dots$ (many more)

We just use transformers now.

= Language modeling
Guess $x_t$ by
#boxed($ p(x_t mid x_(<t)) $)