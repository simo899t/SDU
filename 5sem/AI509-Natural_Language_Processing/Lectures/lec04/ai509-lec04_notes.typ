#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 4: Agents and Evaluation",
  course: "AI509 - Natural Language Processing",
  date: "Fall - 2026"
)

// content start here
= Surprisal
From surprisal over cross-entropy to perplexity
$ "Surprisal"(x_i) = - log p(x_i mid x_1,dots,x_(i-1)) #comment[surprisal pr token] $
$ loss_"CE" (X) = 1/N sum_(i=1)^(N) log p(w_i mid x_1,dots,x_(i-1)) #comment[average surprisal] $
$ "PPL"(X) = exp(loss_"CE"(X)) #comment[exponential of average surprisal] $

= Task-specific fine-tuning
Fine-tune a pre-trained model for one specific downstream task (just a specific problem which could be important).

= Few-shot prompting
Insert a few examples of the task in the prompt to learn on the fly (like while decoding). 
#example(title: "Example: few-shot prompting")[
  ask to classify dogs and salmons, but in the prompt give examples of their classification, then at the end ask it to classify "shark"
]

= Zero-shot prompting
There is just the question, and no examples of how to tackle the question.

Most difficult, but also most representative of how people interact with language models on a day-to-day basis.

= Chain of thought
#figure(
  image("assets/image.png"),
  caption: [Use chain of thought while generating an answer, to get better answers],
)
#pagebreak()

= Evaluations
== Datasets
- Measuring Massive Multitask Language Understanding (MMLU)
  #figure(
    image("assets/image2.png"),
    caption: [multiple choice tasks to test logic and language understanding],
  )
- Ai2 Reasoning Challenge (ARC)
  #figure(
    image("assets/image3.png"),
    caption: [very similar to MMLU, but there are many versions],
  )
#pagebreak()

- #link("https://lastexam.ai/")[Humanity's Last Exam]
  #figure(
    image("assets/image4.png"),
    caption: [In contrast to other datasets, HLE is made up of the most difficult questions which professors from universities around the world.],
  )
  
== Metrics
How does one measure correctness. One needs to decide if the metric should cover the entire answer. Does the full answer match exactly with the ground truth?

Given the "Example: few-shot prompting" from earlier. Is "Shark: 2" correct or "Answer: two"?

== Constraints
One can constraint metric prompts like \"Use `#boxed[]` your final answer\"

= Agents
Initial use:
If a model doesn't know the answer, it will go online and answer based on search engine answers.

- Modern agent: an iterate took use and reasoning

== Toolformer
#figure(
    image("assets/image5.png"),
    caption: [Toolformer was a model which learned itself how to use external tools through application programming interfaces (APIs)],
  )

== ReAct
An agent iteratively observes $o_i$ in its environment and taking actions $a_i$. The
context at time becomes:
$ c_i = (o_1, a_1, dots, o_(t-1), a_(t-1),o_t) $
- The key idea of *ReAct*: Action space now includes an action in the language space $hat(a)$ (called a thought / reasoning trace)

Thoughts does not affect the environment. They only updates the context to do planning or to reflect about previous actions/observations: $ c_(t+1) = (c_t,hat(a)_t) $

= Memory (like MemGPT)
Logs memory in a working context.
#figure(
    image("assets/image6.png"),
    caption: [Writes data to persistent memory after it receives a system alert about limited context space],
  )
Can then recall context learned by searching for keywords.

