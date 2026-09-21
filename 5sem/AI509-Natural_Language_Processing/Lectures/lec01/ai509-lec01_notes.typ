#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 1: Encoding and token",
  course: "AI509 - Natrual Language Processing",
  date: "Fall - 2026"
)

// content start here

= Vocabulary
Given only one sequence of tokens, $s$ $ s = "the" mid "quick "mid" brown "mid" fox "mid" jumps "mid" over "mid" the "mid" lazy "mid" dog" $
The vocabulary is:
$ v = {&"\"the\"":0, "\"quick\"":1, "\"brown\"":2, "\"fox\"":3, \ &"\"jumps\"":4, "\"over\"":5,  "\"the\"":6, "\"lazy\"":7, "\"dog\"":8} $

Note that whitespace tokenization is suboptimal since workds not seen exactly in training is then not in vocabulary (like \"noooo\")

== Byte-pair encoding (BPE)
1. start by single char tokens.
2. Merge with the most likely consectutive two tokens, and add a new tokens for this merge.
3. repeat until desired vocabulary size

*Advanced tokenization tweaks*
- multi-word expression $to$ Enforce single word tokens
- number are messy $to$ Enforce single digits
- code leading with whitespaces $to$ Enforce 1, 2 or 4 length whitespace tokens.

For new texts you can commonly just greedy add the longest tokens matching with the vocabulary.