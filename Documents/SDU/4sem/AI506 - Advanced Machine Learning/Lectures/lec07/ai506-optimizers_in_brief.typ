#import "../../../../../../temp.typ": *

#note(
  title: "Lecture 7 - Optimizers in brief",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "March, 2026",
)

= problems with optimizing deep nets

- Non-convex: We can end-up in local minima
- Inexact: Our gradient is not exact but only an estimate
- Poor correspondence between local and global structure (cliffs etc)
- The starting point matters
- The learning rate matters

= Minimizers

== AdamW
Adam with wight decay.

#figure(
  image("/assets/image-39.png"),
  caption: [This shows how AdamW is better, including warm restarts as well]
)

== Others
Described in AI505-lec05

== Current LR-schredules
A popular current schedule could be a combunation

#figure(
  image("/assets/image-40.png"),
  caption: [$ "warm up" -> "constant" -> "cooldown" $]
)