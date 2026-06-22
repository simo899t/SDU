#import "../../../../../../temp.typ": *

#note(
  title: "Lecture 7 - Loss Functions",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "March, 2026",
)

= Intro
Why do we mostly use simple loss function like Mean Square Error (MSE) or Cross-Entropy Loss.

= Generative models
There are two schools of modelling for a probabilistic model of our data ${x_n, t_n}$
== Schools
=== Generative school
Builds models for the joint distribution $p(x,t)$. This is difficult becaue we'll have to learn the distribution of the data

=== Discriminative school 
Builds models for the conditional distribution $p(t|x).$
This is much easier, because we don't care about $p(x)$, but this is also often less powerful

== Estimating Classes
Our main goal is to estimate the probability that the bottle is truly a Barolo. Using Bayes:
$ p(C_1|x)=(p(x|C_1)p(C_1))/p(x) = (p(x|C_1)p(C_1))/(p(x|C_1)p(C_1) + dots +p(x|C_n)p(C_n))) $

#figure(
  image("assets/image-41.png"),
  caption: [Choose a threshhold for decision making]
)

=== A Closer look at the formalar
Take an exmaple with 2 Classes

$ p(C_1|x)=(p(x|C_1)p(C_1))/p(x) = (p(x|C_1)p(C_1))/(p(x|C_1)p(C_1) + p(x|C_2)p(C_2))) $

We can then take the ratio of $(p(x|C_1)p(C_1))/(p(x|C_2)p(C_2))$

then to contain it from $[0,oo]->[-oo,oo]$ using log

$ a(x) = ln((p(x|C_1)p(C_1))/(p(x|C_2)p(C_2))) $

then to $[0,1]$

$ sigma(a(x)) = 1/(1+exp(-a)) = e^a/(e^a+1) $

#figure(
  image("assets/image-42.png"),
  caption: [sigmoid function contained within range: $[0,1]$]
)

== Maximum likelihood estimator
$ theta_"MLE" = arg max_theta log(prod(i=1,n,p(x_i|theta))) = arg max_theta summ(i=1,n,log(p(x_i|theta))) $



