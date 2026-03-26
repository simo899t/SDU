#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 9: Population Based Methods",
  author: "Simon Holm",
  date: "March - 2026"
)

// Your content starts here

= Population Methods
Instead of optimizing a single design point, population methods optimize a collection of *individuals*. We can increase the number of individuals to prevent the algoriuthm from bering stuck in a local minimum

- Population methods begin with an initial population
- The initial population are usually uniform, normal distribution, or Cauchy distribution

#figure(
  image("assets/image.png", width: 30em),
  caption: [Different distribusions for initializations of a population]
)

== Genetic algorithms
Inspired by biological evolution where the fittest individuals pass their genetic information to the next generation

- Here individuals are interpreted as chromosomes
- The fittest individuals are determined by *selection*
- The next generation is formed by selecting the fittest individuals and performing *crossover* and *mutation*

Algorithms like this are called *Darwinian Evolution methods*

#figure(
  image("assets/image-6.png",width: 30em),
  caption: [Genetic algorithm with truncation selection, single point crossover, and Gaussian mutation applied
to Michalewicz function $ f(x) = - summ(i=1,d,sin(x_i)sin^(2m)((i x_i^2)/pi)) $]
)
#pagebreak()

=== Chromosomes
Chromosomes are typically initialized randomly (like a distribusion)
#figure(
  image("assets/image-1.png",width: 30em),
  caption: [Simple representation of chromosomes as a binary string. In this case a chromosome can either be 1 or 0 (red or blue)]
)
Chromosomes are more commonly represented as real-valued chromosomes which are simply
real-valued vectors


=== Selection
This processes determines which individuals pass their genetic information on to the next generation choosing chromosomes to use as parents for the next generation

- *Truncation selection:* kill the lowest performers (best fit score)
- *Tournament selection* selects fittest out of $k$ randomly chosen individuals
- *Roulette Wheel selection* individuals are chosen with probability proportional to their fitness
$ p = rho_i/(sum_i rho_i) $

#figure(
  image("assets/image-2.png",width: 30em),
  caption: [Exmaple of selection. Note that for this exmaple  $1/"fittness"= 1-1/y$  ]
)
#pagebreak()

=== Crossover/Recombination
Combines the chromosomes of the parents to form children

- Single-point crossover: swap occurs after single crossover point
$ #image("assets/image-3.png",width: 30em) $
- Two-point crossover: two crossover points
$ #image("assets/image-4.png",width: 30em) $
- Uniform crossover: each bit has 50% chance of crossover
$ #image("assets/image-5.png",width: 30em) $

=== Mutation
Mutation supports exploration of new areas of design space

Each bit or real-valued element has a probability (*mutation rate*) of being flipped or modified by noise

== Differential Evolution
Improves each individual $x$ by recombining other individuals according to a simple formula

+ Choose three random, distinct individuals a, b, and c
+ Construct interim design $z = a + w(b-c)$
+ Choose a random dimension to optimize in
+ Construct candidate $x'$ via binary crossover of $x'$ and $z$ $ mycases(z_i, i=j "or with probability" p, x_i, "otherwise", word: "if") $
+ Insert better design between $x$ and $x'$ into next generation


#figure(
  image("assets/image-7.png",width: 30em),
  caption: [Example of differential evolution]
)


== Particle Swarm Optimization
Each individual, or particle, tracks the following
- Current position
- Current velocity
- Best position seen so far by the particle
- Best position seen so far by any particle

At each iteration, these factors produce *force* and *momentum* effects to determine the movement of a particle p from the population:

$ x_i^p <- x_i^p + v_i^p $
$ v_i^p <- w v_i^p + c_1 r_1 (x_i^(p, "best") - x_i^p) + c_2 r_2  (x_i^("best") - x_i^p) $

#figure(
  image("assets/image-8.png"),
  caption: [Example of particle swarm optimization]
)
There are differnt kinds of swarm algorihtm: https://fcampelo.github.io/EC-Bestiary/


== Hybrid Methods
Generally, these methods (Darwinian Evolution methods) are good at finding the best regions in design space, but do not perform as well as descent methods near the minimizer

_(Some giraffes are born with longer necks. They access more food. They reproduce more. Long-neck genes
spread.)_

Hybrid methods try to leverage the strength of both methods

Here are two hybrids:
