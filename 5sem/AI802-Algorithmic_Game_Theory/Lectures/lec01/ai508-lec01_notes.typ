#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         "Lecture 1: Introduction",
  course:        "AI508 — Algorithmic Game Theory",
  author:        "Simon Holm",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

= What is Game Theory
#definition(title: "Definition: Game Theory (very brief)")[
  Created by John Nash, Game Theory is the study of decision making in settings with multiple rational agents whose outcomes depend on each other's choices.
]

#definition(title: "Definition: Algorithmic Game Theory (also very brief)")[
  Studying Game Theory with a computational lense
]

= System/Mechanism Design
Design a good system such that it cannot be exploited easily (#link("https://www.youtube.com/watch?v=7mq1ioqiWEo")[2012 olympics badminton china vs korea])

= Cooperations and Incentives
Given something like the #link("https://unfccc.int/process-and-meetings/the-kyoto-protocol")[The Kyoto Protocol], where all countries should reduce the global $"C"_"O"2$ emission. Could it be beneficial for some countries to just keep emitting and let others reduce, given the cost and how much others reduce.

= Image Generation with GAN's
$ min_theta max_phi (G_theta, D_phi) = EE_(x in p_"data") [log D_phi (X)] + EE_(z in p(z)) [log (1-D_phi (G_theta (z)))] $

= What we will cover
- Auctions
- Selfish routing
- Bimatrix games and equilibria
- Decentralized learning pf equilibria
- Introduction to minmax optimization and GAN's
- Cooperative games and Shapley values (week 46)
- Extensive form games and analyzing (Kuhn) Poker

= Learning outcomes
- Mathematically formulate any (relevant) problem as a multivalent learning problem
- Analyze effectiveness of systems/mechanisms
- In real-world strategic scenarios, identify the players and the game
- Identify the information available and use a relevant algorithm
- Identify 

= Relevant Material
*Game Theory Basics:* Lecture Notes on *Algorithmic Game Theory* by Tim Roughgarden, *Algorithm Design and Analysis* by Noam Nisan, Tim Roughgarden, Éva Tardos.

*Misc:* Relevant papers/readings will be posted for certain weeks

For further information/exercises: #link("https://timroughgarden.org/f13/f13.html")[CS364A: Algorithmic Game Theory]

= Exam
29th of january