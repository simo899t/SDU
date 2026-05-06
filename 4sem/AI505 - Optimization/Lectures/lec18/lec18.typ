#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 15: Discrete Optimization",
  author: "Simon Holm",
  date: "April - 2026"
)

// Your content starts here

= The ROAR-NET API: Constructive Search

== Community Detection Problem
A fully-connected undirected graph
 - vertices $v$ must represent users
 - Weighted edges represent the intensity of some attribute of their interaction
  
Positive weight indicate affinity between users, while negative edge weights indicate lack of affinity. For this groups of users connected mostly by positively weighted edges suggest the existence of a community involving those individuals.

#figure(image("assets/image-6.png"))

*Goal* Partition the vertices into subsets while maximizing the total weight of the edges within the groups (cliques)
- *Clique-partitioning problem*

We need to define the problem and the API functions to be implemented
#figure(image("assets/image-7.png"))


  
