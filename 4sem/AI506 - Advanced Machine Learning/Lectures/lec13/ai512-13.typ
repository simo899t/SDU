#import "../../../../temp/temp.typ": *
#note(
  title: "Lecture 12 - Generative Models",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "April, 2026",
)
// #set heading(numbering: "1.1")

= Basics
The goal is to learn the underlying data-generating distribution $p(x)$, such that you can sample from it.

== Energy-based Models
Idea: let's learn an arbitrary model to predict probabilities $p(x)$

$ p(x) = (e^(beta f_theta (x)))/(Z(theta)) $

Where Z is the *partition function* used for normalizing the output of
the energy-based model to form a probability distribution.

$ Z = integral_(x in cal(X)) e^(beta f_theta (x)) dx $

Notice how this closely resembles softmax, how ever this is the continuous space.

= Probabilistic Graphical Models

== Directed Graphs
- A set of nodes

- A set of edges, each connecting two nodes. In probabilistic graphical models:

- Each node is a random variable

- Each edge is a probabilistic relationship between the random variable

- Directed Acyclic Graphs (DAGs) are used to represent Bayesian networks, where the direction of the edges indicates the direction of the probabilistic dependencies between the random variables.

- Markov Networks are represented using undirected graphs, where the edges represent the dependencies between the random variables without any specific direction.

== Factorization
Any joint probability distribution
can be factorized.
$ p(a,b,c) = p(c|a,b)p(a,b) $
and further
$ p(a,b,c) = p(c|a,b)p(b,a)p(a) $
#pagebreak()

#example(title: [A complex example], [
  Consider this following directed graph
  #figure(
    image("assets/image-5.png"),
    caption: [A complex example of a directed graphical model.]
  )
  Nodes can be discrete (e.g., Bernoulli) or continuous (e.g., Gaussian)
  #align($
  p(x_1,x_2,x_3)\
  p(x_1)p(x_2)p(x_3)\
  p(x_4|x_1,x_2,x_3)p(x_5|x_1,x_3)\
  p(x_6|x_4)p(x_7|x_4,x_5)
  $) 
])
== Inference
We can update $p(x)$

since we have some prior distribution $p(x)$, the

$ p(x|y) = (p(y|x)p(x))/(p(y)) "(Bayes' theorem)" $

Then update $p(x)$


== Expectation-Maximization algorithm
1. Initialize the parameters $theta$ of the model
2. Expectation step: Calculate the probability  of each data point belonging to each possible hidden variable based on the current parameters $theta$
3. Maximization step: Update the parameters $theta$ to maximize the likelihood of the data.
4. Repeat steps 2 and 3 until parameter stability.


*Key Problem* Explaining away effect

Two independent causes can become conditionally dependent given their common effect.

*_Example_* Earthquake and burglary are independent causes of an alarm. However, if the alarm goes off, the probability of both earthquake and burglary increases, making them conditionally dependent.

== Major families of probabilistic graphical models
=== Bayesian networks
Using directed acyclic graphs to represent the conditional dependencies between random variables. Each node represents a random variable, and each directed edge represents a conditional dependency.

=== Markov networks
Undirected graphical models that represent the joint distribution of a set of random variables. They use undirected edges to represent dependencies between variables.


=== Restricted Boltzmann Machines
#figure(
  image("assets/image.png"),
  caption: [These are similar to autoencoders, but they are generative models, and they have a different architecture.]
)

- Probabilistic graphical models are different but similar to neural networks.
- Some concepts are carried over to neural nets (e.g., factorization, having latent variables, the term "hidden units")

== A Birds-Eye perspective
#figure(
  image("assets/image-1.png"),
  caption: [A birds-eye view of the relationship between different types of generative models.]
)

= Neural Networks as Generative Models

Decoder-only Transformers are Generative Sequence Models

The goal is given a dataset ${x_i}_(I<N)$, learn $p(x_1, x_2, ..., x_N)$

Ideal of sequence models $ p(x) = prod(I=0,N,p(x_i|x_(<i))) $

Now once $ z = mu + sigma * eps, quad "where" eps tilde cal(N) (0,1) $

Now the decoder does not need the encoder's output, but it can still learn to generate data by sampling from the latent space.

== Maximize Variational Lower-Bound
Consider a VAE with a probalistic encoder $q(z|x)$ and a probabilistic decoder $p(x|z)$. The goal is to maximize the likelihood of the data, which can be expressed as:
$ log(p_theta (x)) >= EE_(q(z|x))[log(p_theta (x|z))] - D_K(L) (q(z|x) || p(z))  $
Where $D_K(L)$ is the Kullback-Leibler divergence between the encoder's distribution and the prior distribution over the latent space. The first term encourages the decoder to reconstruct the input data accurately, while the second term regularizes the latent space to be close to the prior distribution.

#figure(
  image("assets/image-2.png"),
  caption: [The VAE architecture, where the encoder maps the input data to a latent space, and the decoder generates data from the latent space.]
)

= Generative Adversarial Networks
== Key idea of Generative Adversarial Networks (GANs)
#figure(
  image("assets/image-3.png"),
  caption: [The architecture of a GAN, where the generator tries to produce realistic data to fool the discriminator, while the discriminator tries to distinguish between real and generated data.]
)

== The GAN Training Algorithm
1. Sample real data points $x$ from the training dataset.
2. Sample random noise $z$ from a prior distribution (e.g.  Gaussian).
3. Apply the generator to the noise to produce a fake data point $G(z)$.
4. Apply the discriminator to both real and fake data points to get the probabilities of being real: $D(x)$ and $D(G(z))$.
5. Optimize a min max game
$ min_theta max_phi (G_theta, D_phi) (EE_(x∼p_(X))[log D_phi (X)] + E_(z∼p_z)[log(1 - D_phi (G_theta (z)))]) $x

#figure(
  image("assets/image-4.png"),
  caption: [Current Image generators are based on Diffusion models: More about that in the Computer Vision course #emoji.arrow.r.soon Fall]
)