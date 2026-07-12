#import "@local/tempst:0.1.0": *
#show: note.with(
  title: "Lecture 14 - Graph Neural Networks",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "April, 2026",
)
// #set heading(numbering: "1.1")

= Graph Neural Networks (GNNs)

#figure(
  image("assets/image.png"),
  caption: [a) Molecules, b) Traffic networks, c) Citation networks]
)

== Common tasks on graphs

- Node classification: Predict the class of each node.
- Graph classification: Predict the class of an entire graph. (e.g., classify a molecule as toxic or non-toxic) (maybe even identify the toxic part of the molecule with subgraphs)
- Link prediction: Predict the (future) existence of an edge. (how likely is it that two people will become friends in a social network? e.g. two nodes linking)

== Structure of a graph
- The adjacency matrix $A$ encodes the structure of the graph.
- Nodes can share features, which are stored in a feature vector $V-> RR^d$.
- Feature vectors can store information about the nodes, such as their mass (the molecules example).

#grid(
  columns: 3,
  figure(
    image("assets/image-3.png"),
    caption: [Arbitrary graph structure]
  ),
  figure(image("assets/image-2.png"),caption: [Adjacency matrix]),
  figure(
    image("assets/image-1.png"),
    caption: [Permutation invariance]
  )
)
The adjacency matrix encodes the structure of the graph. This structure is invariant to the ordering of the nodes, which means that the same graph can be represented by different adjacency matrices depending on how the nodes are ordered. This is permutation invariance.
#pagebreak()

= Graph-structured data

For graph-structured neural networks, we might need to adapt the architecture to handle the graph structure. 
For example, we can use Convolution operations on the graph.

We use #link("https://arxiv.org/abs/1609.02907")[*Neighborhood Aggregation*], where we aggregate information from a node's neighbors to update the node's representation. 
#figure(
  image("assets/image-4.png"),
  caption: [Neighborhood Aggregation on graphs\
  $ {x_1,x_2,...,x_k} "vectors" -> x_1 $]
)

= Neural message passing
set $h_u^((0))$ to the input feature vector of node $u$.

Then $ h_u^((k+1)) = "UPDATE"(h_u^((k)), h_u^((k)) | m_(cal(N)(u))^(k)) $
Where $m_(cal(N)(u))^(k) = "AGGREGATE"({h_v^((k)) : v in cal(N)(u)})$ is the message from the neighbors of node $u$ at iteration $k$. This can be some variation of pooling, such as mean, sum, or max pooling.)

After running $K$ iterations, we can use the output of the final layer to define the embeddings for each node.
  $ z_u = h_u^((K)), forall u in V $

- The basic graph neural network

$ h_u^((k)) = sigma (W_"self"^((k)) h_u^((k-1)) + W_"neigh"^((k)) sum_(v in cal(N)(u)) h_v^((k-1)) + b^((k))) $

$ "UPDATE"(h_u^((k)), m_(cal(N)(u))^(k)) = W_"self"^((k)) h_u^((k-1)) + W_"neigh"^((k)) m_(cal(N)(u))^(k) $
where $ m_(cal(N)(u))^(k) = "AGGREGATE"({h_v^((k-1)) : v in cal(N)(u)}) = sum_(v in cal(N)(u)) h_v^((k-1)) $

== Neighborhood aggregation with normalization
$ m_(cal(N)(u))^(k) = sum_(v in cal(N)(u)) (h_v)/sqrt(abs(cal(N)(u)) dot abs(cal(N)(v))) $
Alternatively, use the number of in-going edges for normalization:
$ m_(cal(N)(u))^(k) = sum_(v in cal(N)(u)) (sum_(v in cal(N)(u)) h_v)/(|cal(N)(v)|) $

= #link("https://arxiv.org/abs/1806.03536")[Jumping knowledge]
- Oversmoothing: After many iterations, the node representations can become indistinguishable. This is very prevalent in GNNs, especially when the graph is large and has many layers and the features are more complex and have more connections (compared to fx an image pooling).
- Jumping knowledge networks allow the model to "jump" to any of the previous layers' outputs, which can help mitigate oversmoothing.

$ x = "MLP"(x plus.o h^((1)) plus.o h^((2)) plus.o ... plus.o h^((K))) $


= #link("https://arxiv.org/abs/1710.10903")[Graph Attention (GAT)]

In basic graph neural networks, all edges are considered to
have equal weight. However, in many cases, some neighbors may be more important than others. Graph Attention Networks (GAT) introduce an attention mechanism to assign different weights to different neighbors.

$ m_(cal(N)(u))^(k) = sum_(v in cal(N)(u)) alpha_(u,v) h_v $
where $ alpha_(u,v) $ is the attention weight assigned to neighbor $v$ of node $u$.
$ alpha_(u,v) = softmax(a^top [W h_u plus.o W h_v])) quad (dim = cal(N)(u)) $
where $a$ is a learnable weight vector and $W$ is a learnable weight matrix.  