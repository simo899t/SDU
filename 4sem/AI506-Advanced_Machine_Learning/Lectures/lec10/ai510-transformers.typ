#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 10 - Transformers",
  course: "AI506 — Advanced Machine Learning",
  author: "Simon Holm",
  date: "April, 2026",
)
#set heading(numbering: "1.1")
= Recap
Recap how word embeddings work
- Segment sequence into tokens
- can be optimized with gradient descent
Recap recurrent neural nets
- Powerful sequence models
- ABle to process arbitrary length inputs
- Parameter can be shared sharing across positions
- Drawbacks/Challenges:
  - Long-range dependencies are better, but still challenging

= The classic landscape One architecture per "community"

#figure(
  image("assets/image.png",width: 30em),
  caption: [before the transformer, every field had very different architechture]
)

= The Transformer's takeover
#figure(
  image("assets/image-1.png",width: 30em),
  caption: [after the transformer, most fields use this]
)

== Translation, learned alignment
Translation can be done in many ways,
#figure(
  image("assets/image-2.png",width: 15em),
  caption: [aritechture of translation model, which should generate word $y_t$ by input ${x_1,x_2,dots,x_T}$]
)

#figure(
  image("assets/image-3.png",width: 20em),
  caption: [Visualizing this model]
)
#pagebreak()

= Attention is all you need
Attention is a function similar to a soft key-value dictionary lookup
The attention is defined as $x = sum hat(a)_i v_i, quad "where each weight," hat(a)_i = q dot k_i $
typically normalized with $ softmax(hat(a_i))= (e^(hat(a)_i))/(sum_j e^(hat(a)_i)) $

Usually, $k$ and $v$ are derived from the same input $x$:
$ k = W_k dot v, quad v = W_v dot x $


then the query can come from a seperate input $y$ (another set). This could be relevant in tasks like translation
$ q=W_q dot y $
Or we can do "self-attention" (within the same set of words)
$ q=W_q dot x $



#figure(
  image("assets/image-4.png",width: 25em),
  caption: [Visualizing the "dictionary" of keys and queries for the output $z$]
)

We can use many queries, not just one
This can lead to the attention matrix
$ z_(1:M) = "Attn"(q_(1:M),x) = ["Attn"(q_(1),x) | "Attn"(q_(2),x) | dots | "Attn"(q_(M),x)] $
#pagebreak()

We usually use "multi-head", where we have multiple attentions for each token.This means the operation is repeated K times and the results are concatenated along the feature dimension. Ws differ
$ z_i = ["Attn"_1(q_(i),x), "Attn"_2(q_(i),x), dots, "Attn"_K(q_(i),x)] $

The most common see formulation is $ z = softmax((Q K')/sqrt(d_"key")) dot V $


#figure(
  image("assets/image-5.png", width: 30em),
  caption: [Visualizing the queries in attentions matricies]
)

#figure(
  image("assets/image-6.png", width: 30em),
  caption: [This procces can be splitup.
  - Input Embedding
  - Position embedding]
)
== input embedding
Input text is first split into pieces. Can be characters, word, "tokens" (Tokenization)

== Position Embedding
Many differnt ways of embedding the positions of tokens in the data. We add another embedding table, indexed by positions from $0$ (first position) to some maximum $n$

#figure(
  image("assets/image-7.png", width: 30em),
  caption: []
)
#pagebreak()

== Attention/self-attention
The input sequence is used to create queries, keys, and values!
As an example, the formular $ "Attention"(Q,K,V) = softmax((Q K^T)/sqrt(d_k)) dot V $
```py
class SelfAttention(nn.Module):
    def __init__(self, d_model, d_key):
        super().__init__()
        self.w_q = nn.Linear(d_model, d_key)
        self.w_k = nn.Linear(d_model, d_key)
        self.w_v = nn.Linear(d_model, d_model)

    def forward(self, x):
        q = self.w_q(x)
        v = self.w_v(x)
        k = self.w_k(x)
        
        def attention(Q,K,V):
            return F.softmax((Q @ torch.transpose(K, -2, -1))/sqrt(K.size(dim=-1)), dim=-1) @ V
        
        return attention(q,k,v)
``` 


== Multi-headed
More heads = more attention between parameter / more complexity

Example:
```py
class MultiHeadSelfAttention(nn.Module):
    def __init__(self, d_model, d_key, n_heads):
        super().__init__()
        self.heads = nn.ModuleList([SelfAttention(d_model, d_key) for _ in range(n_heads)])
        self.w_o = nn.Linear(n_heads * d_model, d_model)

    def forward(self, x):
        result = []
        for head in self.heads:
            result.append(head.forward(x))
        result = torch.cat(result,dim=-1)
        return self.w_o(result)
``` 

== Point-wise MLP
A simple MLP applied to each token individually:
$ z_i = W_2 GeLU(W_1 x + b_1) + b_2 $

Think of it as each token pondering for itself about what it has observed previously.

#pagebreak()
Example in @resid

== Residual connections <resid>
For each module, we add the input afterwards, to make shortcuts for the gradient.
$ z_i = "Module"(x_i) + x_i $

Example of both MLP and Risidual connection:
```py
class TransformerBlock(nn.Module):
    def __init__(self, d_model, d_key, n_heads, mlp_factor=4):
        super().__init__()
        self.ln1 = nn.LayerNorm(d_model)
        self.attn = MultiHeadSelfAttention(d_model, d_key, n_heads)
        self.ln2 = nn.LayerNorm(d_model)

        self.mlp = nn.Sequential(
            nn.Linear(d_model, mlp_factor * d_model),
            nn.SiLU(),  # Swish activation function, f(x) = x * sigmoid(x)
            nn.Linear(mlp_factor * d_model, d_model)
        )

    def forward(self, x):
        # pre-norm <-- most common
        x = self.attn(self.ln1(x)) + x
        x = self.mlp(self.ln2(x)) + x

        # post-norm
        # x = self.ln1(self.attn() + x)
        # x = self.ln2(self.mlp() + x)
        return x
``` 

== Residual connections
For each module, we add the input afterwards, to make shortcuts for the gradient.
$ z_i = "Module"(x_i) + x_i $

== LayerNorm
We normalize the current representation of the layer

- there both *post-norm*
$ z_i = "LN"("Module"(x_i)+x_i) $
- and *pre-norm*
$ z_i = "Module"("LN"(x_i))+x_i) $

Example:
```py
class TransformerClassifier(nn.Module):
    def __init__(self, n_embeds, n_classes, d_model=256, d_key=64, n_heads=4, mlp_factor=4, n_layers=2):
        super().__init__()
        self.token_embedding = nn.Embedding(n_embeds, d_model)
        self.transformer_model = nn.Sequential(*[TransformerBlock(d_model, d_key, n_heads, mlp_factor) for _ in range(n_layers)])
        self.final_layer_norm = nn.LayerNorm(d_model)
        self.classifier = nn.Sequential(nn.Linear(d_model, d_model), nn.SiLU(), nn.Linear(d_model, n_classes))

    def forward(self, x):
        x = self.token_embedding(x)
        x = self.transformer_model(x)
        x = torch.mean(x, -2)
        x = self.final_layer_norm(x)
        x = self.classifier(x)
        return x
``` 

== At training time: Masked self-attention
This is regular self-attention as in the encoder, to process what's been decoded so far

training on each $p(z_3|z_2,z_1,x)$, is slow.

Instead we can set zero out all attention weigts after each word, so
$ hat(A):"masked" = A_"raw" dot M, quad"where" M = mat(0,0,dots,0;, 1,0,dots,0;) $


== types of Transformer architechtures
- Encode only
  - only classification for sentence level task
  - like BERT which can extract information from sentences.
  - "Masked Language Modeling"
  - Cross out some words in a sentence, encoding architechture should fill out the blanks
- Decode-only
  - prediction of tokens
  - "Masked Span Prediction"
  - Cross out a sequance of words in a sentence, decoding architechture should fill out the blanks
- Encode-Decode
  - sequence of task (translation)
  - "Next Token Prediction"
  - Predict the next token
  - Called the "Language Modeling Objective"

