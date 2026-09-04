#import "@local/tempst:0.1.0": *
#import "@preview/mitex:0.2.7": mi, mimath

#show: note.with(
  title: "Statistical Learning Theory",
  course: "AI512 — Introduction to Machine Learning",
  author: "Simon Holm",
  date: "2026-09-04",
)

= Generalization bounds

Remember the basic definitions below.

*Definition 5.1 (Generalization error)* Given a hypothesis #mi(`h \in \mathcal{H}`), a loss function #mi(`\ell: Y \times Y \rightarrow [0,1]`) a data distribution #mi(`x,y \sim D`), the generalization error of $h$ is defined as

#mimath(`R(h) = P_{x,y \sim D}[\ell(h(x),y)]`)


*Definition 5.2 (Empirical error)* Given a hypothesis #mi(`h \in \mathcal{H}`), a loss function #mi(`\ell: Y \times Y \rightarrow [0,1]`), and a data set #mi(`S = \{(x_1,y_1),...,(x_m,y_m)\}`). The empirical error of $h$ is defined as:
#mimath(`\widehat{R}_S(h) = \frac{1}{m} \sum_{i=1}^m \ell(h(x_i),y_i)`)

Using these definitions above and the concentration inequalities we covered in, 7. Inequalities we can derive the following theorem.

*Theorem 5.1 (Generalization bound)* Let #mi(`\mathcal{H}`) be a finite hypothesis set. Then for any #mi(`\delta \in (0,1)`), with probability at least #mi(`1-\delta`) over the choice of #mi(`S \sim D^m`), for all #mi(`h \in \mathcal{H}`)


#mimath(`R(h) \leq \widehat{R}_S(h) + \sqrt{\frac{\log |\mathcal{H}| + \log \frac{1}{\delta}}{2m}}`)

*Proof* is in Melih's lecture notes (I do not recommend ;))

This inequality is called a generalization bound, because it bounds the generalization error of any hypothesis #mi(`h \in \mathcal{H}`) in terms of its empirical error. We can derive a number of interesting consequences from this bound:

- The bound is uniform in the sense that it holds simultaneously for all hypotheses in #mi(`\mathcal{H}`).
- The bound is independent of the data distribution $D$ and the loss function #mi(`\ell`).
- The bound increases logarithmically with the size of the hypothesis set #mi(`|\mathcal{H}|`), that is, a richer hypothesis set is more likely to overfit.
- The bound decreases with the size of the training set $m$, that is, more data is less likely to overfit.
- For a fixed training set size $m$ and two different hypothesis sets #mi(`\mathcal{H}_1`) and #mi(`\mathcal{H}_2`), the bound prefers the smaller hypothesis set. This is known as *Occam's razor principle*. The principle is introduced by the 14th century theologian William of Ockham. It states that among competing hypotheses, the one with the fewest assumptions should be selected.

This all means that *True error ≤ training error + bias* for model complexity. Since training error gets smaller when training, with enough samples $m$, this penalty can be made as small as desired

- The bound gives a *Probably Approximately Correct (PAC)* performance guarantee. The event that any hypothesis in #mi(`\mathcal{H}`) is *approximately correct* in the sense that its generalization error is at most #mi(`\epsilon`) with probability at least #mi(`1-\delta`).


= PAC Learnability

=== Realizability
A hypothesis space #mi(`\mathcal{H}`) is realizable with respect to a loss #mi(`\ell`) and data distribution $D$ if 
$exists h^* in cal(H) " such that " R(h)=0$
It trivially follows from this definition that #mi(`\min_{h \in \mathcal{H}} \widehat{R}_S(h)=0`) with probability 1 for a sample set $S$ collected i.i.d. from $D$. 
Hence, under the realizability assumption, all Empirical Risk Minimization (ERM) solutions 
#mimath(`h_S \in \arg \min_{h \in \mathcal{H}} \widehat{R}_S(h)`)
 give zero error.

- In other words, the hypothesis class $cal(H)$ is *rich enough* to contain the “true function.” (label function)
- If this is true, it is *possible* to achieve zero training error *and* zero true error.

This is just an assumption that makes PAC learning easier.

=== Representativeness
A training set $S$ is called #mi(`\epsilon`)-representative if

#mimath(`\forall h \in \mathcal{H}, |R(h) - \widehat{R}_S(h)| \leq \epsilon`)

This means that all hypothesis in the h-space, their risk must be epsilon similar to the generalization error

Intuition: The dataset is $epsilon$-representative if there exists a hypothesis with *minimal* error.

=== Uniform convergence
A hypothesis class #mi(`\mathcal{H}`) has the *uniform convergence property* if there exists a function #mi(`m_{\mathcal{H}}^{\text{UC}} : (\epsilon, \delta)^2 \to \mathbb{N}`) such that for every #mi(`\epsilon, \delta \in (0, 1)`) and every distribution $D$, any sampled dataset #mi(`S=\{(x_i,y_i) \overset{i.i.d.}{\sim} D : i = 1, \ldots, m\}`) is #mi(`\epsilon`)-representative with probability at least #mi(`1-\delta`).

This means that there exists a function that can tell how many data points you would need for a hypothesis, to know that for all distributions. It is $epsilon-$ representative with a $1-delta$ probability certainty

=== Agnostic PAC learnability
A hypothesis class #mi(`\mathcal{H}`) is *agnostic PAC learnable* if there exist a function #mi(`m_{\mathcal{H}}: (\epsilon, \delta)^2 + h <- A(S)`) and a *learning algorithm* $A$ such that for every #mi(`\epsilon, \delta \in (0, 1)`) and every distribution $D$, running the learning algorithm on dataset #mi(`S=\{(x_i,y_i) \overset{i.i.d.}{\sim} D : i = 1, \ldots, m\}`) with #mi(`m \ge m_{\mathcal{H}}(\epsilon, \delta)`) satisfies the following


#mimath(`P(\{S \sim D: R(A(S)) \le \min_{h' \in \mathcal{H}} R(h') + \epsilon\}) \geq 1-\delta.`)

This means that a hypothesis class $cal(H)$ is *agnostic PAC learnable* if we can guarantee that with enough data, we can find a hypothesis that performs almost as well as the best possible hypothesis in our class.

1. *"Agnostic"* = We don't assume there's a perfect hypothesis in $cal(H)$ that makes zero errors. We're realistic about the fact that our hypothesis class might not contain the true underlying function.
	- (if $"min"_(cal(H))R(h) =0$ and $cal(H)$ is agn. PAC learnable, then $cal(H)$ is PAC learnable)

2. *"PAC"* = "Probably Approximately Correct" - we get performance guarantees that are:
    - *Probably*: With high probability (at least $1−delta$)
    - *Approximately*: Within $epsilon$ error of the best possible
    - *Correct*: We can make this guarantee

3. *The guarantee*: If we have enough training data $(m<=cal(H)(epsilon,delta)$), then our learning algorithm $A$ will find a hypothesis that performs within $epsilon$ of the best possible hypothesis in $H$.

*In practical terms:*
- $epsilon$ = How close we want to be to optimal (smaller = better)
- $delta$ = How confident we want to be (smaller = more confident)
- $m_(cal(H))(ϵ,δ)$ = Minimum number of training examples needed
- The algorithm guarantees: $R(A(S))<="min"_(h in cal(H))​R(h)+epsilon$ with probability $≥1−δ$

=== Bayes Error
Bayes error is the best achievable error once unavoidable factors like noise are taken into account.
$R^*="min"_(h´ in cal(H))R(h´)$
 This means that:
- Even with a perfect model and infinite data, you *cannot do better* than this error.
- The remaining error is due to *irreducible uncertainty*, such as:
    - noise in the data,
    - overlapping class distributions,
    - inherent randomness in the labels.
    
Then a hypothesis class is *PAC learnable* with respect to a data distribution $D$ if it admits zero Bayes error, i.e., $R^* = 0$.


= Bias-Complexity Dilemma (Trade-off)

=== No Free Lunch
Even though a perfect classifier $f$ exists (with $R(f)=0$), for any learning algorithm $A$ and some data distribution $D$.
If you randomly sample a training set $S$ from $D$, there is at least a $1/7$ chance that the algorithm $A$ will output a hypothesis with generalization error at least $1/8$.

No matter how good your algorithm is, there are situations (distributions and training sets) where it will fail to generalize well, even if a perfect solution exists. There is always a non-negligible probability of poor performance.

No algorithm is best at every task

// [image omitted: 8318F6F9-9A11-430C-BA13-35CC06D3F7AA.png]

The no free lunch theorem tells us only that we need to include a degree of bias to the learner. However, it does not tell anything about its consequences. Inducing too much bias limits the ability of the learner to explain the training observations. Inducing too little bias leads to overfitting. The goal is to find the right balance between the two. This dilemma is known as the *bias-complexity dilemma*. 

=== *Bias–complexity dilemma*
*Bias–complexity dilemma* = the trade-off between *how complex your model is* and *how well it generalizes*.

- *Simple model (low complexity)*
    - Cannot capture all the structure in the data → *high bias* / high approximation error
    - Very stable → *low variance* / low estimation error
    - Risk: underfitting
- *Complex model (high complexity)*
    - Can fit the data very well → *low bias* / low approximation error
    - Sensitive to training noise → *high variance* / high estimation error
    - Risk: overfitting

*Goal:* Pick the complexity where *total generalization error* is minimized.

Let us describe the bias-complexity dilemma in more formal terms. Given an ERM solution #mi(`h_S \in \arg \min_{h \in \mathcal{H}} \widehat{R}_S(h)`) for a hypothesis space #mi(`\mathcal{H}`), we can decompose its generalization error as below


#mimath(`\underbrace{R(h_S)}_{\text{Generalization error}} = \underbrace{\min_{h \in \mathcal{H}} R(h)}_{\text{Approximation error}} + \underbrace{\epsilon_{est}}_{\text{Estimation error}}`)

- *$R(h_S)$*: the true risk (expected error) of the learned hypothesis.
- *#mi(`\min_{h \in \mathcal{H}} R(h)`)*: The best possible risk within your chosen hypothesis space. This is also called the *approximation error*.
    - This comes from the fact that your hypothesis space #mi(`\mathcal{H}`) may not be rich enough to perfectly represent the true function.
    - No matter how much data you have, you can’t beat this.
- *#mi(`\epsilon_{\text{est}}`)*: the *estimation error*, the extra error due to having only a finite training sample $S$.
	- This decreases as you get more data, because your empirical risk minimizer $h_S$ gets closer to the true best #mi(`h^* \in \mathcal{H}`).
	- But if #mi(`\mathcal{H}`) is large/complex, #mi(`\epsilon_{\text{est}}`) is bigger because you risk overfitting the training data.
	- We can see #mi(`\epsilon_{est}`) as a kind of penalty (a *cost of complexity*)

Since for any #mi(`\mathcal{H'} \supset \mathcal{H}`), it holds that 
#mimath(`\min_{h' \in \mathcal{H}'} R(h') \leq \min_{h \in \mathcal{H}} R(h)`)
We can influence the approximation error by choosing different hypothesis from the hypothesis space. Our task is to traded off approximation and estimation error against each other. 

Increasing the hypothesis space, thereby making our solution more complex will reduce the approximation error, but will increase the estimation error. Since the realizability assumption (Realizability: 2. PAC Learnability) implies a smaller training error, we will observe overfitting in this scenario. The opposite will be true when we restrict the hypothesis space.

=== Bias-Variance decomposition
The bias-complexity dilemma can also be observed from the bias and variance of the estimated values of a regression output. 

Consider a regression problem where observations #mi(`(x,y) \sim D`). In this instance we want to estimate the conditional expectation of
$f^*(x)=EE(y|x)$

With the training set #mi(`S = \{(x_1,y_1),...,(x_m,y_m)\}`).

Our *estimator* for $EE[y|x]$ is a hypothesis #mi(`h_S \in \mathcal{H}`) that minimizes the mean squared error (MSE). 
The expected squared error of the prediction made by this estimator over the noisy label $y$ and training sample $S$ is given by:


#mimath(`E_{S, y|x} \Big [ \Big(y - h_{S}(x) \Big)^2 \Big ] =E_{S, y|x} \Big [ y^2 - 2 y h_{S}(x) + h_{S}(x)^2 \Big ]`)


#mi(`=E_\{y|x\} [ y^2] + E_\{S\} [ h_\{S\}(x)^2 ] - 2 E_\{y|x\} [ y ] E_\{S\} [ h_\{S\}(x) ]`)


#mi(`=E_\{y|x\} [ y]^2 + Var_\{y|x\}[y] + E_\{S\} [ h_\{S\}(x) ]^2 + Var_\{S\} [ h_\{S\}(x) ]- 2 E_\{y|x\} [ y ]E_\{S\} [ h_\{S\}(x) ]`)


#mimath(`{\text{This is the important part, its not crutial to undertstand everything here}}\over={\underbrace{\Big(E_{y|x} [ y ] -E_{S} [ h_{S}(x) ]\Big)^2}_{\text{Estimator~Bias}}  + \underbrace{Var_{S} [ h_{S}(x) ]}_{\text{Estimator~Variance}}+\underbrace{Var_{y|x}[y]}_{\text{Label~noise~variance}}}`)


Note that $E_\{S|x\}[h_S(x)] = E_S [h_S(x)]$ and $Var_\{S|x\}[h_S(x)] = Var_S[h_S(x)]$ since $S$ is collected independently from $x$.


= Vapnik - Chervonenkis (VC) Dimension

=== Restriction
Given #mi(`S = \{x_1, \ldots, x_m \} \subset X`), the following set

#mimath(`\mathcal{H}_S = \{ (h(x_1), \ldots, h(x_m)) : h \in \mathcal{H} \}`)


is called a *restriction* of #mi(`\mathcal{H}`) to $S$. We can do this by

$|cal(H)|=2^(|S|)$


In other words, restriction in the discrete-label case (*don't worry about continuous labels here, trust me*), a *restriction* means evaluating hypotheses *only on a finite dataset*. This produces a *finite set of distinct labelings*, even if the original hypothesis space is infinite.

- *Example*
Consider restricting simple dataset with 3 points:
#mi(`S=\{(1,1),(2,1),(1,2)\}`)

==== Finding the Restriction $cal(H)_S$
For each hypothesis $h in cal(H)$, we evaluate it on our 3 points to get a label vector $mat(h(x_1);h(x_2);h(x_3))$.
*Different linear classifiers might give:*
- Line 1: (+1,+1,+1) - all points positive
- Line 2: (+1,+1,−1) - first two positive, last negative
- Line 3: (+1,−1,+1) - first and last positive, middle negative
- Line 4: (+1,−1,−1) - only first positive
- Line 5: (−1,+1,+1) - only first negative
- Line 6: (−1,+1,−1) - only middle positive
- Line 7: (−1,−1,+1) - only last positive
- Line 8: (−1,−1,−1) - all points negative

*Key insight:* Even though $cal(H)$ has infinitely many lines, they can only produce *finitely many* distinct labelings on our 3-point dataset.



=== Shattering
#mi(`\mathcal{H}`) is said to *shatter* $S$ if #mi(`|\mathcal{H}_S|= 2^{|S|}`).

In words, a hypothesis class #mi(`\mathcal{H}`) shatters from a dataset $S$ if the restriction of #mi(`\mathcal{H}`) to $S$ is the set of all functions from $S$ to #mi(`\{0,1\}`). This means that hypothesis class $cal(H)$ is so expressive that it can achieve *every possible labeling* of the dataset $S$.
use

#mi(`d_(V C)="max"\{m:T_(cal(H))(m)=2^m\} quad 😮`)

*Intuition:* If your hypothesis class can shatter a dataset, it means your hypothesis class is "complex enough" to memorize any labeling of those points - even completely random noise!

*Learning Theory Connection:*
- *Good:* Expressive enough to capture complex patterns
- *Bad:* So expressive it can memorize noise → overfitting risk
- *Key insight:* There's a maximum dataset size your hypothesis class can shatter → this is the *VC dimension*

=== Growth function
The growth function, $tau_(cal(H)): NN^+->NN^+$ of $cal(H)$ is defined as

$tau_(cal(H))(m):=max_(S in X^m)|cal(H)_S|$

The expression above determines the max number of distinct labels in a model that uses a dataset that is on the domain $X$. 

In this case $cal(m)$ is the number of data points in the dataset (on $X$) that the model uses.
// [image omitted: assets/image.png]

When using the growth function to find a shatter-able dataset its to answer the question 
"*What’s the largest dataset that H can fully control?*"



=== VC dimension
The VC dimension of a hypothesis set #mi(`\mathcal{H}`) is the size of the largest dataset that #mi(`\mathcal{H}`) can shatter:

#mimath(`d_{VC}(\mathcal{H}) = \max \{m: \tau_{\mathcal{H}}(m) = 2^m\}`)

Note that if #mi(`d_{VC}(\mathcal{H})=\infty`) then #mi(`\mathcal{H}`) is not PAC learnable.



*Theorem 5.4 (The Fundamental Theorem of Statistical Learning).* Assume that #mi(`d_{VC}(\mathcal{H}) = d < \infty`). Then, there exist #mi(`C_1, C_2 \in \mathbb{R}^+`) such that

- #mi(`\mathcal{H}`) has the uniform convergence property with sample complexity
	
	#mi(`C_1 \frac{d + \log(1/\delta)}{\epsilon^2} \le m_{\mathcal{H}}^{\text{UC}}(\epsilon, \delta) \le C_2 \frac{d + \log(1/\delta)}{\epsilon^2}`)

- #mi(`\mathcal{H}`) is agnostic PAC learnable with sample complexity
	
	#mi(`C_1 \frac{d + \log(1/\delta)}{\epsilon^2} \le m_{\mathcal{H}}(\epsilon, \delta) \le C_2 \frac{d + \log(1/\delta)}{\epsilon^2}`)

- #mi(`\mathcal{H}`) is PAC learnable with sample complexity
	
	#mi(`C_1 \frac{d + \log(1/\delta)}{\epsilon} \le m_{\mathcal{H}}(\epsilon, \delta) \le C_2 \frac{d \log(1/\epsilon) + \log(1/\delta)}{\epsilon}`)


In other words, the following statements are equal:
- #mi(`\mathcal{H}`) has the uniform convergence property.
- Any ERM rule is a successful agnostic PAC learner for #mi(`\mathcal{H}`).
- #mi(`\mathcal{H}`) is agnostic PAC learnable.
- #mi(`\mathcal{H}`) is PAC learnable.
- Any ERM rule is a successful PAC learner for #mi(`\mathcal{H}`).
- #mi(`\mathcal{H}`) has a finite VC-dimension.


= Nonuniform Learnability

=== Formal definition
A hypothesis class #mi(`\mathcal{H}`) is *nonuniformly learnable* if there exist a function #mi(`m_{\mathcal{H}}^{NU}: (0, 1)^2 \times \mathcal{H} \to \mathbb{N}`) and a *learning algorithm* $A$ such that for every #mi(`\epsilon, \delta \in (0, 1)`) and every distribution $D$, running $A$ on data set #mi(`S=\{(x_i,y_i) \overset{i.i.d.}{\sim} D : i = 1, \ldots, m\}`) with #mi(`m \ge m_{\mathcal{H}}^{NU}(\epsilon, \delta, h)`) satisfies #mi(`P(\{S \sim D: R(A(S)) \le \min_{h' \in \mathcal{H}} R(h') + \epsilon\}) \geq 1-\delta.`)


Unlike uniform learnability (fx Agnostic PAC), where with enough data, the algorithm *can always find a hypothesis close to the best one in #mi(`\mathcal{H}`)*. This means that uniform learning prepares for the worst hypothesis, even if the data only needs a simple one.

In *Nonuniformly learnability* different hypotheses might need *different amounts of data*.

We *assume* that *for each* hypothesis #mi(`h \in \mathcal{H}`), there exists a *sample size #mi(`m_{\mathcal{H}}^{NU}(\epsilon, \delta, h)`)* that is sufficient to learn it:

$P(R(A(S)))<=R(h)+epsilon >=1-delta, quad "if" |S|<=m_(cal(H))^("NU")(epsilon, delta, h)$

Note that an agnostic PAC learnable hypothesis class #mi(`\mathcal{H}`) is also nonuniform learnable because 
#mimath(`R(A(S)) \leq \min_{h' \in \mathcal{H}} R(h') + \epsilon`)
 results *trivially* in.
#mimath(`R(A(S)) \leq R(h) + \epsilon, \forall h \in \mathcal{H}`)
Hence, the set of nonuniform learnable hypothesis classes is larger than the agnostic PAC learnable hypothesis classes. With this relaxation, our motivation is to increase the model capacity while sustaining its learnability.

If in uniform learning there is a very complex (bad) hypothesis in $cal(h)$, then sample complexity explodes (we need huge amounts of data), so we are forced to restrict $cal(H)$ (Restriction: 2. PAC Learnability) in order to keep learnability.

With this new concept that Each hypothesis $h$ has its *own required sample size*, simple hypotheses can be learned with few samples and the complex hypotheses are still allowed — they just “cost more data”

=== Intuition
With limited data, we restrict ourselves to simple hypotheses, then as data grows, we are allowed to consider more complex hypotheses.

