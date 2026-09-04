#import "@local/tempst:0.1.0": *
#import "@preview/mitex:0.2.7": mi, mimath

#show: note.with(
  title: "Basic Concepts",
  course: "AI512 — Introduction to Machine Learning",
  author: "Simon Holm",
  date: "2026-09-04",
)

= Formal definitions

== Formal definitions
“A computer program is said to learn from experience E with respect to some class of tasks T and performance measure P, if its performance at tasks in T, as measured in P, improves with experience E”. [Mitchell, 1997]

So
=== Experience E
Experience $E$ comes from the dataset itself $S=\{(x_1,y_1),(x_2,y_2),dots,(x_n,y_n)\}$.

The goal is to find a hypothesis that accurately represents #mi(`S:=\{(x_i, y_i) \overset{i.i.d}{\sim} D\}`)
=== Task T
The task is to predict $f(x_1) = y_i$ with $h(x_i) approx y_i$

$f(x)approx h(x),forall x in X$

Task can either be a *Classification* or a *Regression* dependent on whether Y is a continuous set (like $RR$)
=== Performance measure P
Performance is measure via a loss(risk) function $L:Y times Y -> RR^+$ 
Where 
$ell(y,y´)$

where $y$ is the true value of $y$ and #mi(`\hat{y}`) is the prediction of $y$

Loss (risk) - measures how bad a single prediction #mi(`\hat y`) is compared to the true label $y$.

We define two common choices
- #mi(`\ell(y,\hat{y}) := \mathbb{I}(y \neq \hat{y})`), zero-one loss for classification
- #mi(`\ell(y,\hat{y}) := (y-\hat{y})^2`), squared error for regression


= Empirical Risk Minimization (ERM)

Take the zero-one loss $ell(y,y´) = II(y != y´)$ as an example

Then we want to minimize $L_(D,f) (h):= PP_(x tilde D)[f(x)!=h(x)]$

$h_* = arg min_h L_(D,f) (h)$


Since $f(x)$ is unknown we can find the average using actual true results

#mi(`h_S = arg min_h L_S (h) = arg min_h (abs(\{i in [m], h(x)-f(x)\}))/(m)`)

This is not well defined and has weaknesses regarding the hypothesis. We resolve to curve fitting.


= Polynomial curve fitting

With dataset $S=\{(x_1,y_1),(x_2,y_2),dots,(x_n,y_n)\}$ it is wise to split data into the following:
- Training set, where the learning algorithm will find a $h$ that minimizes $L(h)$
- Test set, check how well the algorithm is doing (are we closer to approximating $f(x)$?) This is ofc assuming i.i.d.

We can use polynomial fitting instead


$y(x) = sum_(m=0)^M w_m x^m$
 Then by *ridge regression* $w=(X^T X+lambda I)^(-1)X^T Y$
We can predict $y$ by $x^T w$ 
// [image omitted: Pasted image 20260119123637.png]
(example of this)

For each order ($m$) we can calculate the risk with the *Root Mean Squared Error (RMSE)*

#mimath(`L_S := \sqrt{\frac{1}{m} \sum_{i \in [m]} (h(x_i) - y_i)^2}`)
// [image omitted: Pasted image 20260119123935.png]
(example)

This should by theory then go down (risk should decrease as the algorithm learns)

Notice that at $M=7$ the model overfits and acts poorly

This is because of very small or large weights
- at a small $m$, model will likely underfit
- at a high $m$, model will likely overfit

To avoid this we use a *regularizer* #mi(`h_S := \arg \min_h L_S(h) + \lambda w_m^2`)
// [image omitted: Pasted image 20260119193706.png]
This mitigates over/under-fitting as polynomial degree increases

