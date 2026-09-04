#import "@local/tempst:0.1.0": *
#import "@preview/mitex:0.2.7": mi, mimath

#show: note.with(
  title: "Introduction to Machine Learning",
  course: "AI512 — Introduction to Machine Learning",
  author: "Simon Holm",
  date: "2026-09-04",
  outline-depth: 2,
)

= Basic Concepts

== Formal definitions

=== Formal definitions
“A computer program is said to learn from experience E with respect to some class of tasks T and performance measure P, if its performance at tasks in T, as measured in P, improves with experience E”. [Mitchell, 1997]

So
==== Experience E
Experience $E$ comes from the dataset itself $S=\{(x_1,y_1),(x_2,y_2),dots,(x_n,y_n)\}$.

The goal is to find a hypothesis that accurately represents #mi(`S:=\{(x_i, y_i) \overset{i.i.d}{\sim} D\}`)
==== Task T
The task is to predict $f(x_1) = y_i$ with $h(x_i) approx y_i$

$f(x)approx h(x),forall x in X$

Task can either be a *Classification* or a *Regression* dependent on whether Y is a continuous set (like $RR$)
==== Performance measure P
Performance is measure via a loss(risk) function $L:Y times Y -> RR^+$ 
Where 
$ell(y,y´)$

where $y$ is the true value of $y$ and #mi(`\hat{y}`) is the prediction of $y$

Loss (risk) - measures how bad a single prediction #mi(`\hat y`) is compared to the true label $y$.

We define two common choices
- #mi(`\ell(y,\hat{y}) := \mathbb{I}(y \neq \hat{y})`), zero-one loss for classification
- #mi(`\ell(y,\hat{y}) := (y-\hat{y})^2`), squared error for regression


== Empirical Risk Minimization (ERM)

Take the zero-one loss $ell(y,y´) = II(y != y´)$ as an example

Then we want to minimize $L_(D,f) (h):= PP_(x tilde D)[f(x)!=h(x)]$

$h_* = arg min_h L_(D,f) (h)$


Since $f(x)$ is unknown we can find the average using actual true results

#mi(`h_S = arg min_h L_S (h) = arg min_h (abs(\{i in [m], h(x)-f(x)\}))/(m)`)

This is not well defined and has weaknesses regarding the hypothesis. We resolve to curve fitting.


== Polynomial curve fitting

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


= Linear Predictors

== Least Squares Regression

Assume a dataset
// [image omitted: Pasted image 20260119193909.png]
Data obviously has a linear correlation. So we can denote the hypothesis space more specifically

#mimath(`\mathcal{H} := \{h: h(x) = w^T x, w \in \mathbb{R}^k\}`)

Where $w=(w_0,w_1)$

Assume dataset #mi(`S=\{(x_i, y_i) : i \in [m]\}`), then squared-error loss for $h$ is:

#mimath(`L_S(w) := \frac{1}{m} \sum_{i \in [m]} (w^\top x_i - y_i)^2`)

Then #mi(`w_S := \arg \min_w L_S(w)`) to find a good vector for the hypothesis $w$

#mimath(`\nabla_w L_S(w) = \frac{2}{m} \sum_{i=1}^{m} x_i (x_i^\top w - y_i) \\
= 0`)


#mimath(`\Rightarrow \sum_{i=1}^{m} x_i x_i^\top w_S = \sum_{i=1}^{m} x_i y_i`)


#mimath(`\Rightarrow w_S = \Bigg( \sum_{i=1}^{m} x_i x_i^\top \Bigg)^{-1} \sum_{i=1}^{m} x_i y_i`)

This is often refered to as least squares regression, and $w$ as the least squares solution.

We can denote $z_i := (x_i, 1)$, where $Z = vec(z_i,dots.v,z_n)$to show that:  

#mimath(`w_S := (Z^\top Z)^{-1} Z^T y.`)

// [image omitted: Pasted image 20260119200002.png]
(this is an example of the learned hypothesis $h$ with $w$)


== Metric Spaces

We would like the vector spaces (such as feature spaces, parameters spaces etc.) to have some plausible properties such as.
- *strict:* #mi(`\forall a, a' \in X, a \neq a' \Rightarrow dist(a,a') > 0`),
- *reflexive:* #mi(`\forall a \in X, \Rightarrow dist(a,a) = 0`),
- *symmetric:* #mi(`\forall a, a' \in X, \Rightarrow dist(a,a') = dist(a',a)`).
- *triangle inequality*: #mi(`\forall a, b, c \in X, dist(a,c) \leq dist(a,b) + dist(b,c)`).
Where $"dist"(a,b)$ is a function that measures distance between two points. 

These requirements are nice to have since they make the distance function $"dist"(a,b)$ behave like a *real notion of distance*.

Using this we can define the *norm* of a vector aka. The length
For $p>0$ the $L_p$ norm is defined as follows:

#mimath(`|| u ||_p = \Big ( \sum_{j=1}^k |u_j|^p \Big )^{1/p}`)


If $p=2$, we get the well-known *Euclidean norm*.

If $p=1$, we get the *Manhattan norm*.

As #mi(`p \rightarrow \infty`), we get the *Maximum norm* (#mi(`L_\infty`)), i.e. #mi(`||u||_\infty := \max \{|u_1|, \ldots, |u_d|\}`).

We can use this norm to derive many nice metric spaces

$"dist"(a,b) = ||a-b||_p$.

// [image omitted: Pasted image 20260119202510.png]
(this example shows the behavior of the distances w.r.t $p$)


== Regularized Least Squares

As we saw in 3. Polynomial curve fitting, $L_S (h_S) = 0$ by memorizing training data can be achieved but will result in overfitting

Assume a hypothesis space $H$ of finite size $|H|$ and a loss function $L_s (h)$ which fits the data as well as possible and constraining model complexity.

#mimath(`h_S := \arg \min_{h \in H} L_S(h)+\lambda |H|`)


With $lambda$ as a *regularization coefficient* and $|H|$ the *regularizer* this is called *Structured Risk Minimization*, one of the biggest achievements of machine learning research in the pre-deep-learning era.

Let us apply this to the least squares problem.

#mimath(`h_S := \arg \min_w \frac{1}{m} \sum_{i \in [m]} (w^\top x_i - y_i)^2`)

As we know, when $m$ become very large, the weighs will also become large, and thus result in overfitting. To counter this we force the model to keep weights small with the constraint 

#mimath(`h_S := \arg \min_w \frac{1}{m} \sum_{i \in [m]} (w^\top x_i - y_i)^2, \qquad ||w||_p \leq \eta`)


This constrained optimization problem can be expressed equivalently as:


#mimath(`h_S := \arg \min_w \max_\lambda \frac{1}{m} \sum_{i \in [m]} (w^\top x_i - y_i)^2 + \lambda (||w||_p^p - \eta)`)

where $lambda >= 0$.

By choosing a large #mi(`\lambda`) and dropping the inner #mi(`\max`) problem, we can find a reasonable approximation referred to as *regularized least squares*:


#mimath(`h_S := \arg \min_w  \frac{1}{m} \sum_{i \in [m]} (w^\top x_i - y_i)^2 + \lambda ||w||_p^p`)

We can solve for $w$ with $nabla L_S (h_S) =0$ to find a good $w$ for the hypothesis


== Ridge regression

Take a special case of regularized least squares, where $p=2$ (example of solving for an opt. $w$)

#mimath(`h_S := \arg \min_w \frac{1}{m} \sum_{i \in [m]} (w^\top x_i - y_i)^2 + \lambda ||w||_2^2`)

We can rewrite the loss of this optimization in vector form as.
#mimath(`L_S(w) := \frac{1}{m} (Z w-y)^2 + \lambda w^\top w`)

Let us find the optimal weights that minimize the loss by setting its gradient to zero once again: (skipped because I can't be bothered smh)


#mimath(`L_S(w) = \frac{1}{m} w^\top Z^\top Zw - \frac{1}{m} 2 w^\top Z^\top y + \lambda w^\top w`)


#mimath(`\Rightarrow w_S = \left(\frac{1}{m} Z^\top Z + \lambda I \right )^{-1} Z^\top y.`)

This again is known as *ridge regression*.

*Note:* finding a lambda which serves an optimal penalty, is normally found with *k-fold Cross Validation*


== Z-score normalization

A model outputs the following:

Means: [-0.00031786 0.00045392 -0.00103062 -0.00226024 -0.00069973 -0.00092399 -0.00058225 0.00039851 0.00066739 -0.00087822] 
Variances: [0.00222035 0.00226498 0.00217988 0.00219522 0.00233499 0.00234279 0.00229155 0.00229439 0.0023026 0.00222032]

Clearly these means vary a lot in scales (-0.0023 is almost 10x larger than -0.0003)

To avoid unintentionally prioritizing the features with larger numbers we can normalize (using *Z-score normalization*)

Assume that some data is normal distributed with some mean #mi(`\mu`) and variance #mi(`\sigma^2`). That is, each dimension $x_d$ of a $D-$dimensional observation $x$ comes from a sampling process as below:
#mimath(`\epsilon \sim N(0,1),\quad x_d = \mu_d + \sigma_d \epsilon`)

We can then 
#mimath(`x' = \frac{x_d -\mu_d}{\sigma_d} = \epsilon`)

And thus we can center and scale based on the sample mean $mu$ and standard deviation $sigma$.


== Lasso regression

Much like ridge regression where

#mimath(`h_S := \arg \min_w \frac{1}{m} \sum_{i \in [m]} (w^\top x_i - y_i)^2 + \lambda ||w||_2^2`)

Where the penalty term is $lambda norm(w)_2^2$ 

In Lasso regression, the penalty term is simply $lambda abs(w)$

This way Lasso “selects” features by letting only the most predictive ones survive while others are pushed exactly to zero. This is unlike ridge regression, where terms can only approach 0.

So with Lasso regression
- If a feature is *strongly correlated with the output*, it tends to survive (keep a nonzero coefficient).
- If a feature is *weakly correlated* or *highly redundant compared with other features*, Lasso often sets it to zero.

This however results in a problem since, 
#mimath(`f'(x) =\begin{cases}  1, & \text{if } x > 0 \\[2mm] -1, & \text{if } x < 0 \\[1mm] \text{undefined}, & \text{if } x = 0 \end{cases}`)

Because of this we cannot solve for $w$ using $nabla L_S (h_S) = 0$

However, we can use gradient descent in the direction $-nabla L_S (h_S)$ (the direction opposite of the greatest uphill) to approach an optimal $w$ where $nabla L_S (h_S)$ is as small as possible.

#mimath(`w_{t+1} := w_t - \alpha \nabla_w L_S(w) \vert_{w:=w_t}`)


This approach is called *gradient descent*. It is in use with nearly all modern machine learning approaches. The coefficient #mi(`\alpha>0`) is called a *learning rate*.

Gradient descent requires repetitive evaluation of the gradient of the loss with respect to the parameters at every iteration: #mi(`L_S(w) \vert_{w:=w_t}`). Hence, its implementation on the computer requires an analytical calculation of this gradient. This may be time consuming for complex loss functions. Deep learning libraries such as PyTorch and TensorFlow allow us to automate this process. See an example PyTorch implementation of Lasso regression below.

=== From 02_Linear_Predictors.ipynb
```python
import torch as th
import torch.nn as nn
import torch.nn.functional as F
import torch.optim

class LassoRegression(nn.Module):
	def __init__(self, n_dims, lambda_coef=1):
	super(LassoRegression, self).__init__()
	self.lambda_coef = lambda_coef
	self.emp_risk = nn.MSELoss()
	self.weight = nn.Parameter(th.randn((n_dims,1)))
	self.bias = nn.Parameter(th.randn((1)))
	
	def predict(self, input):
		return input @ self.weight + self.bias
	
	def learn(self, inputs, labels, num_steps=1):
		# The in-built Stochastic Gradient Descent optimizer
		# The argument "lr" sets the learning rate
		optimizer = torch.optim.SGD(self.parameters(), lr=0.01)
		
		for ii in range(num_steps):
			# Predict with the current weight values
			# This step is called a "forward pass"
			
			predictions = self.predict(inputs)
			loss = ((predictions - labels)**2).mean() \
					+ self.weight.abs().sum()*self.lambda_coef
			
			# Clear the gradient values remaining from
			# the previous iteration
			optimizer.zero_grad()
			
			# Compute the new gradient values
			# This step is called the "backward pass"
			loss.backward()
			
			# Take the gradient descent step
			optimizer.step()

# Convert data into the Torch format
X_train = torch.tensor(X_train).float()
X_test = torch.tensor(X_test).float()
y_train = torch.tensor(y_train).float().reshape(-1,1)
y_test = torch.tensor(y_test).float().reshape(-1,1)

# z-score normalization
m = th.mean(X_train,axis=0)
std = th.std(X_train,axis=0)
X_train = (X_train-m)/std
X_test = (X_test-m)/std
# Train our model
model_lasso = LassoRegression(n_dims=X_train.shape[1], lambda_coef=1)

# Number of gradient descent iterations
num_iterations = 250

# Collect the train and test errors here.
train_errors = np.zeros(num_iterations)
test_errors = np.zeros(num_iterations)

for ii in range(num_iterations):
	model_lasso.learn(X_train, y_train)
	predictions = model_lasso.predict(X_train)
	train_error = ((predictions - y_train)**2).mean().sqrt()
	train_errors[ii] = train_error.detach().numpy()
	
	# Test our model
	predictions = model_lasso.predict(X_test)
	test_error = ((predictions - y_test)**2).mean().sqrt()
	test_errors[ii] = test_error.detach().numpy()

# Plot the learning curve
plt.plot(np.arange(num_iterations),train_errors,'b-', label="Train RMSE")
plt.plot(np.arange(num_iterations),test_errors,'r-', label="Test RMSE")
plt.xlabel("Iteration")
plt.ylabel("RMSE")
plt.legend(loc="upper right")
plt.show()

```
// [image omitted: Pasted image 20260119234924.png]
(plotted graph from lecture)


= Classification

== Binary Classification

Assume that we have a binary classification problem where

#mi(`S=\{(x_1,y_1),(x_2,y_2),dots,(x_n,y_n)\}, quad "and" quad y in\{0,1\}`)


Like before we assume the hypothesis to be linear:

#mimath(`\mathcal{H} := \{h: h(x) = w^\top x, w \in \mathbb{R}^k \}`)

But in this case the output is now discrete and not continuous.

Let's define one possible way to interpret the label $y$.
- Sign of $h_i$
	- If $h_i>0 -> "class 1"$
	- If $h_i<0 -> "class 0"$
- magnitude of $|h_i|$
	- Larger $|h_i| ->$ higher *confidence*
	- Smaller $|h_i| ->$ lower *confidence*

*Note:* this is just *one* possible way of interpreting a discrete output.

When interpreted this way, the hypothesis $h_i$ is called the *discriminant function*. Such a model is not easy to train since we normally desire a differentiable loss function, which we can't with this "sign" function. By considering this a probability problem would like to achieve:

$p_i = w^T x_i quad "WRONG!"$

The problem with this is that the range of $w^T x_i in (-oo,oo)$ and $p_i in [0,1]$

By taking the log we can widen this so.

$log(p_i) prop w^T x_i$

This works well for any $w^T x_i<0$, but is undefined for $w^T x_i >0$ since $log(a) in (-oo,0)$ 
To fix this, we account for the probability of the other class. We actually want to express

$w^T x_i=log ((PP(y_i = 1|x_i))/(PP(y_i = 0|x_i))) = log(p_i/(1-p_i))$

This is called logistic function, and we can solve for $p_i$ to find probabilities for both classes.


$log(p_i/(1-p_i)) = w^T x_i quad => p_i = 1/(1+e^(-w^T x_i))$



== Logistic Regression

We have leaned how binary classification can be done using probability. From that we get that:

$log(p_i/(1-p_i)) = w^T x_i quad => p_i = 1/(1+e^(-w^T x_i))$

The function $sigma(u) = 1/(1+e^(-u))$ is known as the sigmoid function, and the inverse of the sigmoid function is

$sigma^(-1)(p) = p/(1-p) quad "for some" p, "given that" u = w^T x_i$

This is called logistic regression.


Now to determine the probability of the whole dataset. We use the following notation

$PP( y_i | x_i , w) = p_i^(y_i)(1-p_i)^(1-y_i)$

This way:
- $PP( y_i=0 | x_i , w) = 1-p_i$
- $PP( y_i=1 | x_i , w) = p_i$
// [image omitted: Pasted image 20260120232310.png]
(This is an example of how logistic regression uses probability to determine whether mice are obese or not)

And so to determine the whole dataset we can


#mimath(`\prod_{i=1}^m p_i^{y_i} (1-p_i)^{1-y_i}`)

Note that products are messy to differentiate, which is nice, for optimization, therefore we can express this as a sum instead:

#mimath(`\ell(w) = \log\left(\prod_{i=1}^m p_i^{y_i} (1-p_i)^{1-y_i}\right) = \sum_{i=1}^m\left[y_i \cdot log(p_i) + (1-y_i)\cdot log(1-p_i)\right]`)

Now to express this as a loss function we take the negative log-likelihood, and substitute $p_i=sigma(w^T x_i)$ (the sigmoid)


#mimath(`\mathcal{L}_{01}(w) = -\sum_{i=1}^m \log \sigma(w^\top x_i)^{y_i} (1-\sigma(w^\top x_i))^{1-y_i}`)

Now we can differentiate the loss function w.r.t $w$ to find an optimal $w$. That is stupid, so I won't do it here.


== Multi-Class Classification

Assume that we now have more than 2 classes (so not binary anymore). Assume that we have $C$ classes, so that

#mi(`S=\{(x_1,y_1),(x_2,y_2),dots,(x_n,y_n)\}, quad "and" quad y in\{0,1,C-1\}`)

We will need to model the class probabilities of $C$ different classes: #mi(`p_1, p_2, \ldots, p_C`):

In binary classification, we discriminated classes with a *sign function*. In multi-class classification we ought to assign a *score for each class*:

$s_c = w_c^T x_i, quad c=1,2,dots,C$

Where $s_c in (-oo,oo)$

We want to map these scores to probability. When mapping to probability, we want to make sure that the following constraints hold:
- $0<=p_i<=1$, each probability is constraint as a number between 1 and 1
- $sum_(c=1)^C p_c =1$, the sum of all $p_c$ is always 1.

*Softmax* deals with exactly this, by generalizing the binary classification to $C>2$

#mimath(`\sigma(x)_c = \dfrac{e^{x_i}}{\sum_{j=1}^k e^{x_j}}`)

The related loss function is then:
#mimath(`\mathcal{L}_{CE}(W) = -\sum_{i=1}^m \log \sigma(w_{y_i}^\top x_i) = \sum_{i=1}^m \Big \{ -w_{y_i}^\top x_i + \log \Big ( \sum_{c=1}^C e^{w_c^T x_i} \Big ) \Big \}`)

where #mi(`W = [w_1 \ldots w_C]`) is a matrix of the weight vectors for each class. This loss function is called the *cross entropy loss*. We will revisit this loss and understand better why it is given this particular name.

#line(length: 100%, stroke: 0.5pt + luma(200))
Let's try and make an as general algorithm as possible using what we have leaned form regularization

#mimath(`L(W) := L_{CE}(W) + \lambda \sum_{c=1}^C ||w_c||_p`)

Like before we can use gradient descent to minimize loss
// [image omitted: Pasted image 20260120122015.png]
(example from lecture)


== Performance Metrics for Classification

The goal of classification is to recognize a pattern. We introduce confusion matrices.
// [image omitted: Pasted image 20260120122207.png]
For confusion matricies: 
- TP true and correct class 
- FP: false and correct class 
- TN true and incorrect class 
- FN: false and incorrect class 

We can compute many performance metrics from the confusion matrix:

- Accuracy $:= "Sum of Diagonal"/"Sum of all samples"$
- Precision (per class) $:= "TP"/"TP+FP"$
- Recall (per class) $:= "TP"/"TP+FN"$
- F1 Score (per class) $:= "F1"=2 dot ("Precision" dot "Recall")/("Precision" + "Recall")$
- False Positive Rate (per class) $:= "FP"/"FP+TN"$
- Specificity $:= "TN"/"TN+FP"$

// [image omitted: AC774AE3-844E-46AB-A256-E5FEE1E54126_1_105_c.jpeg]

// [image omitted: 84C826E6-C028-43C7-8C81-25E73AD21B01_1_105_c.jpeg]

// [image omitted: 0095E31D-CAD2-4C43-A4B7-3A78904CF39C_1_105_c.jpeg]

// [image omitted: 98EBF5AF-E16D-4C2F-A2A5-0D81A9D264D1_1_105_c.jpeg]

// [image omitted: 062F5071-77DF-43E9-9334-C0AAC967D3AE_1_102_o.jpeg]

// [image omitted: 2361E8B7-F465-4832-8FFA-CD3E81C78445_1_102_o.jpeg]

// [image omitted: 40A3AD9C-CD22-4AF9-B987-C32326FED5C4_1_102_o.jpeg]


== K-Fold Cross Validation

// [image omitted: Pasted image 20260120130706.png]
Split up data in k different ways, to optimize


== K-Nearest Neighbors (kNN) Classifier

kNN is based around memorizing the entire dataset and using distance to neighboring data points to classify new data.

This way a new point is classified based on the classes of its *k nearest neighbors* in the training set.

There are multiple ways to do kNN, here I will show 2 alike, yet different ways.
==== Standard kNN

#mimath(`\hat{y} = \arg \max_{c \in [C]} \sum_{i=1}^k \mathbb{1}(y_i = c)`)

The standard kNN selects a class to a new data point, only based on the majority vote of the k-nearest neighbors.

==== Weighted kNN

#mimath(`\hat{y} = \arg \max_{c \in [C]} \sum_{i=1}^k \mathbb{1}(y_i = c) \dfrac{1}{d(x,x_i)}`)

The weighted kNN does the same, however it normalizes each vote, using the ratio of the distance. This way, points with a small distance to the new point $x$ contribute more to the vote, than points with greater distance to the new point.  

==== Voronoi cells
A *voronoi cell* is the area around a datapoint to which it is the closest point
// [image omitted: Pasted image 20260120134432.png]
Example, any new data point within the blue area, will be classified to the blue

When you do this for all data points, you can create a Voronoi map
// [image omitted: Pasted image 20260120135459.png]
// [image omitted: Pasted image 20260120135503.png]


== Receiver Operating Characeteristics (ROC)

*ROC curve* is a tool to *evaluate the performance of a binary classifier*.
// [image omitted: Pasted image 20260120232350.png]
Take this example, to find out where the threshold should be (for the model to be optimal), we can use ROC 



To understand ROC, we need *two rates*:
1. True Positive Rate (TPR)/ sensitivity / Recall

$"TPR" = "TP"/"TP+FP"$

2. False Positive Rate (FPR)

$"FPR"="FP"/"TN + FN"$

W

The ROC plots these as follows for each:
// [image omitted: Pasted image 20260120232105.png]
The more to better thresholds are the ones most at the top left. Now we can choose one, depending on what the model is trying to achieve.

Also, we can use the *AUC* (area under curve) as measurement for how good the classifier is. The higher the AUC, the better the classifier. A perfect classifier has an AUC of 1. A classifier that performs no better than random guessing has an AUC of 0.5.


= Probability Theory

== Basic Concepts

*Intuitive meaning of probability:* how often an event happens if you repeat a random experiment many times.

#mimath(`P(A) = \lim_{n \rightarrow \infty} \frac{n_A}{n}`)

==== Sample space
The *sample space* is all the possible outcomes.
- Example: Rolling one die → #mi(`\Omega = {1, 2, 3, 4, 5, 6}`)

==== Event $bold(A)$ 
An *event* is just a subset of the sample space.
- Example: #mi(`A = \text{"rolling an even number"} = {2, 4, 6}`)

Events are what you actually care about measuring probabilities for.

We say powerset of $Omega$ as $2^(Omega)$, then $sigma(Omega) subset 2^(Omega)$ is an eventspace (collection of subsets of $Omega$). An eventspace must be closed under countable unions, countable intersections, and complements. That means:
1. #mi(`\bigcup_{i=1}^{\infty} A_i \in \sigma(\Omega)`) 
2. #mi(`\bigcap_{i=1}^{\infty} A_i \in \sigma(\Omega)`)
3. #mi(`A^c \in \sigma(\Omega)`)

For many discrete sample spaces such as the outcome of the roll of a pair of dice, it is possible to assume that #mi(`2^\Omega=\sigma(\Omega)`). However, for continuous sample spaces we need to define a #mi(`\sigma`)-algebra that is smaller than #mi(`2^\Omega`). This is because an uncountable set with size $2^(Omega)$ is very unstable and breaks the rules of probability.

Example in a dart game, if we care about the precise hit of a dart arrow, this can become very complex. In dart we resort to only care about the actual points the dart arrow hits. So we make the space smaller.

A *probability measure* is a function #mi(`P: \sigma(\Omega) \rightarrow [0,1]`) such that:
1. #mi(`P(\Omega) = 1`)
2. #mi(`P(A) \geq 0`) for all #mi(`A \in \sigma(\Omega)`)
3. If #mi(`A_1, A_2, \dots`) are disjoint events (i.e. #mi(`A_i \cap A_j = \emptyset`) for all #mi(`i \neq j`)\), then #mi(`P(\bigcup_{i=1}^{\infty} A_i) = \sum_{i=1}^{\infty} P(A_i)`)

We actually want to restrict the events to a *well-behaved σ-algebra*

==== Probability Space: 
A tuple defined as #mi(`(\Omega, \sigma(\Omega), P)`).

==== Inclusion-Exclusion Principle:

#mimath(`P(A \cup B) = P(A) + P(B) - P(A \cap B)`)

This takes into account the fact that #mi(`P(A \cap B)`) is counted twice in $P(A) + P(B)$. A direct consequence of this is that #mi(`P(A \cup B) \leq P(A) + P(B)`) which is called the *union bound*.
==== Conditional Probability:

#mimath(`P(A|B) = \frac{P(A \cap B)}{P(B)}`)

The intuitive meaning of this is the probability of event $A$ given that event $B$ has occurred. That is, the frequency of event $A$ in the subset of trials where event $B$ has occurred. In mathematical terms

#mimath(`P(A|B) = \lim_{n \rightarrow \infty} \frac{n_{A \cap B}}{n_B}`)

The definition of conditional probability can be rewritten as #mi(`P(A \cap B) = P(A|B)P(B)`). This is called the *product rule*.

==== *Independence:* 
If the probability of event $A$ is not affected by the occurrence of event $B$, these two events are said to be independent.

In terms of conditional probabilities we can describe this situation as $P(A|B) = P(A)$. Applied to the definition of conditional probability, this means that $A$ and $B$ are independent if and only if 
$P(A sect B) = P(A)P(B) quad "independent"$

$P(A sect B) != P(A)P(B) quad "dependent"$

==== *Law of Total Probability:* 
If #mi(`B_1, B_2, \dots, B_n`) is a partition of #mi(`\Omega`), i.e. #mi(`B_i \cap B_j = \emptyset`) for all #mi(`i \neq j`) and #mi(`\bigcup_{i=1}^n B_i = \Omega`), then 
#mimath(`P(A) = \sum_{i=1}^n P(A|B_i)P(B_i)`)

==== *Bayes' Rule:* 

#mimath(`P(A|B) = \frac{P(B|A)P(A)}{P(B)}`)
This can be derived from the definition of conditional probability as follows:

#mimath(`P(A \cap B) = P(A|B)P(B) = P(B|A)P(A) \Rightarrow P(A|B) = \frac{P(B|A)P(A)}{P(B)}`)

Assume law of total probability #mi(`\mathcal{H}`), i.e. #mi(`H_i \cap H_j = \emptyset`) for all #mi(`i \neq j`) and #mi(`\bigcup_{i=1}^n H_i = \mathcal{H}`). Also assume that $D$ is the observed data. Then, Bayes' rule can be written as:

#mimath(`P(H_i|D) = \frac{P(D|H_i)P(H_i)}{P(D)} = \frac{P(D|H_i)P(H_i)}{\sum_{j=1}^n P(D|H_j)P(H_j)}`)

Here,
- $P(H_i)$ is our *prior belief* in a hypothesis $H_i$,
- $P(H_i|D)$ is our *posterior belief* in $H_i$ after observing the data $D$,
- $P(D|H_i)$ is the *likelihood* of $H_i$, and $P(D)$ is the *evidence*.
  
The concepts above give a general framework for *statistical inference*: we start with a prior belief, collect new observations, and update our belief based on them. This approach has connections to how humans make decisions under uncertainty.


== Random Variables

It is not always convenient to describe sets. We can facilitate this by defining a random variable.

A *random variable* is function #mi(`X: \Omega \rightarrow \Lambda`) that maps each elementary event #mi(`\omega \in \Omega`) to an element on its *range* #mi(`\lambda \in \Lambda`). We can define a probability measure on #mi(`\Lambda`) as follows: 
#mimath(`P_X(A) := P(\{\omega \in \Omega: X(\omega) \in A\})`)

where #mi(`A \subset \Lambda`). This is called the *induced probability measure* of $X$. 

*Intuition:* *measures the outcome* as a number.

==== Example of random variable
Consider the experiment of tossing a fair coin three times and denote heads by $H$ and tails by $T$. The sample space for this experiment is #mi(`\Omega = \{HHH, HHT, HTH, HTT, THH, THT, TTH, TTT\}`). 

Because the coin is fair

#mimath(`P(H) = P(T) = \frac{1}{2}`)

Because the coin tosses are independent events, we have

#mimath(`P(HHH) = P(H)P(H)P(H) = \frac{1}{2} \cdot \frac{1}{2} \cdot \frac{1}{2} = \frac{1}{8}`)

and likewise for all other outcomes. Let $X$ be the number of heads in a sequence. Then $X$ is a random variable with range #mi(`\Lambda = \{0,1,2,3\}`). The induced probability measure of $X$ is:
1. #mi(`P_X(0) = P(\{TTT\}) = \frac{1}{8}`)
2. #mi(`P_X(1) = P(\{TTH, THT, HTT\}) = \frac{3}{8}`)
3. #mi(`P_X(2) = P(\{HTH, HHT, THH\}) = \frac{3}{8}`)
4. #mi(`P_X(3) = P(\{HHH\}) = \frac{1}{8}`)

Let us give another example. Let us make up two random variables from the outcome of the roll of a fair die:
1. $X$ is the outcome of the die itself i.e. #mi(`\Lambda = \{1,2,3,4,5,6\}`).
2. $Y$ is whether the outcome of the die is even or odd, i.e. #mi(`\Lambda = \{even, odd\}`).

Then consider the case of the joint event #mi(`P(X \leq 4 \cap Y = even) = P(\{2,4\}) = \frac{2}{6}`).

Marginalization
- For the following $P(X=x,Y=y)$ where $Y$ is binary (like even odd) 	
- Because of this we can drop one of them 

#mi(`P(X=x)=sum_(y=\{E,O\})P(X=x|Y=y)P(Y=y)`)

- and so
$P(X=x) = 1/2 P(X=x|Y=E)+1/2P(X=x|Y=O)$



== Expectance and Variance

The *expectation* (a.k.a. *expected value*) of a random variable $X$ is defined as:

#mimath(`E[X] := \sum_{x \in \Lambda} x P_X(x)`)

Take the three coin tosses example from 2. Random Variables where the random variable $X$ indicates the number of heads, $EE[X] = 1.5$ because $EE[X] = 0 dot 1/8 + 1 dot 3/8 + 2 dot 3/8 + 3 dot 1/8 = 1.5$.

The expectation of a random variable is also called its *mean*. The expectation of a random variable is a measure of its *central tendency*. However, it does not tell us anything about the *spread* of the random variable. 

This can be measured by the *variance* of a random variable which is defined as:

$"Var"[X] := EE[(X-EE[X])^2] = EE[X^2] - EE[X]^2$

In the above example, $Var[X] = 0.75$ because 
$"Var"[X] = EE[X^2] - EE[X]^2 = 0^2 dot 1/8 + 1^2 dot 3/8 + 2^2 dot 3/8 + 3^2 dot 1/8 - 1.5^2 = 0.75$

The square-root of the variance is called the *standard deviation* of a random variable. Standard deviation is commonly denoted by #mi(`\sigma`). In the above example, #mi(`\sigma(x) = \sqrt{0.75} = 0.866`).

==== Moments
Generally speaking, a *moment* of a random variable is defined as:

$EE[(X-EE[X])^k]$

where $k$ is a positive integer. The first moment is the expectation itself. The second moment is the variance. The third moment is called the *skewness* of a random variable. It is a measure of the asymmetry of the distribution of a random variable. The fourth moment is called the *kurtosis* of a random variable. It is a measure of the heaviness of the tails of the distribution of a random variable.

#line(length: 100%, stroke: 0.5pt + luma(200))
=== Common rules for Expectance and Variance

#mimath(`\mathbb{E}[aX + bY] = a\mathbb{E}[X] + b\mathbb{E}[Y]`)


#mimath(`\mathbb{E}[c] = c`)

- Discrete

#mimath(`\mathbb{E}[g(X)] = \sum_x g(x) , P(X = x)`)

- Continous

#mimath(`\mathbb{E}[g(X)] = \int_{-\infty}^{\infty} g(x) f_X(x) , dx`)


#mimath(`\mathbb{E}[aX] = a\mathbb{E}[X]`)


#mimath(`\mathbb{E}\left[\sum_{i=1}^n X_i\right] = \sum_{i=1}^n \mathbb{E}[X_i]`)


#mimath(`\text{If } X \perp Y: \quad \mathbb{E}[XY] = \mathbb{E}[X] \cdot \mathbb{E}[Y]`)


#mimath(`\mathbb{E}[X] = \mathbb{E}[\mathbb{E}[X|Y]]`)


#mimath(`\mathrm{Var}[X] = \mathbb{E}[X^2] - (\mathbb{E}[X])^2`)

and so $EE[X^2] = EE[X]^2 + "Var"[X]$

#mimath(`\mathrm{Var}(aX) = a^2 \mathrm{Var}(X)`)


$"Cov"[X,Y] := E[(X-E[X])(Y-E[Y])] = E[X Y] - E[X]E[Y]$


#mimath(`\mathrm{Var}(AB) \neq \mathrm{Var}(A) \cdot \mathrm{Var}(B)`)

above only applies if they are *dependant*

#mimath(`\mathrm{Var}(AB) = \mathbb{E}[A]^2 \mathrm{Var}(B) + \mathbb{E}[B]^2 \mathrm{Var}(A) + \mathrm{Var}(A)\mathrm{Var}(B)`)


#mimath(`Var[\sum_{i=1}^n X_i] = \sum_{i=1}^n Var[X_i]`)



== Continuous Random Variables

Now consider the case where #mi(`\Lambda = \mathbb{R}`). This means a random variable $X$ maps outcomes $ω∈Ω$ to numbers in $RR$.


In this case, we have a *continuous random variable* with the induced probability measure:

#mimath(`P_X(A) = P(\{\omega \in \Omega: X(\omega) \in A\})`)


where #mi(`A \subset \Lambda =\mathbb{R}`). 

This probability measure has the pre-image of $A$ under $X$ on its argument, i.e. 
$P_X(A) = P(X^(-1)(A))$
Where $X^(-1)(A)$ is the pre-image, aka all the inputs that, when put through the function $X$, give a numbers in $A$ (the ones we care about).

A common situation for using #mi(`X: \mathbb{R} \rightarrow \mathbb{R}`) is where events are intervals $(a,b)$ on the range of $X$. In this case, we can define a probability measure on #mi(`\mathbb{R}`) as follows:

#mimath(`P_X(a < x < b) := P(\{\omega \in \Omega: X(\omega) \in (a,b)\})`)


where #mi(`a,b \in \mathbb{R}`) and $a < b$. 

=== Example
- Bus arrives uniformly at any time in $[0,60]$ minutes
- $X(w)=w$ (random variable is just the arrival time)
- $A=[0,15]$ minutes (we want to know the prop. that the bus arrives in between 15 and 45 minutes.)
- Pre-image is then $X^(-1) (A) =[15,45]$ in $Omega$ 
- Probability is therefore: 

$P_X (A) = P(X^(-1) (A)) =P(w in [15,45]) = (45-15)/60 = 1/2$

The #mi(`\sigma`)-algebra of #mi(`\mathbb{R}`) by the intervals of the form $(a,b)$ is called the *Borel #mi(`\sigma`)-algebra*. 
=== Cumulative distribution function (CDF)
We can also determine probability given that $X$ is $<=$ some number $x$:

#mimath(`F_X(x) := P(X \leq x) \quad \forall x \in \mathbb{R}`)

Note that this is a non-decreasing function (probabilities goes up as $x$ increases).

=== Probability density function (PDF)

Often, we are interested in interpreting the behavior of a continuous distribution by investigating how densely probability is packed at each point $x$. This is the derivative of $F_X (x)$.

#mimath(`f_X(x) := \frac{d}{dx} F_X(x) \quad \forall x \in \mathbb{R}`)

Then, by the fundamental theorem of calculus, we have that :

#mimath(`F_X(a) = \int_{-\infty}^a f_X(x) dx`)

Hence an event can be described in terms of the probability density function as follows:

#mimath(`P(a < X < b) = F_X(b) - F_X(a) = \int_{-\infty}^b f_X(x) dx - \int_{-\infty}^a f_X(x) dx = \int_{a}^b f_X(x) dx`)


The probability density function of a continuous random variable is non-negative and integrates to 1:

#mimath(`\int_{-\infty}^{\infty} f_X(x) dx = 1`)

However, keep in mind that the probability density function can take values greater than 1, since its argument is not a probability but a probability density. The total densities of a *set* of elementary events make up a probability. 


Consider that

#mimath(`P_X(X=x) = P_X(x < X < x) = \int_{x}^{x} f_X(u) du = 0`)

This means that
- There are infinitely many possible events in $(a,b)$
- Therefore, the probability that an event takes place at exactly $(a,a)$ is $0$.

#line(length: 100%, stroke: 0.5pt + luma(200))

The cumulative distribution function of two jointly distributed random variables $X$ and $Y$ is defined as:

#mimath(`F_{X,Y}(x,y) := P(X \leq x, Y \leq y) = \int_{-\infty}^x \int_{-\infty}^y f_{X,Y}(u,v) dv du`)

where $f_\{X,Y\}(x,y)$ is the joint probability density function of $X$ and $Y$. The probability density function of $X$ can be obtained by taking the partial derivative of $F_\{X,Y\}(x,y)$ with respect to $x$:
#mimath(`f_{X,Y}(x,y) := \frac{\partial}{\partial x} F_{X,Y}(x,y) \quad \forall x,y \in \mathbb{R}`)


The expectation of a continuous random variable is defined as:

#mimath(`E[X] := \int_{-\infty}^{\infty} x f_X(x) dx`)

  
The definition of its variance and standard deviation are the same as in the discrete case.


$"Var"[X] := EE[(X-EE[X])^2] = EE[X^2] - EE[X]^2$



$sigma = sqrt("Var"(X))$



== Common Distributions

- Benoulli
	- Binary output (coinflip, yes/no ect.)
	- $P_X (x)=p^x (1-p)^(1-x)$ 
	- this way $P_X (1) = p$ and $P_X (0)=1-p$ 
	- Then $EE[X]=p$ and $"Var"[X]=p(1-p)$
- Beta
	- The Beta distribution elegantly captures both our *belief about the center* and our *confidence in that belief*!
	- 
$f_X (x)= Gamma(alpha + beta)/(Gamma(alpha)Gamma(beta))x^(alpha-1)(1-x)^(beta-1)$

	- where $Gamma(alpha + beta)/(Gamma(alpha)Gamma(beta))$ is the beta function $Beta(alpha, beta)^(-1)$
	- Note that $Gamma(x) =(x-1)!$
	- - $x^(alpha−1)$: Controls the behavior near $x=0$
	- $(1−x)^(beta−1)$: Controls the behavior near $x=1$
	- Together they create the characteristic Beta shape
		- // [image omitted: beta_function.png|500]
		- *Example*:
			1. Prior: Beta(1,1) - "No idea about coin bias"
			2. Flip coin 10 times: 7 heads, 3 tails
			3. Posterior: Beta(1+7, 1+3) = Beta(8,4)
			4. New belief: Coin is probably biased toward heads
	
// [image omitted: beta_dist.png]

- Multi-noulli
	- *Concrete Example: Rolling a die*
		If XX represents the outcome of rolling a 6-sided die:
		- $P_X (1)$ = "probability of rolling a 1"
		- $P_X (2)$ = "probability of rolling a 2"
		- $P_X (6)$ = "probability of rolling a 6"
		
		*For a fair die*: $P_X (1)=P_X (2)=dots=P_X (6)=1/6$
		*For a biased die*: Maybe $P_X( 6)=0.5$ and others are $0.1$ each
// [image omitted: multi-nouli_dist.png]
	
- Dirichlet
	- 
$f_X (x)=frac(Gamma(sum^d_(i=1)alpha_1),product^d_(i=1)Gamma(alpha_i))$

// [image omitted: dirichlet_dist.png]
	- *Example: Website A/B/C Testing*
		1. *Prior*: Dirichlet(1, 1, 1) - "no preference between variants"
		2. *Observe*: Variant A: 20 clicks, B: 15 clicks, C: 10 clicks
		3. *Posterior*: Dirichlet(21, 16, 11) - "updated beliefs"
		4. **repeat**
		5. At the end use results to predict new observations
- Uniform
	- *The constant $1/(b−a)$​:*
		- This is the *height* of the rectangular probability density
		- *Why this value?* Because the area must equal 1 (total probability)
		- **Area = height × width = $1/2(b−a)(b-a)=1$​ ✓
		- $EE[X]$ is the *midpoint* of the interval - so perfectly symmetric
		- $"Var"[X]$ Depends on the *width* of the interval
			- Wider interval → more spread → higher variance
	- *Intuitive Understanding*
		*"All outcomes equally likely"* - that's the essence of uniform distribution.
	- // [image omitted: Pasted image 20251115113127.png]
	
	- *Examples:*
		- *Fair die*: Each face has probability 1661​ (discrete uniform)
		- *Random time*: Arrival uniformly distributed between 2:00 PM and 4:00 PM
		- *Random number generator*: Most computers generate uniform random numbers on [0,1]
	
- Gaussian
	- Mean: $mu$
	- Standard deviation: $sigma$
	- 
$f_X (x)=cal(N)(x|mu,sigma^2) = 1/sqrt(2pi sigma^2) e^(-1/2((x-mu)/(sigma^2))^2)$

	- where 
$P(a<x<b)=integral^b_a f(x)"  "d x$

	- Here  $1/sqrt(2pi sigma^2)$ is the normalization, meaning that 
$integral_(-infinity)^(infinity) e^(-1/2((x-mu)/(sigma^2))^2)= sqrt(2pi sigma^2)$

	- and so 
$integral_(-infinity)^(infinity) 1/sqrt(2pi sigma^2) e^(-1/2((x-mu)/(sigma^2))^2) = 1$

	- // [image omitted: gaussian_dist.png]
- Multivariable Gaussian 
	- 
$f_X (x)=cal(N)(x|mu,Sigma)=1/sqrt(((2pi)^2)norm(Sigma))dot e^(-1/2(x-mu)^T Sigma^(-1)(x-mu) )$

	- // [image omitted: multivariable_gaussian_dist.png]
	- for the parameters 
$mu=vec(0,0) quad quad Sigma = mat(1,0;0,1)$



== Estimators And Bias

==== Estimator
A function that estimates a value. Very simple

Fx we can estimate a mean #mi(`\hat{\mu} = \frac{1}{n} \sum_{i=1}^n x_i`)

==== *Estimator Bias:* 
The *bias* of an estimator $f$ is defined as #mi(`B(f) = E[f(S)] - \theta`), where $B(f)$ is Bayes Error

An estimator is said to be *unbiased* if $B(f) = 0$.

However, the sample variance #mi(`S^2 = \frac{1}{n} \sum_{i=1}^n (X_i - \widehat{X})^2`) is a biased estimator of the variance #mi(`\sigma^2`) of a random variable $X$. That is why is it common to use the *unbiased sample variance* #mi(`\frac{1}{n-1} \sum_{i=1}^n (X_i - \widehat{X})^2`) instead of the sample variance $S^2$. The change made in the denominator is called *Bessel's correction*.

*Think of it like shooting at a target:*
- Unbiased estimator:
	- Your shots are *centered* on the bullseye (true value)
	- Sometimes you're high, sometimes low
	- *On average*, you hit the center
	
- Biased estimator:
	- Your shots are *systematically off-center*
	- Maybe always shooting a bit to the left
	- *On average*, you miss the target, but... in a predictable direction :O


== Inequalities

Markov's inequality

$P(X >= epsilon) <= (EE[X])/epsilon$

Therefore 
$P(X<epsilon)> 1-EE[X]/epsilon$

- *Key Properties*
		- *Minimal Requirements*: Only needs #mi(`X \geq 0`) - the most general assumption possible.
		- *One-Sided Bound*: Only controls upper tail #mi(`P(X \geq \epsilon)`), not deviations around the mean.
		- *No Sample Size Improvement*: The bound doesn't get better with more data.
		- *Often Vacuous*: Can give meaningless bounds like #mi(`P \leq 5/3 > 1`).
		
	- *Why It Matters:*
		- *Starting Point*: Foundation for deriving other inequalities
		- *Worst-Case Analysis*: When you know almost nothing about your random variable
		- *Quick Feasibility Check*: "Is this probability even theoretically possible?"
		
	- *Intuition*: If you only know the average, you can't say much about extremes. Like knowing the average income in a city tells you little about how many millionaires live there.


- Chebyshev's inequality
	- With Chebyshev's inequality, we are able to describe the same problem as an expression dependent on n (sample size). 
	- With this, probability of correctness will maybe increase as sample size increases
		- In other words, *the probability that our estimate is “correct” (i.e., close to the true mean) increases as the sample size grows*.
		- This is because of 
#mimath(`P(|X - \mu| \geq \epsilon) \leq \frac{\sigma^2}{\epsilon^2}`)
by this as $n->infinity$, $r h s->0$  
	- By this to get probability $<= delta$, you need ​$n<=sigma^2/(n epsilon^2)$ samples

		*Why This Is Better:*
		- *Variance Matters*: If you know values are tightly clustered (small #mi(`\sigma^2`)), deviations are less likely
		- *Sample Size Scaling*: More data → better estimates (empirical risk gets closer to true risk)
		- *Practical Bounds*: Often gives more meaningful bounds where Markov fails

		*Intuition*: Knowing both the average AND the spread lets you make much better predictions. Like knowing both average height and that heights cluster tightly around the mean.
	- // [image omitted: chebyshevs_inequality.png]
	- 
- WLLN (weak law of large numbers)
	- For i.i.d. samples where, mean $mu$ and $"Var"(X)=sigma^2$ 
	- Then sample mean (#mi(`\hat{y}_n`)) converges to $mu$ as sample size increases     
	- // [image omitted: WLLN.png]
- Hoeffding's inequality
	- Hoeffding's for intervals not just bounded by [0,1]
	- 
#mimath(`P\left(\frac{1}{n} \sum_{i=1}^n X_i - E[\frac{1}{n} \sum_{i=1}^n X_i] \geq \epsilon\right) \leq e^{-2n\epsilon^2/\sum_{i=1}^n(b_i-a_i)^2}`)

	- // [image omitted: cheb_vs_hoeff.png]
	To get probability $<=delta$, you need $n>=ln(1/delta)/(2epsilon^2)$​ samples
	
	The exponential concentration means that as you collect more training data, your confidence that the empirical risk approximates the true risk grows *exponentially fast*, not just linearly. This is why we can make strong theoretical guarantees about generalization in machine learning!
	

// [image omitted: which_inequality.png]


= Statistical Learning Theory

== Generalization bounds

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


== PAC Learnability

==== Realizability
A hypothesis space #mi(`\mathcal{H}`) is realizable with respect to a loss #mi(`\ell`) and data distribution $D$ if 
$exists h^* in cal(H) " such that " R(h)=0$
It trivially follows from this definition that #mi(`\min_{h \in \mathcal{H}} \widehat{R}_S(h)=0`) with probability 1 for a sample set $S$ collected i.i.d. from $D$. 
Hence, under the realizability assumption, all Empirical Risk Minimization (ERM) solutions 
#mimath(`h_S \in \arg \min_{h \in \mathcal{H}} \widehat{R}_S(h)`)
 give zero error.

- In other words, the hypothesis class $cal(H)$ is *rich enough* to contain the “true function.” (label function)
- If this is true, it is *possible* to achieve zero training error *and* zero true error.

This is just an assumption that makes PAC learning easier.

==== Representativeness
A training set $S$ is called #mi(`\epsilon`)-representative if

#mimath(`\forall h \in \mathcal{H}, |R(h) - \widehat{R}_S(h)| \leq \epsilon`)

This means that all hypothesis in the h-space, their risk must be epsilon similar to the generalization error

Intuition: The dataset is $epsilon$-representative if there exists a hypothesis with *minimal* error.

==== Uniform convergence
A hypothesis class #mi(`\mathcal{H}`) has the *uniform convergence property* if there exists a function #mi(`m_{\mathcal{H}}^{\text{UC}} : (\epsilon, \delta)^2 \to \mathbb{N}`) such that for every #mi(`\epsilon, \delta \in (0, 1)`) and every distribution $D$, any sampled dataset #mi(`S=\{(x_i,y_i) \overset{i.i.d.}{\sim} D : i = 1, \ldots, m\}`) is #mi(`\epsilon`)-representative with probability at least #mi(`1-\delta`).

This means that there exists a function that can tell how many data points you would need for a hypothesis, to know that for all distributions. It is $epsilon-$ representative with a $1-delta$ probability certainty

==== Agnostic PAC learnability
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

==== Bayes Error
Bayes error is the best achievable error once unavoidable factors like noise are taken into account.
$R^*="min"_(h´ in cal(H))R(h´)$
 This means that:
- Even with a perfect model and infinite data, you *cannot do better* than this error.
- The remaining error is due to *irreducible uncertainty*, such as:
    - noise in the data,
    - overlapping class distributions,
    - inherent randomness in the labels.
    
Then a hypothesis class is *PAC learnable* with respect to a data distribution $D$ if it admits zero Bayes error, i.e., $R^* = 0$.


== Bias-Complexity Dilemma (Trade-off)

==== No Free Lunch
Even though a perfect classifier $f$ exists (with $R(f)=0$), for any learning algorithm $A$ and some data distribution $D$.
If you randomly sample a training set $S$ from $D$, there is at least a $1/7$ chance that the algorithm $A$ will output a hypothesis with generalization error at least $1/8$.

No matter how good your algorithm is, there are situations (distributions and training sets) where it will fail to generalize well, even if a perfect solution exists. There is always a non-negligible probability of poor performance.

No algorithm is best at every task

// [image omitted: 8318F6F9-9A11-430C-BA13-35CC06D3F7AA.png]

The no free lunch theorem tells us only that we need to include a degree of bias to the learner. However, it does not tell anything about its consequences. Inducing too much bias limits the ability of the learner to explain the training observations. Inducing too little bias leads to overfitting. The goal is to find the right balance between the two. This dilemma is known as the *bias-complexity dilemma*. 

==== *Bias–complexity dilemma*
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

==== Bias-Variance decomposition
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


== Vapnik - Chervonenkis (VC) Dimension

==== Restriction
Given #mi(`S = \{x_1, \ldots, x_m \} \subset X`), the following set

#mimath(`\mathcal{H}_S = \{ (h(x_1), \ldots, h(x_m)) : h \in \mathcal{H} \}`)


is called a *restriction* of #mi(`\mathcal{H}`) to $S$. We can do this by

$|cal(H)|=2^(|S|)$


In other words, restriction in the discrete-label case (*don't worry about continuous labels here, trust me*), a *restriction* means evaluating hypotheses *only on a finite dataset*. This produces a *finite set of distinct labelings*, even if the original hypothesis space is infinite.

- *Example*
Consider restricting simple dataset with 3 points:
#mi(`S=\{(1,1),(2,1),(1,2)\}`)

===== Finding the Restriction $cal(H)_S$
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



==== Shattering
#mi(`\mathcal{H}`) is said to *shatter* $S$ if #mi(`|\mathcal{H}_S|= 2^{|S|}`).

In words, a hypothesis class #mi(`\mathcal{H}`) shatters from a dataset $S$ if the restriction of #mi(`\mathcal{H}`) to $S$ is the set of all functions from $S$ to #mi(`\{0,1\}`). This means that hypothesis class $cal(H)$ is so expressive that it can achieve *every possible labeling* of the dataset $S$.
use

#mi(`d_(V C)="max"\{m:T_(cal(H))(m)=2^m\} quad 😮`)

*Intuition:* If your hypothesis class can shatter a dataset, it means your hypothesis class is "complex enough" to memorize any labeling of those points - even completely random noise!

*Learning Theory Connection:*
- *Good:* Expressive enough to capture complex patterns
- *Bad:* So expressive it can memorize noise → overfitting risk
- *Key insight:* There's a maximum dataset size your hypothesis class can shatter → this is the *VC dimension*

==== Growth function
The growth function, $tau_(cal(H)): NN^+->NN^+$ of $cal(H)$ is defined as

$tau_(cal(H))(m):=max_(S in X^m)|cal(H)_S|$

The expression above determines the max number of distinct labels in a model that uses a dataset that is on the domain $X$. 

In this case $cal(m)$ is the number of data points in the dataset (on $X$) that the model uses.
// [image omitted: assets/image.png]

When using the growth function to find a shatter-able dataset its to answer the question 
"*What’s the largest dataset that H can fully control?*"



==== VC dimension
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


== Nonuniform Learnability

==== Formal definition
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

==== Intuition
With limited data, we restrict ourselves to simple hypotheses, then as data grows, we are allowed to consider more complex hypotheses.


= Bayesian Learning

== Maximum Likelihood Estimation (MLE)

MLE assumes that there is even more uncertainty and randomness than previorsly covered. MLE is like rewinding the data-generating process and asking which parameters could have most plausibly produced what we observed. (So we can replicate them)

Let 
#mimath(`S=\{x_1, x_2, \ldots, x_m\}`)

At the stage of modeling, we draw a hypothesis about how this data could have been generated. Let our hypothesis be that the data is generated as independent samples a distribution following a parametric density function #mi(`p(x|\theta)`), where #mi(`\theta`) is the parameter of the distribution. Then the probability density function of the random variable $S$ representing the occurrence of the dataset is given by

#mimath(`p(S|\theta) = p(x_1, x_2, \ldots, x_m | \theta) = \prod_{i=1}^m p(x_i|\theta).`)

If we find #mi(`\theta`) we can predict new data in the same shape as $S$

The expression above is a density function and is called the *likelihood* of parameters #mi(`\theta`) given the dataset $S$.

We can do this by maximizing the likelihood function #mi(`p(S|\theta)`).

This uses "log-likelihood". Where because for $p>0$, 
$log(product(P) = sum(log(p)))$
This way we can inspect each $p$. So


#mimath(`\theta_{MLE} = \arg\max_\theta \sum_{i=1}^m \log p(x_i|\theta).`)


==== Example
Let us give an example. Choose #mi(`p(x|\theta) = \mathcal{N}(x|\mu, \sigma^2) = \frac{1}{\sqrt{2\pi\sigma^2}}\exp\left(-\frac{(x-\mu)^2}{2\sigma^2}\right)`), where #mi(`\theta = (\mu, \sigma^2)`). 

Then consider that a single term of the log-likelihood function for a data point $x_i$ is given by

#mimath(`\log p(x_i|\theta) = -\frac{1}{2}\log(2\pi\sigma^2) - \frac{(x_i-\mu)^2}{2\sigma^2}.`)


We can then maximize the likelihood with respect to #mi(`\mu`) and #mi(`\sigma^2`) by setting the gradient of the full likelihood function to zero and solving for #mi(`\mu`).

#mimath(`\frac{\partial}{\partial \mu} \sum_{i=1}^m \log p(x_i|\theta) = \sum_{i=1}^m \frac{x_i-\mu}{\sigma^2} = 0 \implies \mu_{MLE} = \frac{1}{m}\sum_{i=1}^m x_i.`)

We can also solve for #mi(`\sigma^2`).

#mimath(`\frac{\partial}{\partial \sigma^2} \sum_{i=1}^m \log p(x_i|\theta) = -\frac{m}{2\sigma^2} + \frac{1}{2\sigma^4}\sum_{i=1}^m (x_i-\mu)^2 = 0 \implies \sigma^2_{MLE} = \frac{1}{m}\sum_{i=1}^m (x_i-\mu)^2.`)

Note that the parameters for a normal distribution is just the mean $mu$ and variance $sigma^2$

Now that we have found the parameters that fits the data $mu_"MLE"$ and $sigma^2_"MLE"$

Given a test input $x_*$, the learned model predicts the output in the form of a distribution, which is called the *predictive distribution*:


#mimath(`y_*|x \sim \mathcal{N}(y_*|w_{MLE}^T \phi(x_*), \sigma^2_{MLE})`)

This determines how likely an output is given an input. If the likelihood is good, when we can use

#mimath(`\hat{y} = w^T_{\text{MLE}}\phi(x_*)`)

*If* we have a safety-critical prediction task, we can also build confidence sets. For instance,

#mimath(`P\left(|y_* - w_{MLE}^T \phi(x_*)| \leq \sigma_{MLE}\right) = P\left(w_{MLE}^T \phi(x_*) - \sigma_{MLE} \leq y_* \leq w_{MLE}^T \phi(x_*) + \sigma_{MLE}\right)`)


#mimath(`= \int_{w_{MLE}^T \phi(x_*) - \sigma_{MLE}}^{w_{MLE}^T \phi(x_*) + \sigma_{MLE}} \mathcal{N}(y_*|w_{MLE}^T \phi(x_*), \sigma^2_{MLE}) dy_*`)


#mimath(`= \int_{w_{MLE}^T \phi(x_*) - \sigma_{MLE}}^{w_{MLE}^T \phi(x_*) + \sigma_{MLE}} \frac{1}{\sqrt{2\pi\sigma^2_{MLE}}}\exp\left(-\frac{(y_*-w_{MLE}^T \phi(x_*))^2}{2\sigma^2_{MLE}}\right) dy_*`)


#mimath(`= \int_{-1}^{1} \frac{1}{\sqrt{2\pi}}\exp\left(-\frac{z^2}{2}\right) dz = 0.68.`)



== Bayesian Learning

The MLE approach cannot model the potential dependency of the variance on the input $x$. That means that beacuse $sigma^2$ is a single number. The problem with having a *single variance* (#mi(`\sigma^2`)) is that it *assumes all inputs have the same uncertainty*, which is often unrealistic. Let me explain carefully.

The only source of uncertainty is on the precise values of the model parameters, which can be mitigated by collecting more data due to the law of large numbers. This ansatz is called *frequentist learning*. 

In real-world problems, the modeler is never certain about the model and in many cases they are interested in accounting for this source of uncertainty. 

*Bayesian learning* offers a way to do this. Let us see how it works on the same linear regression problem. 

#mimath(`w \sim \mathcal{N}(w|0, \alpha^{-1}I)`)


#mimath(`y_i|x_i, w \sim \mathcal{N}(y_i|w^T \phi(x_i), \sigma^2), \quad i=1,\ldots,m.`)

This time we are not assuming that $w$ (the distribution) is a fixed parameter, but a random variable. We start with the prior belief that #mi(`w \sim p(w)`). Based on the data, we update our belief about $w$ to $p(w|S)$. 

- $p(w)$ is called the *prior distribution* 
- and $p(w|S)$ is called the *posterior distribution*. 

The goal of *learning* in Bayesian machine learning is to calculate the posterior distribution $p(w|S)$ given the dataset $S$. This means that we start with a "unbiased" distribution and update it as we gather more information

This is done via the Bayes' rule:


#mimath(`p(w|S) = \frac{p(S|w)p(w)}{p(S)} = \frac{p(S|w)p(w)}{\int p(S|w)p(w) dw}.`)

The term in the denominator #mi(`p(S) = \int p(S|w)p(w) dw`) is called the *evidence* (also called the *marginal likelihood*). This means "Given our model (all possible parameter settings), how likely is it that we would see the data we actually observed?"

- In almost every real-world use case, the evidence is hard to calculate because of complexity. Therefore, we will not be able to calculate the posterior distribution exactly. Instead, we will use approximations. The whole field of Bayesian machine learning is about finding good approximations to the posterior distribution.

- The evidence quantifies the fit of the whole model family to data, as it can be viewed as the average likelihood with respect to the prior distribution. Therefore, it can be used for model selection. For instance, if we have two competing models $p(S,w)$ and $p'(S,w')$, we can choose the one with the higher evidence.

Let us next calculate the posterior distribution for the linear regression problem. We have:
  
#mimath(`\begin{align*}

p(w|S) &= \prod_{i=1}^m \mathcal{N}(y_i|w^T \phi(x_i), \sigma^2) \mathcal{N}(w|0, \alpha^{-1}I) \\

& = \mathcal{N}(w|0, \alpha^{-1}I) \prod_{i=1}^m \mathcal{N}(y_i|w^T \phi(x_i), \sigma^2) \\

& \propto \exp\left(-\frac{1}{2}w^T \alpha I w\right) \prod_{i=1}^m \exp\left(-\frac{1}{2\sigma^2}(y_i-w^T \phi(x_i))^2\right) \\

& \propto \exp\left(-\frac{1}{2}w^T \alpha I w - \frac{1}{2\sigma^2}\sum_{i=1}^m (y_i-w^T \phi(x_i))^2\right) \\

& \propto \exp\left(-\frac{1}{2}w^T \left(\alpha I + \frac{1}{\sigma^2}\sum_{i=1}^m \phi(x_i) \phi(x_i)^T\right) w + \frac{1}{\sigma^2}\sum_{i=1}^m y_i \phi(x_i)^T w\right) \\

& = \mathcal{N}\left(w\left|\left(\alpha I + \frac{1}{\sigma^2}\sum_{i=1}^m \phi(x_i) \phi(x_i)^T\right)^{-1}\frac{1}{\sigma^2}\sum_{i=1}^m y_i \phi(x_i), \left(\alpha I + \frac{1}{\sigma^2}\sum_{i=1}^m \phi(x_i) \phi(x_i)^T\right)^{-1}\right.\right).

\end{align*}`)


A neater notation would be possible considering that the posterior mean calculation involves the inverse of the posterior covariance.

#mimath(`p(w | S ) = \mathcal{N}(w|\mu_{post}, \Sigma_{post}),`)

where

#mimath(`\begin{align*}

\Sigma_{post} &= \left(\alpha I + \frac{1}{\sigma^2}\sum_{i=1}^m \phi(x_i) \phi(x_i)^T\right)^{-1}\\

\mu_{post} &= \Sigma_{post}\left ( \frac{1}{\sigma^2}\sum_{i=1}^m y_i \phi(x_i) \right ).

\end{align*}`)


  

For a new test input $x_*$, the Bayesian model predicts the output in the form of a distribution:

#mimath(`\begin{align*}

p(y_*|S, x_*) &= \int p(y_*|x_*, w) p(w|S) dw \\

&= \int \mathcal{N}(y_*|w^T \phi(x_*), \sigma^2) \mathcal{N}(w|\mu_{post}, \Sigma_{post}) dw \\

&= \mathcal{N}(y_*|\mu_{post}^T \phi(x_*), \sigma^2 + \phi(x_*)^T \Sigma_{post} \phi(x_*)).

\end{align*}`)

The resulting distribution $p(y_* | S, x_*)$ is called the *posterior predictive distribution*. This distribution has some remarkable properties:

- It considers all possible values of $w$ and averages over them proportionally to their posterior probability. This essential and unique property of Bayesian models is called *model averaging*. It is a way of accounting for model uncertainty.

- Its variance depends on the input $x_*$. Hence, it is able to take into account potential changes in the model confidence in different regions of the input space. This property of a probabilistic model is called *heteroscedasticity*. The Bayesian approach provides this property for free, whereas the frequentist approach requires a separate model for the variance.

In the cases where the predictor is a distribution, there are multiple ways one can use it to make a final prediction. For instance, one can choose the mode of the predictive distribution:

#mimath(`\widehat{y} = \arg\max_y p(y_* = y|S, x_*).`)

This is called the *Bayes predictor*. One can alternatively choose to take a sample from the predictive distribution:


#mimath(`\widehat{y} \sim p(y_*|S, x_*).`)

This is called the *Gibbs predictor*. Bayes predictor is known to be the optimal predictor in terms of the expected loss.
// [image omitted: Pasted image 20260121124740.png]
(Example from lectures of predicted)


== Maximum A-Posteriori Estimation

Much like in Bayesian Linear Regression, with *MAP* we can estimate the *posterior mean* of the weight distribution:


#mimath(`w_{\text{MAP}} = \arg\max_w p(w \mid S) = \arg\max_w p(S \mid w) \, p(w) = \arg\max_w \log p(S \mid w) + \log p(w)`)


The intractable denominator $p(S)$ drops out since it does not depend on $w$. This makes the training objective tractable.

In Bayesian linear regression, the posterior is Gaussian:


#mimath(`p(w \mid S) = \mathcal{N}\big(w \mid \mu_w, \Sigma_w\big)`)


where the posterior covariance $Sigma_w$ is fully determined by the prior and the data. Once $Sigma_w$ is known, the MAP estimate is simply the *posterior mean*:


#mimath(`w_{\text{MAP}} = \mu_w = \Sigma_w \left( \frac{1}{\sigma^2} \sum_{i=1}^m y_i \phi(x_i) \right)`)


For a given test input $x^*$, the MAP estimate predicts the output as:


#mimath(`y_* \mid x_* \sim p(y_* \mid w_{\text{MAP}}, x_*)`)


Since the MAP estimate is a *point estimate*, it does not capture the uncertainty in the model parameters. Once you have #mi(`\Sigma_w`)​, computing #mi(`\text{MAP}`)​ is just *matrix multiplication.* 

Because of this, it is not considered a fully Bayesian approach, as it cannot perform model averaging over the posterior.

To do model averaging, you would need the average predictions over the posterior including all its uncertainties.

MAP *ignores this uncertainty*, so it can’t capture:
- Variance in predictions due to uncertain parameters
- Risk of overconfident predictions if data is scarce
- True Bayesian model averaging benefits


== Monte Carlo Integration

Consider an integral of the form:

$EE[f(x)]=I = integral f(x) p(x) dif x.$

for some function $f(x)$ and probability density function $p(x)$. If we can take $m$ samples from $p(x)$, we can approximate the integral by the sample average:

#mimath(`I \approx \frac{1}{m}\sum_{i=1}^m f(x_i),`)

where #mi(`x_i \sim p(x)`). This is called the *Monte Carlo integration*.


== Generative Models

For a supervised learning problem where the data comes from #mi(`x,y \sim D`) for an unknown data distribution $D$. We can approach the modeling problem in two ways based on what we want to approximate about the data distribution. Consider that the joint distribution factorizes in two ways:
$p(x,y) = p(y|x)p(x)$

$p(x,y) = p(x|y)p(y)$

We can choose the first factorization and account for $p(x)$ using Monte Carlo integration based on the training samples. Then it suffices to approximate $p(y|x)$. This approach is called *discriminative modeling*. The probabilistic and Bayesian linear regressors we developed above were all discriminative models since they fitted $p(y|w,x)$ to data.

An alternative approach would be to choose the second factorization: $p(x,y) = p(x|y)p(y)$ and aim to approximate both $p(y)$ and $p(x|y)$. Notice that this approach attempts to infer the whole data generating process, where a label is first created and the related input is generated. A real-world example could be a person choosing the digit to draw first and then drawing it. The picture of the resulting drawing because the input of the aimed classifier. This approach is called *generative modeling*.

For instance, we can fit a generative model on a data set #mi(`S = \{(x_1, y_1), (x_2, y_2), \ldots, (x_m, y_m)\}`) by choosing a parametric distribution family #mi(`p(x|y,\theta)`). For a classification problem, we can approximate $p(y)$ by class frequencies, i.e.

#mi(`P(y=c) = \frac{1}{m}\sum_{i=1}^m \mathbb{1}(y_i = c)`).

and choose the class-conditional distribution to be:


#mimath(`p(x|y=c, \theta_c) = p(x|\theta_{c}),`)


that is, each class shares the same distribution family but has its own parameters. Then we can fit the parameters #mi(`\theta_c`) on the training samples belonging to class $c$ by maximum likelihood estimation:


#mimath(`\theta_c^{MLE} = \arg\max_{\theta_c} \sum_{ \{i : y_i = c\} } \log p(x_i|\theta_c).`)

Given a test input $x_*$, the generative model predicts the output in the form of a distribution:

#mimath(`p(y=c|x_*) = \frac{p(x_*|y=c, \theta_c)P(y=c)}{\sum_{c'} p(x_*|y=c', \theta_{c'})P(y=c')}.`)



== Naive Bayes Classifier

In the generative classification example above, consider the case that the class-conditionals are normal distributions with full covariance matrices:

#mimath(`p(x|y=c, \theta_c) = \mathcal{N}(x|\mu_c, \Sigma_c).`)

Then the training set will be used to fit $d+d(d+1)/2$ parameters per class, where $d$ is the dimensionality of the input. The first term is due to the parameters of the mean and the rest is due to the parameters of the covariance matrix. This is a lot of parameters to fit. We can instead make the simplifying assumption that the class-conditionals of the individual features are independent. Then we have:

#mimath(`\begin{align*}

p(x|y=c, \theta_c) &= \prod_{j=1}^d p(x_j|\theta_{j,c}) \\

&= \prod_{j=1}^d \mathcal{N}(x_j|\mu_{j,c}, \sigma_{j,c}^2).

\end{align*}`)

The assumption that the class-conditionals factorize across individual features is called the *naive Bayes assumption*. For the normal distributed class-conditionals used in our example, the resulting model will then have only $2d$ parameters per class. The resulting generative classifier is called a *naive Bayes classifier*. Applied to normal distributed class-conditionals, the naive Bayes classifier follows the same formulas as above with the assumption that the covariance matrix is diagonal.


$P(x_1=a,x_2=b|y=c) = ("#"(x_1=a)and"#"(y=c))/"#"(y=c) dot ("#"(x_2=a)and"#"(y=c))/"#"(y=c)$


#mi(`P(underbrace(y=c^*, "prediction")|underbrace((x_1=a^*,x_2=b^*),"query input"))=(P(x_1=a^*|y=c^*)dot P(x_2=b^*|y=c^*) dot p(y=c^*))/(sum_(c^* in underbrace(Y, \{y_1\}))P(x_1=a^*|p=c^*)dot P(x_2=b^*|p=c^*)dot P(p=c^*))`)



= Kernel Methods

== Transforming the input space

Suppose that we have a dataset where we can't easily classify data using a support vector (4. Support Vector Machines (SVM)). We can transform the space using a transformation function, to model the data by using more dimentions. Take the example from the lectures

$T(x,y)=(x^2,y^2,sqrt(2)x^2 y^2)$

// [image omitted: Pasted image 20260122111142.png]

This way we can use a hyperplane of $+1$ dimensionality to classify the data.


== Kernel Trick

Suppose that $T(x)$ is a mapping to a higher dimension

Note that algorithms like SVM only need *dot products*

That means that with
#mimath(`k(x_i,x_j) = \phi(x_i)^T\phi(x_j)`)

Because of the definition of the inner product, we can describe the relation between two points using $k(x_i,x_j)$


Computing #mi(`\phi(x)`) explicitly can be *very expensive or infinite-dimensional*

- *Linear Kernel:* #mi(`k(x,y) = x^\top y`)
- *Polynomial Kernel:* #mi(`k(x,y) = \left( x^\top y + c \right)^d`)
- *Radial Basis Function (RBF) Kernel:* #mi(`k(x,y) = \exp(-\frac{\| x-y \|^2}{2\sigma^2})`)
- *Sigmoid Kernel:* #mi(`k(x,y) = \tanh\left( \gamma x^\top y + c \right)`)


== Kernel regression

_(no notes yet)_


== Support Vector Machines (SVM)

Among many possible hyperplanes that may separate the data points of two classes, SVM chooses the one that maximizes the smallest distance between a data point and the hyperplane. 

This distance is called the *margin*. The data points that are closest to the hyperplane are called *support vectors*.

Note that we use because of these property, SVM is also called a *maximum margin classifier*. We would normally use k-fold cross validation, and balance the bias-variance tradeoff to ensure a maximum margin.

// [image omitted: Pasted image 20260120175429.png]
In this example is 2d, where the support vectors are lines


= Ensemble Methods

== Bootstrap Aggregation (Bagging)

_(no notes yet)_


== Boosting

_(no notes yet)_


== Federated Learning

_(no notes yet)_

