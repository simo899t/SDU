#import "@local/tempst:0.1.0": *
#import "@preview/mitex:0.2.7": mi, mimath

#show: note.with(
  title: "Probability Theory",
  course: "AI512 — Introduction to Machine Learning",
  author: "Simon Holm",
  date: "2026-09-04",
)

= Basic Concepts

*Intuitive meaning of probability:* how often an event happens if you repeat a random experiment many times.

#mimath(`P(A) = \lim_{n \rightarrow \infty} \frac{n_A}{n}`)

=== Sample space
The *sample space* is all the possible outcomes.
- Example: Rolling one die → #mi(`\Omega = {1, 2, 3, 4, 5, 6}`)

=== Event $bold(A)$ 
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

=== Probability Space: 
A tuple defined as #mi(`(\Omega, \sigma(\Omega), P)`).

=== Inclusion-Exclusion Principle:

#mimath(`P(A \cup B) = P(A) + P(B) - P(A \cap B)`)

This takes into account the fact that #mi(`P(A \cap B)`) is counted twice in $P(A) + P(B)$. A direct consequence of this is that #mi(`P(A \cup B) \leq P(A) + P(B)`) which is called the *union bound*.
=== Conditional Probability:

#mimath(`P(A|B) = \frac{P(A \cap B)}{P(B)}`)

The intuitive meaning of this is the probability of event $A$ given that event $B$ has occurred. That is, the frequency of event $A$ in the subset of trials where event $B$ has occurred. In mathematical terms

#mimath(`P(A|B) = \lim_{n \rightarrow \infty} \frac{n_{A \cap B}}{n_B}`)

The definition of conditional probability can be rewritten as #mi(`P(A \cap B) = P(A|B)P(B)`). This is called the *product rule*.

=== *Independence:* 
If the probability of event $A$ is not affected by the occurrence of event $B$, these two events are said to be independent.

In terms of conditional probabilities we can describe this situation as $P(A|B) = P(A)$. Applied to the definition of conditional probability, this means that $A$ and $B$ are independent if and only if 
$P(A sect B) = P(A)P(B) quad "independent"$

$P(A sect B) != P(A)P(B) quad "dependent"$

=== *Law of Total Probability:* 
If #mi(`B_1, B_2, \dots, B_n`) is a partition of #mi(`\Omega`), i.e. #mi(`B_i \cap B_j = \emptyset`) for all #mi(`i \neq j`) and #mi(`\bigcup_{i=1}^n B_i = \Omega`), then 
#mimath(`P(A) = \sum_{i=1}^n P(A|B_i)P(B_i)`)

=== *Bayes' Rule:* 

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


= Random Variables

It is not always convenient to describe sets. We can facilitate this by defining a random variable.

A *random variable* is function #mi(`X: \Omega \rightarrow \Lambda`) that maps each elementary event #mi(`\omega \in \Omega`) to an element on its *range* #mi(`\lambda \in \Lambda`). We can define a probability measure on #mi(`\Lambda`) as follows: 
#mimath(`P_X(A) := P(\{\omega \in \Omega: X(\omega) \in A\})`)

where #mi(`A \subset \Lambda`). This is called the *induced probability measure* of $X$. 

*Intuition:* *measures the outcome* as a number.

=== Example of random variable
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



= Expectance and Variance

The *expectation* (a.k.a. *expected value*) of a random variable $X$ is defined as:

#mimath(`E[X] := \sum_{x \in \Lambda} x P_X(x)`)

Take the three coin tosses example from 2. Random Variables where the random variable $X$ indicates the number of heads, $EE[X] = 1.5$ because $EE[X] = 0 dot 1/8 + 1 dot 3/8 + 2 dot 3/8 + 3 dot 1/8 = 1.5$.

The expectation of a random variable is also called its *mean*. The expectation of a random variable is a measure of its *central tendency*. However, it does not tell us anything about the *spread* of the random variable. 

This can be measured by the *variance* of a random variable which is defined as:

$"Var"[X] := EE[(X-EE[X])^2] = EE[X^2] - EE[X]^2$

In the above example, $Var[X] = 0.75$ because 
$"Var"[X] = EE[X^2] - EE[X]^2 = 0^2 dot 1/8 + 1^2 dot 3/8 + 2^2 dot 3/8 + 3^2 dot 1/8 - 1.5^2 = 0.75$

The square-root of the variance is called the *standard deviation* of a random variable. Standard deviation is commonly denoted by #mi(`\sigma`). In the above example, #mi(`\sigma(x) = \sqrt{0.75} = 0.866`).

=== Moments
Generally speaking, a *moment* of a random variable is defined as:

$EE[(X-EE[X])^k]$

where $k$ is a positive integer. The first moment is the expectation itself. The second moment is the variance. The third moment is called the *skewness* of a random variable. It is a measure of the asymmetry of the distribution of a random variable. The fourth moment is called the *kurtosis* of a random variable. It is a measure of the heaviness of the tails of the distribution of a random variable.

#line(length: 100%, stroke: 0.5pt + luma(200))
== Common rules for Expectance and Variance

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



= Continuous Random Variables

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

== Example
- Bus arrives uniformly at any time in $[0,60]$ minutes
- $X(w)=w$ (random variable is just the arrival time)
- $A=[0,15]$ minutes (we want to know the prop. that the bus arrives in between 15 and 45 minutes.)
- Pre-image is then $X^(-1) (A) =[15,45]$ in $Omega$ 
- Probability is therefore: 

$P_X (A) = P(X^(-1) (A)) =P(w in [15,45]) = (45-15)/60 = 1/2$

The #mi(`\sigma`)-algebra of #mi(`\mathbb{R}`) by the intervals of the form $(a,b)$ is called the *Borel #mi(`\sigma`)-algebra*. 
== Cumulative distribution function (CDF)
We can also determine probability given that $X$ is $<=$ some number $x$:

#mimath(`F_X(x) := P(X \leq x) \quad \forall x \in \mathbb{R}`)

Note that this is a non-decreasing function (probabilities goes up as $x$ increases).

== Probability density function (PDF)

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



= Common Distributions

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



= Estimators And Bias

=== Estimator
A function that estimates a value. Very simple

Fx we can estimate a mean #mi(`\hat{\mu} = \frac{1}{n} \sum_{i=1}^n x_i`)

=== *Estimator Bias:* 
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


= Inequalities

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

