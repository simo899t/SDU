- Find the likelihood function for this distribution on the dataset $S$.
	- Likelihood is $P(theta|n,k) = vec(n,k)theta^(k)(1-theta)^(n-k)$
	- then $(n!)/(k!(n-k)!)theta^(k)(1-theta)^(n-k)$
	- then $P(theta|S) = product^m_(i=1) P(theta|x_i,k)$ where $k=3$
- Identify the model parameters.
	- This is $theta$
- Find the maximum likelihood estimate of the parameter θ.
- Make the model Bayesian by introducing a prior distribution on the Binomial distribution parameter $\theta$. Let the prior be a Beta distribution with parameters α=1 and β=2.
- Calculate the posterior distribution.
- Normalize the posterior distribution with the normalization constant.
- Hint: If the PDF of the Beta distribution is given as $p(\theta|\alpha, \beta) = \theta^{\alpha-1}(1-\theta)^{\beta-1} / Z$,
then:

$p(\theta|A+\alpha, B+\beta)/Z'$ is still a Beta distribution.

- The normalization constant for the Beta distribution is given by $C = \text{Beta}(a, b) = \frac{\Gamma(a)\Gamma(b)}{\Gamma(a+b)}$, where for positive integers, $\Gamma(x) = (x-1)!$.

- Use the `gamma` function from the `scipy.special` library to calculate the normalization constant.

  

- Predict 20 new predictions for the random variable $X$ using 3 trials and parameter $\theta$ sampled from posterior disribution.

- Calculate their mean.

- Calculate their standard deviation.