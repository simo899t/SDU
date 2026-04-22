In the generative classification example above, consider the case that the class-conditionals are normal distributions with full covariance matrices:
$$p(x|y=c, \theta_c) = \mathcal{N}(x|\mu_c, \Sigma_c).$$
Then the training set will be used to fit $d+d(d+1)/2$ parameters per class, where $d$ is the dimensionality of the input. The first term is due to the parameters of the mean and the rest is due to the parameters of the covariance matrix. This is a lot of parameters to fit. We can instead make the simplifying assumption that the class-conditionals of the individual features are independent. Then we have:
$$\begin{align*}

p(x|y=c, \theta_c) &= \prod_{j=1}^d p(x_j|\theta_{j,c}) \\

&= \prod_{j=1}^d \mathcal{N}(x_j|\mu_{j,c}, \sigma_{j,c}^2).

\end{align*}$$
The assumption that the class-conditionals factorize across individual features is called the **naive Bayes assumption**. For the normal distributed class-conditionals used in our example, the resulting model will then have only $2d$ parameters per class. The resulting generative classifier is called a **naive Bayes classifier**. Applied to normal distributed class-conditionals, the naive Bayes classifier follows the same formulas as above with the assumption that the covariance matrix is diagonal.

$$P(x_1=a,x_2=b|y=c) = ("#"(x_1=a)and"#"(y=c))/"#"(y=c) dot ("#"(x_2=a)and"#"(y=c))/"#"(y=c) $$
$$P(underbrace(y=c^*, "prediction")|underbrace((x_1=a^*,x_2=b^*),"query input"))=(P(x_1=a^*|y=c^*)dot P(x_2=b^*|y=c^*) dot p(y=c^*))/(sum_(c^* in underbrace(Y, {y_1}))P(x_1=a^*|p=c^*)dot P(x_2=b^*|p=c^*)dot P(p=c^*))$$