#### Unimodality
There exists a uniqe optimizer $x^*$ for the function $f$ . Global minimum.

#### Finding an initial bracket
We know that the global minimum is $in[a,c]$ when
$$a<b<c " and "f(a)>f(b)<f(c)$$

We start of by making a guess, then move around the interval to guarantee this. Then we can shrink the interval later.

We can eliminate great parts of the space, by setting 2 function evaluations $epsilon$ close to each-other, because we now know in which direction the minimum is.

#### Fibonacci Search Algorithm
We can backtrack the Fibonacci sequence to find shrink the interval within $n$ iterations.
![[Pasted image 20260217114816.png]]

#### Golden Section Search
$$lim_(n->oo) F_(n+1)/F_n = lim_(n->oo) 1/(rho_n) = lim_(n->oo) phi.alt (1s^(n+1))/(1-s^n) = phi.alt approx 1.61803$$
![[Pasted image 20260217114950.png]]

#### Quadratic Fit Search
Iteratively fits quadratic function to three bracketing points
![[Pasted image 20260217115310.png]]

#### Lipschitz continuous
This means that there is a bound for the derivative
ex. an exponential function derivative is unbound, because it keeps 
growing forever.
A function $f$ is Lipschitz continuous on $[a, b]$ if there exists an $ell > 0$ such that:
$$|f(x)-f(y)| <= ell(|x-y|), forall x,y in [a,b]$$

#### Shubert-Piyavskii Method
Requires that the function is Lipschitz continuous
![[Pasted image 20260217115434.png]]