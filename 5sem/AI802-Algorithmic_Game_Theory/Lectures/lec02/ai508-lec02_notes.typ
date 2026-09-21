#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         "Lecture 2: Probability Recap",
  author:        "Simon Holm",
  course:        "AI508 — Algorithmic Game Theory",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

= Probability spaces, outcomes, and events 
A probability model consists of a sample space $Omega$ of possible outcomes $omega$ together with probabilities assigned to events $A$.

#definition(title: "Definition: Outcome and event")[
  An _outcome_ is a single element $omega in Omega$. And _event_ is a subset $A subset Omega$
]

#example(title: "Example: Different sample spaces")[
  If two fair coins are tossed, then
  $ Omega = {H H,H T, T H, T T} $
  An event "exactly one head" would then be
  $ H T, T H $
]

For a finite or countable sample space, probabilities satisfy
$ PP(A) = sum_(w in A) PP(omega), quad sum_(omega in Omega) = 1. $

Remember that 
$ P(A^C) = 1-P(A) $
$ P(A union B) = P(A) + P(B) - P(A inter B) $

#pagebreak()

= Random variables

#definition(title: "Definition: Random Variable")[
  The random variable $X$ is a function that maps
  $ X : Omega -> RR. $
]

#example(title: "Example: Coin")[
  Two coins giving heads
  $ X = {0,1,2} $

  Where,
  - $0 = X{T T}$, 
  
  - $1 = X{T H} = X{H T}$, 
  
  - $2 = X{H H}$
]

= Discrete vs continuous random variables
The distinction between discrete and continuous refers to the _distribution of $X$_, not to whether the underlying sample space $Omega$ is discrete or continuous.

$ EE[X] = sum_(x in X) x dot P(X=x) $

#example(title: "Example: Continuous sample space, discrete random variable")[
  Given the uniform probability distribution $Omega = [0,1]$, then 
  $ X(omega) = cases(-1 quad & w <=1/2, 1  & w > 1/2) $
  Although $Omega$ is continuous, $X$ takes only two values. Hence $X$ is _discrete_, with
  $ PP(X = -1) = PP(X=1) = frac(1,2,style:"skewed") $
]

#example(title: "Example: Continuous random variable on the same space")[
  Again let $Omega = [0,1]$ be uniform, but now define $ X(omega) = Omega $
  Then $X tilde "Uniform"[0,1]$, so $X$ is a continuous random variable.
]
#pagebreak()

For a discrete random variable, we write its probability mass function as
$ p_X (x) = PP(X = x) $
Where the probability satisfies
$ sum_x p_X (x) = 1 $

= Joint distributions and marginalization
#definition(title: "Definition: Joined distribution")[
  Given that $X,Y$ are random variables defined on the same probability space. Then their joint distribution is
  $ p_(X Y) (x,y) = PP(X=x,Y=y) $
]

#definition(title: "Definition: Marginalization")[
  If we know the joint distribution but only care about $X$, we sum out the possible values of $Y$:
  $ p_X (x)= PP(X=x) = sum_(y in Y) p_(X Y) (x,y) $
  And similarly
  $ p_Y (y)= PP(Y=y) = sum_(x in X) p_(X Y) (x,y) $
]

#example(title: "Example: Marginalization")[
  Suppose that,
  #figure(
    table(
    columns: 3,
    rows: 3,
    align: center + horizon,
    [], [*$Y=0$*], [*$Y=0$*],
    [*$X=0$*], $0.2$, $0.3$,
    [*$X=1$*], $0.1$, $0.4$
  ),
    caption: [Example table],
  ) <label>
  
  Then $ PP(X=0) = 0.2+0.3=0.5, quad PP(X=1) = 0.1+0.4=0.5 $
]


#pagebreak()

= Expectation
#definition(title: "Definition: Expectation of a random variable")[
  For a discrete random variable X, its expectation is
$ EE[X] =sum_x x PP(X = x) $
whenever the sum is well defined. For a countably supported random variable, a sufficient
condition is $EE[X] < oo$.
]

#example(title: "Example: Expectation of a discrete random variable")[
  If X takes values $−1, 0, 1$ uniformly, then
  $ EE[X^2] = (-1)^2 1/3 + 0^2 1/3+ 1^2 1/3 = 2/3 $
  We did not need to separately compute the distribution of $X^2$
]

#definition(title: "Definition: The lazy statistician rule (LOTUS)")[
  Suppose we want the expectation of a function $f(X)$. We do not need to first derive the distribution of $f(X)$. Instead,
  $ EE[f(X)] = sum_x f(x) PP(X=x) $

  for a discrete random variable. This identity is commonly called the _Law of the Unconscious Statistician_ (LOTUS); informally, it is the “lazy statistician rule.”
]

#proof(title: "Proof: LOTUS")[
  To see why it holds, start from the definition of the expectation of $f(X)$:
  $ EE[f(X)] = sum_z z PP(f(X)=z) $
  Now group together all values x for which $f(x) = z$:
  $ PP(f(X) = z) = sum_x (PP(X=x)) where x:f(x)=z $
  So...
  $ EE[f(X)] &= sum_z z sum_x (PP(X=x)) where x:f(x)=z \
    &= sum_x f(x) PP(X = x). $
    #QED
]

#definition(title: "Definition: Bernoulli")[
  Given that $X = cases(1 quad &p, 0 &1-p)$

  So that $EE[X] = 1p + 0(1-p) = p$
]

#theorem(title: "Theorem: Indicators and counting")[
  An indicator random variable for an event A is
  $ bb(1)_A = cases(1"," quad A "occurs,", 0"," quad A "does not occur.") $
  Since $bb(1)_A$ is Bernoulli,
  $ EE[bb(1)_A]= PP(A) $
  This makes indicator variables extremely useful for counting.
]


= Variance and covariance

#definition(title: "Definition: Variance")[
  Variance captures the spread of value around the
  $ Var(X) = EE[(X - EE[X])^2] = EE[X^2] - EE[X]^2 $
]

#example(title: "Example: Variance of a Bernoulli")[
  Given $X$ where $ X = cases(1 quad &p, 0 &1-p) quad"with" EE[X] = p $,
  $ Var(X) = EE[X^2] - EE[X]^2 = (1^2 p + 0^2 (1-p)) - p^2 = p - p^2 = p(1-p) $
]

#definition(title: "Definition: Covariance")[
  Given $X, Y$,

  $ Cov(X, Y) &= EE[(X- EE[X]) (Y - EE[Y])] ) \ 
    &= EE[X Y + EE[X] EE[Y] - EE[Y] - Y EE[X]] \
    &= EE[X Y] + EE[X]EE[Y] - EE[X] EE[Y] - EE[Y EE[X]] \
    &= EE[X Y] - EE[X]EE[Y] $
]
#pagebreak()

#definition(title: "Definition: Uncorrelation of 2 random variables")[
  2 R.V's are uncorrelated if 
  
  $ Cov(X,Y) = 0 iimp EE[X Y] = EE[X]EE[Y] $

  This does not generally mean $EE[X Y] = 0$ unless at least one of the means is zero.
]



== Independence implies zero covariance
#definition(title: "Definition: Independence")[
  $X,Y$ are independent if
  $ p_(X Y)(x,y) = p_X (x) p_Y (y) quad forall (x,y) $
]


If $X$ and $Y$ are independent, then their joint mass function factorizes:
$ p_(X Y) (x,y) = p_X (x) p_Y (y) $
Therefore 
#proof(title: "Proof")[
  $ EE[X Y] &= sum_x sum_y x y p_(X Y) (x,y) \
            &= sum_x sum_y x y p_(X) (x) p_Y (y) \
            &= (sum_x x p_X (x)) (sum_y y p_Y (y)) \
            &= EE[X]EE[Y] $

  Because of this
  #boxed($ X bot Y iimp Cov(X, Y) = 0 $)
  #QED
]
The converse is false in general.

== Gaussian exception
For jointly Gaussian random variables, zero covariance does imply independence. Thus *if* $(X, Y)$ is *jointly Gaussian*,
$ Cov(X, Y) = 0 bii X bot Y $
The assumption that they are _jointly_ Gaussian is important; it is not enough for the two marginal distributions to be Gaussian separately.
#pagebreak()

= Conditional probability

#definition(title: "Definition: Conditional probability")[
  Assume there are 2 events $A$ and $B$ and let $P(B)>0$
  
  Then the probability of event $A$ given $B$ is:
  $ P(A mid B) = P(A inter B)/P(B) $

  Conditioning means that we restrict our attention to outcomes in which B has occurred and renormalize probabilities within that restricted set.
]

#example(title: "Example: Conditional probability")[
  Given $B={x>3} = {4,5,6}$

  The probability that $A="EVEN" = {2,4,6}$ is:
  $ P(A inter B)/P(B) = (2/6)/(3/6) = 2/3 $
]

#example(title: "Example: Conditional probability with random variables")[
  $ PP(X = x mid Y = y) = (PP(X=x,Y=y))/PP(Y=y), quad PP(Y=y) > 0. $
]


#definition(title: "Definition: Law of total probability")[
  If $B_1,dots,B_m$ partition the sample space and $PP(B_i) >0$, then
  #boxed($ PP(A) = sum_(i=1)^(m) PP(A mid B_i)PP(B_i) $)
  Indeed,
  $ PP(A) &= sum_(i=1)^(m) PP(A cap B_i) \ 
          &= sum_(i=1)^(m) PP(A mid B_i)PP(B_i) $
]
#pagebreak()

#definition(title: "Definition: Bayes Rule")[
  Given $A,B$
  #boxed($ PP(A mid B) = (PP(B mid A) PP(A))/PP(B) $)
  Where $PP(B mid A)$ is the _likelihood_, $PP(A)$ is the _prior_ and $PP(B)$ is the _marginal evidence_

  A useful interpretation
  $ "posterior" = ("likelihood" times "prior")/"evidence" $
]

#definition(title: "Definition: Independent events of a random variable")[
  Event $A$ and $B$ are independent if anf only if
  $ PP(A inter B) = PP(A)PP(B) $
]

#theorem(title: "Theorem: " + $A "and" A^c$)[
  If $A$ and $A^c$ are the two possibilities, then
  $ PP(B) = PP(B mid A)PP(A) + PP(B mid A^c)PP(A^c) $
  Thus the denominator normalizes the updated belief by accounting for all ways in which the evidence could have arisen.
]
#pagebreak()




= Conditional expectation
Note that conditional expectations are not scalars,

#definition(title: "Definition: Conditional expectation")[
  Given $Y$
  $ EE[X mid Y = y] = sum_(x in X) x dot P(X = x mid Y = y) $
]

#theorem(title: "Theorem: Law of Total Expectation")[
  Given $X$ and $Y$,
  #boxed($ EE[X] = EE[EE[X mid Y = y]] = sum_(y in Y) (sum_(x in X ) x dot P(X = x mid Y = y)) P(Y = y) $)
]

#proof(title: "Proof of LoTE")[
  Starting from the definition,
  $ EE[X] = sum_x x PP(X=x) $
  Now marginalize over $Y$
  $ PP(X=x) = sum_y PP(X=x,Y=y) $
  Therefore
  $ EE[X] &= sum_x x sum_y PP(X = x, Y = y) \
          &= sum_y sum_x x PP(X = x, Y = y) \
          &= sum_y sum_x x PP(X=x mid Y=y)PP(Y=y) \
          &= sum_y P(Y=y) underbrace(sum_x x PP(X=x mid Y=y), EE[X mid Y=y]) \
          &= sum_y EE[X mid Y=y]PP(Y=y) \
          &= EE[EE[X mid Y]] $
]
For countable supports, the same proof works whenever the relevant sums are absolutely convergent, for example when $EE[X] < oo$.

#pagebreak()


= Continuous random variables
A continuous random variable is described by a probability density function $f_X$ satisfying
$ f_X (x) >= 0, quad integral_(-oo)^(oo) f_X (x) dif x = 1. $
Probabilities are then obtained by integration:
$ PP(a<=X<=b) = integral_(a)^(b) f_X (x) dif x $

For a continuous random variable $X$,
$ PP(X=x) = 0 $
for every individual point $x$.

#definition(title: "Definition: Expectation of a continues random variable")[
  The expectation of a continuous random variable is
  #boxed($ EE[X] = integral_(-oo)^(oo) x f_X (x) dif x $)
]

#definition(title: "Definition: LOTUS, the continuous version")[
  The continuous version of LOTUS is
  #boxed($ EE[g(X)] = integral_(-oo)^(oo) g(x) f_X (x) dif x $)
]

For jointly continuous $X,Y$ with joint density $f_(X,Y)$, marginalization becomes
#boxed($ f_X (x) integral_(-oo)^(oo) f_(X,Y) (x,y) dif y $)

#pagebreak()

= Gaussian distributions
== Univariate Gaussian
A Gaussian random variable with mean $mu$ and variance $var$ is written as:
$ X from normal(mu, var) $
and has density
#boxed($ f_X (x) = 1/sqrt(2 pi var) exp(-((x-mu)^2)/(2var)). $)
The mean $mu$ controls the center, while $var$ controls the spread.
#figure(
  image("assets/image.png"),
  caption: [The density of a standard Gaussian $normal(0, 1)$.],
)
== Multivariate Gaussian
A random vector $X in RR^d$ is multivariate Gaussian with mean vector $mu in RR^d$ and positive-definite _covariance_, matrix $Sigma in RR^(d times d)$ if
$ X from normal (mu, Sigma) $
with density
#boxed($ f_X (x) = 1/((2 pi)^(frac(d,2,style: "skewed") abs(Sigma)^frac(1,2,style: "skewed"))) exp(-1/2 tran((x-mu)) inv(Sigma))). $)
The covariance matrix controls both the spread in each coordinate and the dependence between coordinates.


In to dimensions
$ Sigma = mat(var_1, Cov(X_1,X_2); Cov(X_1,X-2), var_2) $
If the off-diagonal is zero, the contours are axis-aligned. A nonzero covariance rotates the elliptical contours.
#figure(
  image("assets/image-1.png"),
  caption: [Schematic level sets of two-dimensional Gaussian densities. For jointly Gaussian
coordinates, zero covariance implies independence.],
) <label>

= Markov and Chebyshev inequalities
#definition(title: "Definition: Markov's inequality")[
  Let $X$ be a non negative random variable, then.
  $ P(X>=a) <= EE[X]/a $
]

#proof(title: "Proof: Markov's inequality")[
  Let $X$ be a nonnegative discrete random variable

  $ EE[X] &= sum_x x PP() $


  $ EE[X] &= sum_(x>=0) x dot P(X=x) \
  &= sum_(x=0)^a x dot P(X=x) + sum_(x>=a) x dot P(X=x) \
  &= sum_(x>=a) x dot P(X=x) >= a sum_(x>=a) P(X=x) \ &= a PP(X>=a)  \
  EE[X]/a&>= PP(X>=a) quad bii quad boxed(P(X>=a) <= EE[X]/a)
  $
  #QED
]
#pagebreak()

#definition(title: "Definition: Chebyshev's inequality")[
  Let $X$ be a non negative random variable, then.
  $ P(mid X-mu mid >= k std) <= 1/k^2. $
]



#proof(title: "Proof: Chebyshev’s inequality")[
  Let $mu = EE[X]$ and $ std^2 = Var[X].$ For $k >0$, then 
  $ P(mid X-mu mid >= k std) <= 1/k^2. $

  Notice that
  $abs(X-mu) >= k std bii (X-mu)^2 >= k^2 std^2$

  Then by setting $Y = (X-mu)^2$ we can apply markov's inequality.
  $ PP(abs(X-mu)>= k std) &= PP((X-mu)^2 >= k^2 std^2)\
 &<= EE[(X-mu)^2]/(k^2 var) #comment[applying Markov]  \
  &= var/(k^2 var)\
  &= boxed(1/k^2) $
  Thus Chebyshev’s inequality is simply Markov’s inequality applied to the squared deviation
from the mean. This essentially mean: The probability that $X$ is more than $k$ standard deviations away from the mean is at most $1/k^2$
]
#pagebreak()


= Exercises
#question(title: "2.7")[
For any random variables X, Y with finite expectations and constants $a$, $b$, show that:
$ EE[a X + b Y] = a EE[X] + b EE[Y] $
Importantly, X and Y do _not_ need to be independent.
]
#answer[
Given that $ EE[a X + b Y] = sum_x sum_y (a x + b y) thin p_(X Y) (x, y) $

Then 
$ sum_x sum_y (a x + b y) thin p_(X Y) (x, y) &= a sum_x x sum_y p_(X Y) (x, y) + b sum_y y sum_x p_(X Y) (x, y) \
&= a sum_x x PP(X=x) + b sum_y y PP(Y=y) \
&= a EE[X] + b EE[Y]
$
]

#question(title: "2.8")[
Suppose there are $n$ students. Each student independently answers a given question correctly with probability $p$.
\
\
+ What is the expected number of students who answer the question correctly?
+ A teaching assistant checks the answers one by one, stopping as soon as a correct answer is found, or after all n answers have been checked. What is the expected number of answers that the teaching assistant checks?
+ Now suppose the instructor randomly chooses between two possible questions:
  - an _easy_ question, chosen with probability $q$, for which each student answers correctly with probability $p_E$;
  - a _hard_ question, chosen with probability $1−q$, for which each student answers correctly with probability $p_H$.

  
  Let $X$ denote the total number of students who answer correctly. Compute $EE[X]$ using conditional expectations and the law of total expectation.
]
#pagebreak()

#answer[
  + $X_i= cases(1 quad "if student "i" answers correct", 0 quad "otherwise")$
  
    Given that $T = sum_(i = 0) ^n x_i$
    
    Then $EE[T] = EE[sum_(i = 0) ^n x_i] = sum_(i = 0) ^n EE[x_i] = sum_(i = 0) ^n p = n p$
    \
    \
  + $X_i= cases(1 quad "if student "i" is checked", 0 quad "otherwise")$

    Then $P(x_i = 1) = (1-p)^(i-1)$

    $EE[T] = sum_(i=0)^n EE[x_i]$

\
  + $X_i= cases(1 quad "if student "i" is correct", 0 quad "otherwise")$

    Then $EE[x_i] = EE[EE[X|Q]] = EE[x_i mid E]p_E + EE[x_i mid H]p_H = p_E q + p_H (1-q)$

    $EE[T] = sum_(i=0)^n EE[x_i] = n(p_E q + p_H (1-q))$


]


#question(title: "2.9")[
Let $X$ be uniform on ${−1, 0, 1}$ and $Y = X^2$. Verify directly from the joint distribution that $X$ and $Y$ are not independent, but are uncorrelated.
]
#answer[
  Firstly, independence means that for $p_(X Y) (x,y)=p_X (x)p_Y (y)$

  Given that $(x,y) = (0,1)$ that means that

  $ p_(X Y)(0, 1) &= 0 \
    p_X (0) thin p_Y (1) &= 1/3 dot 2/3 = 2/9 $

  so $p_(X Y) (x,y) != p_X (x)p_Y (y)$ and $X,Y$ must be dependent.

For uncorrelation, the joint pmf is nonzero only at the three pairs
$(-1,1), (0,0), (1,1)$, each with mass $1/3$. So
$ EE[X Y] &= sum_x sum_y x y thin p_(X Y) (x,y)
    = (-1)(1) 1/3 + (0)(0) 1/3 + (1)(1) 1/3 = 0 \
  EE[X] &= sum_x x thin p_X (x) = (-1) 1/3 + 0 dot 1/3 + 1 dot 1/3 = 0 $
Therefore
$ Cov(X, Y) = EE[X Y] - EE[X] EE[Y] = 0 - 0 dot EE[Y] = 0 $
Thus $X,Y$ are uncorrelated (despite being dependent).

]

#question(title: "2.10")[
A disease affects $1%$ of a population. A test is positive with probability 0.99 when the disease is present and with probability $0.05$ when it is absent. Compute the probability that a person has the disease given a positive test.
]
#answer[
Given that
- $P(D) = 1/100$
- $P(+ mid D) = 99/100$
- $P(+ mid not D) = 5/100$

Then $ P(D mid +) = (P(+ mid D)P(D))/P(+) = (P(+ mid D)P(D))/(P(+ mid D)P(D) + P(+ mid not D)P(not D) + ) $ 
]

#question(title: "2.11")[
Let $X tilde cal(N)(0, 1)$ and define $Y = 2X + 3$. What are $EE[Y]$ and Var(Y )? What is the distribution of $Y$ ?
]
#answer[
Given that
- $X tilde normal(0,1)$
- $EE[X] = 0$, $Var[X]=1 $

Then $EE[Y] = 2 dot EE[X] + EE[3] = 0+3$,

And $Var[Y] = Var[2X] +Var[3] =  2^2 Var[X] + 0 = 4 $
]