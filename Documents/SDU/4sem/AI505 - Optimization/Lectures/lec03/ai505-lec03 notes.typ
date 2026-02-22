
#let title = "Lecture 3: Bracketing"
#let author = "Simon Holm"
#let date = "15/02/2026"

#import "../../../../../../temp.typ": *

 #sdu-title(
   title: title,
   author: author,
   date: date
 )

#pagebreak()

// Your content starts here

= Solutions and Recognizing Them for Smooth Functions
We know that point $x^*$ is a *global* minimizer if $f(x^*) ≤ f (x) quad forall x$.

Also with if there is a neighborhood $N$ of $x^*$ such that $f(x^*) ≤ f (x) quad forall x in N$. Then $x^*$ is a *local* minimizer

= Taylor's Theorem
Suppose $f: RR^n -> RR$ is continuously differentiable and that $bold(p) in RR^n$

Then $ f(x+p) = f(x) + nabla f(x+t bold(p))^T bold(p) $ 
for $t in (0,1)$

- Here $f(x+bold(p))$ is a step in the direction $bold(p)$ scaled by $t$ and is influenced by $nf(x+t bold(p))$, which tells us the steepest increase. "If I move in this direction, how much does the function change?"

Also if $f$ is twice continuously differentiable, then

$ nabla f(x+ bold(p)) = nabla f(x) + int(0,1,nabla^2 f(x+t bold(p)) bold(p) dif t) $

We can apply the same theory for the gradient to calculate, "If I start at $x$ and move along $p$, how does the slope (gradient) change along the way, and what is the gradient at the new point $x+p$?"

We know this from the *Fundamental Theorem of Calculus:* $ g(1)-g(0) = int(0,1,g'(t)dif t) $ 
so therfore when $g(t) = nf(x+t bold(p))$

Then 
$ nf(x+ bold(p))-nf(x) = int(0,1,nabla_p nf(x+t bold(p)) dif t) = int(0,1,nabla^2 f(x+t bold(p)) bold(p) dif t) $

and that

$ f(x+bold(p)) = f(x) + nf(x)^T bold(p) + 1/2 bold(p)^T nnf(c+t bold(p)) bold(p)  $

We can show this because (again by *FTC*)
$ f(x+bold(p)) = f(x) + int(0,1,nabla f(x+s bold(p)) dif s) $

$ f(x+bold(p)) = f(x) + int(0,1,(nabla f(x) + int(0,1,nabla^2 f(x+t bold(p)) bold(p) dif t))^T bold(p) dif s) $
Then split the integral and reduce them
$ f(x+bold(p)) = f(x) + nabla f(x)^T bold(p) + 1/2 bold(p)^T nnf(c+t bold(p)) bold(p) quad c in (0,1) $

= Algorithms for Unconstrained Optimization of Smooth Functions
Two main strategies for finding local minima of smooth functions
- Line Search methods, such as gradients descent etc.
- Truest regionm

== Unimodality
There exists a point $in [a,b]$ such that:


$ f(x) "is decreasing for" x<x^* "and increasing for"x>x^*  $



= Finding an initial bracket
We know that the global minimum is $in[a,c]$ when
$ a<b<c " and "f(a)>f(b)<f(c) $

#code(
  ```py
  def bracket_min(f,x=0,s=1e-2,2=2.0)
    a, ya=x, f(x)
    b, yb=a+s, f(a+s)
    if yb > ya # if +f'(x) -> go the other way
      a, b = b, a
      ya, yb = yb, ya
      s = -s
    
    while true
      c, yc = b+s, f(b+s)
      if yc > yb
        return a < c ? (a, c) : (c, a)
      a, ya, b, yb = b, yb, c, yc
      s *= k
  ```
)
The algorithm above describes a way to Iteratively find an certainty-interval where the solution $x^*$ lies within.

Here is an example of a ```py bracket_min``` function at work
#image("/assets/image-1.png")

Note that we can then shrink the interval by making function evaluations and see if they hold the unimodality property. By this we can eliminate parts of the interval that cannot contain the minimum with guarantee.
#pagebreak()


= Fibonacci Search Algorithm
Using the Fibonacci sequence of 
$ F_0 = F_1 = 1, quad F_n = F_(n-1)+F_(n-2), quad forall n | n>=2 $

to divide the interval at each iteration "```py stepsize *= Fn```"

= Golden Section Search
Using the fixed golden ratio $phi$ 
$ lim(n->oo) F_(n+1)/F_n = lim_(n->oo) 1/(rho_n) = lim(n->oo) phi (1s^(n+1))/(1-s^n) = phi approx 1.61803 $
to divide the interval at each iteration "```py stepsize *= 1.61803```"


= Quadratic Fit Search
Iteratively fits quadratic function to three bracketing points
#image("/assets/image-2.png")

== Using linear algrebra
You assume locally that $f(x)$ is quadratic, so you fit:
$ f(x) approx b_0 + b_1x+b_2x^2 $
Then, using the 3 function values, you solve for $b_0,b_1,b_2$

#code(
  ```py
  A = np.vander(x,4)
  
  coeff = np.linalg.solve(A,y) ## Error!! Why?
  
  B = A.T @ A
  z = np.linalg.inv(B) @ A.T @ y
  
  coeff = np.linalg.lstsq(A, y)[0]
  np.allclose(z,coeff)
  
  f=np.poly1d(coeff)
  plt.plot(x, y, 'o', label='Original data', markersize=2)
  plt.plot(x, f(x), 'r', label='Fitted line')
  plt.legend()
  plt.show()
  ```
)

$ #image("/assets/image-3.png", width: 25em) $

= Lipschitz continuous
This means that there is a bound for the derivative
ex. an exponential function derivative is unbound, because it keeps 
growing forever.
A function $f$ is Lipschitz continuous on $[a, b]$ if there exists an $ell > 0$ such that:
$ |f(x)-f(y)| <= ell(|x-y|), forall x,y in [a,b] $

= Shubert-Piyavskii Method
The Shubert-Piyavskii method is guaranteed to find the global minimum of any bounded
function. Note that it requires that the function be Lipschitz continuous
#image("/assets/image-4.png")

= Bisection Method

If, we want to find a stationary point, we look for $x<-nf(x) =0$

In other words, we look for the roots of $nf(x)$ aka, where it cuts the x-axis

We can do this, by 
- setting an interval
- splitting the interval $(a+b)/2$
- chose a side that where $"sign"(f'(a))̸ = "sign"(f'(b))$
- repeat until error is acceptable. (interval is "small enough")

This is guaranteed to converge within $eps$ of $x^*$ within $log(abs(b-a)/eps)$




