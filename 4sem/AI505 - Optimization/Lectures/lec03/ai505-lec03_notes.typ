#import "../../../../temp/temp.typ": *

#let title = "Lecture 3: Bracketing"
#let author = "Simon Holm"
#let date = "February - 2026"

#show: note.with(
   title: title,
   author: author,
   date: date
 )


// Your content starts here

= Solutions and recognizing them for smooth functions
#definition(title: [Global minimizer], [A point $x^*$ is a *global* minimizer if $ f(x^*) ≤ f (x) quad forall x. $])

#definition(title: [Local minimizer], [A point is a *local minimizer* if there is a neighborhood $N$ of $x^*$ such that $ f(x^*) ≤ f (x) quad forall x in N. $])


= Taylor's theorem
#theorem(title: [Taylor's theorem], [
  Suppose $f: RR^n -> RR$ is continuously differentiable and that $bold(p) in RR^n$

Then $ f(x+p) = f(x) + nabla f(x+t bold(p))^T bold(p) $ 
for $t in (0,1)$
])


Above, $f(x+bold(p))$ is a step in the direction $bold(p)$ scaled by $t$ and is influenced by $nf(x+t bold(p))$, which tells us the steepest increase. "If I move in this direction, how much does the function change?"

Also if $f$ is twice continuously differentiable, then

$ nabla f(x+ bold(p)) = nabla f(x) + int(0,1,nabla^2 f(x+t bold(p)) bold(p) dif t) $

We can apply the same theory for the gradient to calculate, "If I start at $x$ and move along $p$, how does the slope (gradient) change along the way, and what is the gradient at the new point $x+p$?"
#pagebreak()

We know this from the *Fundamental Theorem of Calculus:* 
#theorem(title: [Fundamental theorem of Calculus],[
  $ "FTC1": quad ddx int(a,x,f(x)) dx = f(x) $
  $ "FTC2": quad int(a,b,f(x)) dx = F(b)-F(a) $
where $F(x) = integral f(x) dx $
])

$ g(1)-g(0) = int(0,1,g'(t)dif t) $ 

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
#pagebreak()

= Algorithms for Unconstrained Optimization of Smooth Functions
Two main strategies for finding local minima of smooth functions
- Line Search methods, such as gradients descent etc.
- Truest regionm

== Unmorality
There exists a point $in [a,b]$ such that:


$ f(x) "is decreasing for" x<x^* "and increasing for"x>x^*  $



= Finding an initial bracket
We know that the global minimum is $in[a,c]$ when
$ a<b<c " and "f(a)>f(b)<f(c) $


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
The algorithm above describes a way to Iteratively find an certainty-interval where the solution $x^*$ lies within.

#figure(
  image("assets/image-1.png"),
  caption: [example of a ```py bracket_min``` function at work]
)

Note that we can then shrink the interval by making function evaluations and see if they hold the unimodality property. By this we can eliminate parts of the interval that cannot contain the minimum with guarantee.
#pagebreak()

= Shrinking the interval
When shrinking an interval $[a,c]$ we can assume a middlepoint $b$ and then place some evaluation point $x$ and compare $x$ and $b$

- *if* $f(x) < f(b)$ the minimum is between $b$ and the far end, so discard the near end
- *if* $f(x) > f(b)$ the minimum is between $a$ and $x$, so discard the far end

== Fibonacci Search Algorithm

$ F_0 = 0,quad F_1 = F_2 = 1, quad F_n = F_(n-1)+F_(n-2) $

the placement of $x$ is determined by Fibonacci numbers, and you decide upfront how many iterations you want. It's optimal for a fixed number of evaluations.

== Golden Section Search
$ limm(n->oo) (F_(n+1))/F_n = limm(n->oo) 1/rho_n = limm(n->oo) phi (1-s^(n+1))/(1-s^n) $

places $x$ at the golden ratio $phi = 0.618$ of the interval. Each step shrinks the interval by the same factor $phi$. It's essentially the limit of Fibonacci search as the number of iterations goes to infinity.



= Quadratic Fit Search
One can also iteratively fit quadratic function to three bracketing points
#figure(
  image("assets/image-2.png"),
  caption: [fitting of a quadratic approximation $h(x)$ to a function $f(x)$]
)
#pagebreak()

== Linear algebra application
You assume locally that $f(x)$ is quadratic, so you fit:
$ f(x) approx b_0 + b_1x+b_2x^2 $
Then, using the 3 function values, you solve for $b_0,b_1,b_2$

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

#figure(
  image("assets/image-3.png", width: 25em),
  caption: [Example from Marco's slides]
)
#pagebreak()

= Lipschitz continuous <lipcont>

A function $f$ is Lipschitz continuous on $[a, b]$ if there exists constant $ell > 0$ such that:
$ abs(f(x)-f(y)) <= ell dot |x-y| , forall x,y in [a,b] $
This means that there is a bound for the derivative $nf$


#figure(
  image("assets/image-5.png", width: 15em),
  caption: [*Intuition of Lipschitz continuity:* lets bound the slope of the secant between two points $x,y$ by a constant $ell$ ]
)

- *Example: * an exponential function derivative is unbound, because it keeps growing forever.

= Shubert-Piyavskii Method
The Shubert-Piyavskii method is a optimization algorithm method for finding the global minimum of a univariate function $f: [a,b] -> RR$
function. Note that it requires the function $f$ to be Lipschitz continuous. [@lipcont]
#figure(
  image("assets/image-4.png", width: 20em),
  caption: [Example of method]
)
No need for further explanation. Intuition is just that it guarantees a lowerbound for the optimal solution.

#pagebreak()

= Bisection Method
If, we want to find a stationary point, we look for $x<-nf(x) =0$

In other words, we look for the roots of $nf(x)$ aka, where it cuts the x-axis

We can do this, by 
- setting an interval
- splitting the interval $(a+b)/2$
- chose a side that where $"sign"(f'(a)) != "sign"(f'(b))$
- repeat until error is acceptable. (interval is "small enough")

#figure(
  image("assets/image-6.png"),
  caption: [example of the bisection method for closing an interval]
)

This is guaranteed to converge within $eps$ of $x^*$ within $log(abs(b-a)/eps)$




