
#let title = "Lecture 4: Local Descent"
#let author = "Simon Holm"
#let date = "19. februar 2026"
#import "../../../../../../temp.typ": *

 #sdu-title(
   title: title,
   author: author,
   date: date
 )

#pagebreak()

// Your content starts here


= Zoutendijk
When the algorithm is good enough for $theta$ to not be $0$, then $ cos^2 theta norm(nabla f_k)^2 < oo $ can never be 0 unless $nabla f_k = 0$ (e.g. stationary point)

= Rate of Convergence

Assume that ${x_k}$ is a sequence in $RR^n$ that converges to $x^*$ (a solution)
$ norm(x_(k+1)-x^*)/norm(x_k -x^*) <=r quad "for all "$k$" sufficiently large" $
The convergence is said to be Q-linear (quotient-linear) if there is a constant $r in (0, 1)$ such that

That means that the the distance to the solution $x^*$ decreases at each iteration by at least a constant factor bounded by $<1$

== Q-superlinear
The convergence is said to be Q-superlinear if
$ lim(k->oo) norm(x_(k+1)-x^*)/norm(x_k -x^*) =0 $

Superlinear convergence (quadratic, cubic, quartic, etc) is regarded as fast and desirable, while
sublinear convergence is usually impractical.


== R-linear 
A slightly weaker form of convergence: 
overall rate of decrease in the error

We say that convergence is R-linear (root-linear) if there is a sequence of nonnegative scalars ${v_k}$ such that



