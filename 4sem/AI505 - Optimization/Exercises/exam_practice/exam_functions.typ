
#import "../../../../temp/temp.typ": *

#show: note.with(
  title: "Exam_prep",
  course: "AI505 - Optimization",
  author: "Simon Holm",
  date: "June, 2026",
)
#set heading(numbering: none)

= A simple minimization problem

Defining a minimization problem such that $x$ lies within a feasible set of solutions
$ min_x f(x) \ st x in cal(X) $

#example([
  $ min_(x_1,x_2) f(x_1,x_2) \ 
  st #align($x_1 &>= 0 \
             x_2 &>= 0 \
             x_1 + x_2 &<= 1 \ $) $
  #figure(
    image("1/assets/image-1.png")
  )
  ])

#pagebreak()
= Taylor expansion
Taylor expansion

Let $a$ be a fixed point. The Taylor expansion approximates $f(x)$ near $a$ using only information about $f$ at $a$.

$ f(x) = f(a) + tran(nf(a)) (x-a) + 1/2 tran((x-a)) nnf(a) (x-a) $

= Convexity

$ f: RR^n -> RR \ f(alpha x + (1-alpha) y) <= alpha f(x) + (1-alpha) f(y)  $

#figure(
  image("1/assets/image-2.png")
)

For a convex hull:
#figure(
  image("1/assets/image-3.png")
)
$ "conv"(X) = {sum_i lambda_i x_i | x_i in X, quad  lambda_i >= 0, quad  sum_i lambda_i = 1} $

= Directional derivative

$ nabla_s f(s) = pv(f(x),x_1) s_1 + pv(f(x),x_2) s_2 + dots +  pv(f(x),x_n) s_n $
  