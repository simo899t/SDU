#import "../../../../temp/temp.typ": *

#show: exercise.with(
  title: "AI 504 activity: ",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm"),
  date: "May, 2026",
  outline-depth: 1
)

#set enum(numbering: "1.")

= Model 1 $quad$ Love
1. Person x loves everybody.$ forall y L(x,y) $
2. Someone loves person y.$ exists x L(x,y) $
3. Everyone loves person y.$ forall x L(x,y) $
4. Everyone loves somebody (but not necessarily the same single person).$ forall x exists y L(x,y) $
5. Someone loves everybody.$ exists x forall y L(x,y) $
6. There is somebody who is universally loved.$ exists x forall y L(y,x) $
7. Everyone is loved by someone (but not necessarily the same single person).$ forall x exists y L(y,x) $
8. Everybody loves themselves.$ forall x L(x,x) $
9. Love is reciprocal. (If any person loves another, that person loves them back.)$ forall x,y L(x,y) imp L(y,x) $
10. Everyone loves someone who loves them back.$ forall x exists y L(x,y) and L(y,x) $
#pagebreak()

= Model 2 $quad$ Divisibility
11.  Define m | n using a logical formula which only uses multiplication ·, equality =, and the existential quantifier $exists$.$ exists x in NN : m dot x = n $
12.  Define “n is even” using only 2 and |.$ 2|n $
13.  Define “n is odd” using only 1, 2, |, and +.$ 2|n+1 $
14.  Which numbers n satisfy n | 0? Which numbers n satisfy 0 | n?$ NN, 0 $
15.  Which numbers n satisfy n | 1? Which numbers n satisfy 1 | n?$ 1, NN $
16.  Define C(x, y, z), “x is a common divisor of y and z.” $ x | y, and x | y $
17.  Write the divisors of 24 and 56. Which ones are common, and which is the greatest common divisor?$ {1,2,4,6,8,12,24}, {1,2,4,8,14,28,56}, {1,2,3,8} , 8 $
18.  Go through the same exercise with 30 and 48.$ {1,2,5,6,10,15,30}, {1,2,4,8,12,16,24,48}, {1,2} , 2 $
19.  What relationship do you notice that the greatest common divisor has amongst all common divisors, besides the fact that it is larger than all the rest? $ "All the common divisors is a devisor for the greatest common devisor " $
20.  Use your observations to write a formula defining “x is the greatest common divisor of y and z.” Only use ∀, |, ∧ (AND), as well as the already-defined ternary condition C. (Ternary means “has three arguments.”) $ C(x,y,z)) and forall omega in NN. (C(w,y,z) -> w|x) $
Let $C D(y,z) = {v in NN | C(v,y,z)} \ C(x,y,z)) and forall omega in C D(y,x). w|x $
#pagebreak()

= Model 1 $quad$ Eventualities

21. Let $f : RR -> RR$ be a function from the real numbers to the real numbers. Define the condition $P(f) eq.triple "\""f$ is always positive."
$ forall x in RR. f(x) > 0 $
22.  Define "$f$ is positive for all inputs greater than −100.”
$ forall x in {v in RR| v > -100}. space f(x) > 0 $
23.  Define "$f$ is positive for all inputs greater than 0.”
$ forall x in {v in RR| v > 0}. space f(x) > 0 $
24.  Define "$f$ is positive for all inputs greater than 100.”
$ forall x in {v in RR| v > 100}. f(x) > 0 $
25.  Define "$f$ is eventually positive," i.e., "there is some point past which $f$ is always positive.”
$ exists y in RR, forall x in {w in RR | w > y}. f(x) > 0 $
26.  Define " $f$ is eventually greater than 1."
$  exists y in RR, forall x in {w in RR | w > y}. f(x) > 1 $
27.  Define " $f$ is eventually greater than 1000."
$ exists y in RR, forall x in {w in RR | w > y}. f(x) > 100 $
28.  Define " $f$ eventually is greater than every number." In other words, to ensure that $f$ lies above any horizontal line, we just have to go sufficiently far to the right. For those of you who know calculus, this is the same as saying $lim_(x->oo) f(x) = oo$.
$ forall z in RR, exists y in RR, forall x in {v in RR | v > y}. space f(x) > z $

29.  The reason there are fancy words like “monotone” instead of “increasing” is that there are multiple ways of making the notion of “ $f$ is an increasing function” formal! One is indeed “ $f$ is monotone,” which is defined by $ (forall x, y)(x < y -> f (x) < f (y)). $ \ Another is $lim_(x->oo) f(x) = oo$, which we defined in the previous problem. Are these two conditions equivalent? Does either entail the other? $ abs(x) = cases(yes lim_(x->oo) f(x) = oo , no "monotone") qqquad tan^(-1)(x) = cases(no lim_(x->oo) f(x) = oo, yes "monotone") $\ So no, they are not equivalent.
#pagebreak()

30.   Let $M( f )$ be the condition "$f$ is monotone," $A( f )$ be " $f$ is antitone," and $L( f )$ be lim
$ lim_(x->oo) f(x) = oo $
Complete the analogy:
$ M : A::L:? $
and give a formal definition of the missing condition.

$ M : A::L:lim_(x->oo) f(x) = -oo $
$ forall z in RR, exists y in RR, forall x in {v in RR | v > y}. space f(x) > z $
