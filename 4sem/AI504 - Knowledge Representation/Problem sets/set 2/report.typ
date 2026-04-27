#import "temp/temp.typ": *


#assignment(
  title: "Problem set 2",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm", "Johannes Rothe", "Shuagib Ibrahim", "Anne Sofie Høj", "Daniel Egedal Nissen"),
  date: "March, 2026",
  outline-depth: 1
)

= Problem 1
Work over the set ${a, b, c, d}$ of nouns. Consider the set of sentences

$ Gamma = {"All" a "are" b, "All" a "are" c, "All" b "are" d, "All" c "are" d}. $

Write down two different proof trees verifying $Gamma prov$ All $a$ are $d$. Write your proof trees carefully and as legibly as possible.

== Solution

Here is the $1^"st"$ solution

#figure(
  grid(
    columns: 2,
    align: horizon, 
    gutter: 1mm, 
    ptree(
      "all a are d",
      r($$, "all a are b "),
      r($$, "all b are d" )
    ),
    [BARBARA]
  ),
   caption: [Proof tree 1]   
)<proof-tree-1>


The Tree $T$ on @proof-tree-1 is composed of two different  trees  $cal(T)_0$ (ie the left side of the T) and $cal(T)_1$ (ie the right side of T). Using the BARBARA rule we show that $T$ is a  proof tree of $Gam$, this implies that $Gam prov "All a are d"$.

Here is the $2^"nd"$ solution


#figure(
  grid(
    columns: 2,
    align: horizon, 
    gutter: 1mm,    
    ptree(
      "all a are d",
      r($$, "all a are c"),
      r($$, "all c are d")
    ),
    [BARBARA]      
  ),
  caption: [Proof tree 2]
)<proof-tree-2>

The Tree T on @proof-tree-2 is composed of two different  trees  $cal(T)_0^*$ (the left side of the T) and $cal(T)_1^*$ (the right side of T). Using the BARBARA rule we show that T is a  proof tree of $Gam$, this implies that $Gam prov "All a are d"$.




#pagebreak()

= Problem 2

Work over the set ${a, b, c, d}$ of nouns. In this question I want you to find *two different models of the same size*. Each of your models should satisfy all of the following sentences.

$ "All" a "are" b, "All" a "are" c, "All" b "are" d, "All" c "are" d$ and falsify both of the following sentences.
$ "All" b "are" c, "All" c "are" b.$

_Note_. We have to be a little careful in saying what it means for two models to be different. But basically
if you label the elements of your model $1, 2, dots, n$ you should not be able to relabel (i.e., permute) the elements of one of your models to get the other one.

== Solution

Let $model$ be a model over the set of nouns $M = {a,b,c,d}$.

=== Model 1 ($model_1$)

#align($
[|a|] &= emptyset
\
[|b|] &= {b}
\
[|c|] &= {c}
\
[|d|] &= {b,c,d}

$)
This model would then *satisfy* the following:

$
model_1 ent "all a are b"
$

$
model_1 ent "all a are c"
$

$
model_1 ent "all b are c"
$

$
model_1 ent "all c are d"
$

And *falsify:*


$
model_1 ent.not "all b are c"
$

$
model_1 ent.not "all c are b"
$



#pagebreak()

=== Model 2 
Let $model$ be a model where $M = {a,b,c,d}.$


#align($
[|a|] &= {a}
\
[|b|] &= {a,b}
\
[|c|] &= {a,c}
\
[|d|] &= {a,b,c,d}

$)

This model would then *satisfy* the following:

$ model_2 ent "All a are b" $
$ model_2 ent "All a are c" $
$ model_2 ent "All b are d" $
$ model_2 ent "All c are d" $

And *falsify*:
$ model_2 ent.not "All b are c" $
$ model_2 ent.not "All c are b" $


#pagebreak()



= Problem 3
The _dual_ of a sentence is obtained by switching it's two nouns. For example the dual of all $p$ are $q$ is all $q$ are $p$. We denote the dual of a sentence $phi$ by $phi^dag$. If $Gamma$ is a set of sentences, by $Gamma^dag$ we mean the
obvious thing, i.e., “take the dual of each sentence in $Gamma$.”

Suppose $Gamma prov phi$. Prove that $Gamma^dag prov phi^dag$ by induction on proof trees. Be very careful and deliberate, so that
I know you understand what you're doing.


== Solution
#pseudo[
  *#u("Proof by induction")*

  *Goal:*  if $T$ is a proof of $phi$  from $Gam$ then, there exist a proof such that $Gam^dag prov phi^dag$.
  - *#u("Base case")* (T is a single leaf)
  + Suppose $T$ is a proof of $phi$ from $Gamma$. Since $T$ is a single leaf, the tree is just $phi$, and since all leaves are either premises and come from $Gamma$, or is on the form "all $p$ are $p$", which is an axiom, it follows that $phi in Gamma$, or $phi=phi^dag$ and thus is trivial.
  + For the dual $phi^dagger$ from $phi$, if $phi in Gamma$ and $Gamma^dagger$ contains all the duals of $Gamma$, then $phi^dagger in Gamma^dagger$. 
  
  - *#u("Inductive hypothesis")*
  + For all proofs of $T$, $Gam$, and $phi$, if $T$  is a proof of $phi$ from $Gam$,then there exist a proof $T$, such that $ Gam^dag prov phi^dag$
  - *#u("Inductive step")*
  + Suppose $T$ is not a leaf, decompose $T$ as follows.
  + Let's assume that $cal(T)$ is not a single leaf $phi$ and $phi^+$, but $cal(T)$ is composed of two trees, left and right trees labeled $cal(T_0) $ and $cal(T_1)$, respectively, and it's dual:
  + #figure(
      grid(
      columns: 2,
      align: horizon, 
      gutter: 1mm, 
      ptree(
        "all p are q",
        r($cal(T)_0$, "all p are x"),
        r($cal(T)_1$, "all x are q")
      ),
      [BARBARA]
    ),
    caption: [Proof tree 3]
  )<proof-tree-3>
  + The proof tree can then be restructured, so it satisfies the $phi^dag$ dual, which will then satisfy

  + #figure(
      grid(
      columns: 2,
      align: horizon, 
      gutter: 1mm, 
      ptree(
        "all q are p",
        r($cal(T)^dag_1$, "all q are x"),
        r($cal(T)^dag_0$, "all x are p")
        ),
        [BARBARA]
      ),
        caption: [$T^dag$]
      )<proof-tree-3>


  + By the basis of the inductive hypothesis applied to $cal(T_0) $ and $cal(T_1)$, it follows that $Gam^dag prov cal(T_0)^dag "and" cal(T_1)^dag$, and by using the BARBARA rule, it follows that $Gam^dag prov phi^dag$.
 ]

#pagebreak()

= Problem 4
Continuing the previous problem, there is also a notion of a dual model. So the idea is that given a model $cal(M)$, we should be able to define its “dual” $cal(M)^dag$, which has the property that $cal(M)^dag ent phi^dag$ if and only if $cal(M) ent phi$, for each sentence $phi$. Define the dual and prove that it enjoys this property
(Hint. Start out with some explicit small models, see if you can find their duals by trial and error, and
then see what the common pattern is.)

== Solution

// Then we wish to define $model^dag ent phi^dag$ iff $model ent phi$

// $model ent phi$ means that for any $Gam$ containing a sentence such as $"All" a "are" b$ (denoted as $(a,b)$. Then
// $ [|a|]_model psubset [|b|]_model $

// The dual $ Gam^dag$ is defined as: $ Gam^dag = {phi^dag = (y,x) | phi = (x,y) in Gam}. $



// $ [|x|]^dag = {y | x in [|y|]} $


// That means that for a dual model to $model^dag ent phi^dag$ then
// $ [|b|]_(model^dag) psubset [|a|]_(model^dag) $













// === maybe

// Suppose that $Gam prov phi$, then $underbrace(Gam ent phi => Gam prov phi,    "Completeness theorem")$

// Lets define the dual set of sentences 
// $ Gam^dag$ is defined as: $ Gam^dag = {phi^dag = (y,x) | phi = (x,y) in Gam}. $

// Such that $ forall phi in Gam | Gam prov phi, exists! phi^dag in Gam^dag |    Gam^dag prov phi^dag $

// then by the soundness theorem
// $ Gam^dag prov phi^dag => Gam^dag ent phi^dag. $
// Therefore,
// $ model^dag ent phi^dag $
// === maybe \#2
First lets define $model$ and $model^dag$

$ model = (M, [|  |] , M -> cal(P)(M)) $ 
$ model^dag = (M, [|  |]^dag, M -> cal(P)(M)) $ 


By observing some examples we see that the dual interpret can be described as $ [|p|]^dag =  f([|p|]) = M without [|p|] $ 

in a case of $[|p|] psubset [|q|]$ that means that $f([|q|]) psubset f([|p|])$  (which will be proved below).

We can see this in this example on @venn-diag-1 where $[|p|]={1} psubset [|q|]={1,2} psubset M$

#figure(
  grid(
    columns: 3,
    column-gutter: 1em,
    align: center + horizon,
    venn(
  scale: 1cm,
  domain: $M$,
  universe-fill: red.transparentize(75%),  // color universe, or none
  ($f([|q|])$, ("1","2"),white),                         // auto → palette color
)
,
    text(size: 2em)[$subset.eq$],
    venn(
  scale: 1cm,
  domain: $M$,
  universe-fill: red.transparentize(75%),  // color universe, or none

  ($f([|p|])$, ("1"), white),
  // explicitly unfilled
)
,
  ),
  caption: [$[|p|] psubset [|q|] psubset M => f([|q|]) psubset f([|p|]) = M without [|q|] psubset M without [|p|]$]
)<venn-diag-1>








Now we can use the interpretations $[|p|]$ and $[|q|]$

$ "If" [|p|] psubset [|q|] "that means that" f([|q|]) psubset f([|p|]) $
$ "If" [|p|] psubset [|q|] "that means that" [|q|]^dag psubset [|p|]^dag $

This is the same as 

$ [|q|]^dag psubset [|p|]^dag "iff" [|p|] psubset [|q|]  $

Which is

$ model^dag ent phi^dag "iff" model ent phi $

#pagebreak()

* PROOF*

We want to prove that:
$
\ x subset.eq y <==> M \\ y subset.eq M \\ x
$

We begin first with proving the one way implication:
$
x subset.eq y &==> M \\ y subset.eq M \\ x
$

#align(center,"Where:")

$ \ x subset.eq y &equiv forall z in x : z in y $

#align(center,"And:") 

$ M \\ y equiv forall z : z in M \\ y <==> z in M "and" z in.not y $

$ M \\ x equiv forall z : z in M \\ x <==> z in M "and" z in.not x $


We start by assuming $x subset.eq y $. Let $z in M \\ y$. We have to prove:
$ z in M \\ x $

We then need to figure out if it is true that:
$ z in.not x $
We can assume that $z in x$ and attempt to get to a contradiction. We know that if $z in x$ then $z in y$ from earlier, since $x$ is a subset of $y$, and therefore $z in y$ would need to be true for the $z in x$ to hold, but we know $z in M \\ y <==> z in M "and" z in.not y$. Where it is stated that $z$ is not in $y$, that means $z$ cannot be in $x$ either (because $x subset.eq y$), meaning that:
$ z in.not x $

This completes the one way implication. Then we need to prove the other direction for the entire bi-implication to hold. We attempt to prove:
$
M \\ y subset.eq M \\ x &==> x subset.eq y 
$
We start by assuming $M \\ y subset.eq M \\ x $. To prove that $x$ is a subset of $y$, assume $z in x "but" z in.not y$. If $z in x$, $z in M$. This ensures that $z in M \\ y$. That further shows that $z in M\\x$. If $z in M \\ x$, then $z in.not x$. This shows a contradiction, therefore we can conclude that the implication holds.      


This means that the bi-implication holds.

#pagebreak()

= Problem 5

The point of this exercise is to show you that models can be built out of anything, as long as they have the correct type. Work over the set $N = {0, 1, 2, 3}$ of nouns. Now this set of nouns makes for
weird-looking sentences like "all $3$ are $1$". But once we construct an $N$-model, it makes perfect sense to
say that such sentences are satisfied or falsified.

Define the $N$-model $cal(R)$ as follows. Its domain is $RR$, the set of real numbers, and for each $b in N$, define

$ [|b|] = {x in RR: x^b-x < 0} $

List all of the $N$-sentences that $cal(R)$ satisfies, and argue that your answer is correct. (For the purposes of this exercise, assume $0^0 = 1$.)

== Solution

To show this, we can construct all the nouns $[|b|]$ from $N$ that follows $cal(R)$:

#align($
b=0 &==> {x in RR: x^0 -x < 0} &==> {x in RR: 1 -x < 0} &==> {x in RR: x > 1}
\
b=1 &==> {x in RR: x^1 -x < 0} &==> {x in RR: x -x < 0} &==> {x in RR: x < x}
\
b=2 &==> {x in RR: x^2 - x < 0}
\
b=3 &==> {x in RR: x^3 - x < 0}
$)

Then, we can for each of the constructed nouns attempt to find the places where the values are true according to the model $cal(R)$.

\ For $b=0$, this one becomes obvious due to it's simple nature. We know that all values have to be larger than 1, so we simply get:

$ b=0 ==> {x in RR: x > 1} &==> (1,infinity) $
$ [|0|] = (1,infinity) $

\ $b=1$, there is an inequality that can never be satisfied, so no $x$ will be allowed in the set, making it the empty set $emptyset$:

$ b=1 &==> {x in RR: x < x} &==> (emptyset) $
$ [|1|] = emptyset $

\ Then for $b=2$, the value must be calculated, as the solution is not as intuitive as the previous 2, so we try to find every value of $x$, where $y=0$, since this is the cutoff according to the inequality:
\ $ b=2 &==> {x in RR: x^2 - x < 0} $

Making the equation:
$ x^2 - x = 0 $

We use the quadratic formula $x=(-b plus.minus sqrt(b^2 - 4a c))/(2a) $ to find the solutions, and calculate the discriminant $d=b^2-4a c $ shows, which shows there will be 2 solutions, since it is positive.
$ d = (-1)^2-4 dot 1 dot 0 = 1 $

The quadratic formula will then take both a positive and negative form to find the $x$'s:

$ x_1=(-(-1) + sqrt((-1)^2 - 4 dot 1 dot 0))/(2 dot 1) = (1 + sqrt(1))/(2) = 2/2 = 1 $
$ x_2=(-(-1) - sqrt((-1)^2 - 4 dot 1 dot 0))/(2 dot 1) = (1 - sqrt(1))/(2) = 0/2 = 0 $

Now we just need to know where $y<0$ so we can find the interval. A quick look at the formula would show that it is convex, so we calculate $x=0.5$ to make sure that it is between the two points, that are negative.

$ x^2-x -> 0.5^2-0.5 = -0.25 $

So the interval becomes:

$ [|2|]=(0,1) $

\ $b=3$, once again the equation can be solved, for whenever it is below 0, since this value is given by the inequality. Therefore, we attempt to find all $x$ where $y=0$.

$ b=2 &==> {x in RR: x^3 - x < 0} $

We attempt using the zero product property to find the correct $x$'s. We start by factoring out the most common factor $x$, so we get:

$ x^3 -x = x dot (x^2-1) $

Then we can apply the difference of squares rule, since this can be written as:

$ x dot (x^2-1) = x dot (x^2-1^2) = x dot (x-1) dot (x+1) $

Then we can apply the zero product property and solve each product by themselves, as they each have their own $x$:

$ x=0 $
$ x-1=0 &==> x = 1 $
$ x+1=0 &==> x = -1 $

So we now know all the places where $y=0$, the missing step is to find intervals where $y<0$:

$ x=-2 : x^3 -x -> -2^3-(-2) = -6 $
$ x=-0.5 : x^3 -x -> -0.5^3-(-0.5) = 0.375 $
$ x=0.5 : x^3 -x -> 0.5^3-0.5 = -0.375 $
$ x=2 : x^3 -x -> 2^3-2 = 6 $

We can then observe that before and until $x=-1$ the values are in the negative, changing to positive and then back to negative at $x=0$, and then back to positive at $x=1$. Making the interval where the equation is fulfilled according to the inequality:
$ [|3|]=(-infinity,-1) union (0,1) $
\
Thereby, we know that all the correct intervals for the values fall at:

#align($
[|0|] &= (1,infinity) 
\
[|1|] &= (emptyset) 
\
[|2|]&=(0,1) 
\
[|3|]&=(-infinity,-1) union (0,1) 
$)
\
\
\
Therefore, the list of the N-sentences that $cal(R)$ satisfies:
\
\

#align($
cal(R) ent &"All of" 2 "are" 3 => [|2|] psubset [|3|]
\
cal(R) ent &"All of" 1 "are" 0 => [|1|] psubset [|0|]
\
cal(R) ent &"All of" 1 "are" 2 => [|1|] psubset [|2|]
\
cal(R) ent &"All of" 1 "are" 3 => [|1|] psubset [|3|]
$)



