#import "@local/tempst:0.1.0": *

#show: assignment.with(
  title: "Problem set 3 - written part",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm", "Johannes Rothe", "Shuagib Ibrahim", "Anne Sofie Høj", "Daniel Nissen"),
  date: "March, 2026",
  outline-depth: 1
)

= Exercise 1.8
Here is a set $Gam$:

$ Gam = {all(a,b), all(c,d), all(a,c), all(a,e), all(c,e)} $

Then $Gam ent.not all(d,b)$. The point of this problem is to give two models of Γ where All d are b is false.
+ Find the canonical model of $Gam$, and check that $all(d,b)$ is false in that model.
+ Find a model $model$ with just one element such that $model ent Gam$ but $model ent.not all(d,b)$ [Hint: You can do this by modifying the model in Exercise $1.8$. That is, you use a model $model$ with $M = {*}$, and with the interpretation function given by something like $(1.8)$. The only difference is that we don't want $y$ on the right, we want $dots$. You can also get a one-point model this by using Exercise $1.9$ just below. On the other hand, some people might find Exercise 1.9 easier to think about after working on this exercise.]

== Solution
1. Let $M ={a,b,c,d,e}$ and $Gam = {all(a,b), all(c,d), all(a,c), all(a,e), all(c,e)}$
  
  Then $model = (M,[||]:M->cal(P)(M))$ becomes the canonical model, because the universe is the same as the signature, and it only satisfies the sentences in $Gam$.


  $ [|a|] &= {a} \   
   [|b|] &= {a,b} \
   [|c|] &= {a,c} \
  [|d|] &= {a,c,d} \
   [|e|] &= {a,c,e} $

The different sentences that can be stated from this canonical model are:
  \
  $ model ent all(a,b) quad qquad model ent all(c,d) quad model ent all(a,c) quad $
  
  $ model ent all(a,e) quad qquad model ent all(c,e) quad model ent.not all(d,b) quad $

  This model thus satisfies all sentences in $Gam$, and doesn't satisfy $all(d,b)$.




  
#pagebreak()

2.  Let's assume that M is still the signature ${a,b,c,d,e}$ and $Gam$ still holds. Then we can create a $model$ that only contains one element $M = {1}$ and that $[|d|] = {1}$ and every other nouns interpretation is simply the empty set. 

  $ [|a|] &= emptyset \     
   [|b|] &= emptyset  \   
   [|c|] &= emptyset  \  
   [|d|] &= {1}       \     
   [|e|] &= emptyset     $

From this $model$ we can see that $model ent Gam$: 

$
model ent all(a,b) \
model ent all(c,d) \
model ent all(a,c) \
model ent all(a,e) \
model ent all(c,e)
$


But since the empty set is a subset of every set, and not reverse, the model $model$ doesn't satisfy the property: $ model ent.not all(d,b) $
