#import "../../../temp/temp.typ": *

#show: exam.with(
  title:         "Written Exam",
  // subtitle:      "Re-exam",
  course:        "AI504: Knowledge Representation",
  author:        "Simon Holm",
  date:          "June 1st 2026",
  student-id: "sihol24",
  username:      "",
  student-number: "sihol24",                 
  duration:      "3 hours",
  allowed-aids:  "No aids",
  university:    "University of Southern Denmark",
  outline:       true,
)


#set heading(numbering: none)

= Multiple choice questions
== 1
+ $no$
+ $yes$
+ $no$
+ $no$

== 2
+ $yes$
+ $no$
+ $no$
+ $no$

== 3
+ $no$
+ $no$
+ $no$
+ $yes$





== 4
+ $no$
+ $yes$
+ $no$
+ $no$

== 5
+ $yes$
+ $no$
+ $no$
+ $no$


== 6
+ $no$
+ $yes$
+ $no$
+ $no$

== 7
+ $no$
+ $no$
+ $no$
+ $yes$



== 8
+ $no$
+ $no$
+ $yes$
+ $no$

== 9
+ $no$
+ $yes$
+ $no$
+ $no$

== 10
+ $no$
+ $no$
+ $no$
+ $yes$


== 11
+ $yes$
+ $no$
+ $no$
+ $no$



== 12
+ $no$
+ $no$
+ $yes$
+ $no$


#pagebreak()
= Open questions
== 1 
Given that
$ (e bi not b) and (b bi not d) and (d bi not a) and (a bi not c) and (c bi not e) $
This means that $e bi not e$ which mean that there is *0* solutions

== 2

Lets prove that $ (forall x in ip(c))(forall y in ip(term(r,d))) (x,y) in ip(r) $

Because of the transitive relation where $forall x,y,z in M$ if $(x,y) in ip(r) and (y,z) in ip(r)$ then $(x,z) in ip(r)$

This mean that $ip(d) subset ip(term(r,d))$. Therfore

$ (forall x in ip(c))(forall y in ip(term(r,d))) (x,y) in ip(r) => (forall x in ip(c))(forall y in ip(d)) (x,y) in ip(r) $
i.e
$ sent(c,(term(r,(term(r,d))))) => sent(c,(term(r,d))) $
#QED

Now lets construct a model $cal(N)$ which does _not_ satisfy this.

Let $cal(N)$ have the domain $M = {1,2}$
Then $ ip(c) = {1}, quad ip(c) = {2}, quad "and" quad ip(r) = {(1,2)} $
This means that 

$ ip(term(r,d)) = {1} $
$ ip(term(r,(term(r,d)))) = emptyset $

This proves that $ cal(N) ent sent(c,(term(r,(term(r,d))))) $
And also that 
$ cal(N) ent.not sent(c,(term(r,d)))) $

#QED

 
== 3
The property is:

Any sentence $phi$ which is not an axiom, must be either be a single letter, or a sentence on the form: $sent((term(r,psi)),rho)$ where $psi$ and $rho$ is arbitrary sentences with the same property.

- $sent((term(r,c)),d)$ must have this term by definition
- $sent(c,(term(r,d)))$ must _not_ have this term by definition
- Since no axioms are allowed under this property, no sentences on the form $sent(x,x)$ has this property.
- Lets try and prove this last one using induction
#pagebreak()
#pseudo[
  *Proof by induction* 
  - *Goal:* if $Gam prov phi$ and $phi$ has the property, then there must exists a sentence in $Gam$ which has the property
  + *$underline("Base case")$*
    + Let $Gam = {phi}$,
    + Then since $phi$ has this property, and since $phi in Gam$, there is an element in $phi$ which has this property.
  + *$underline("Inductive hypothesis")$*
    + Since $Gam prov phi$
    + Assume that by applying the logic rules Barbara and down, one can derive $phi$ from $Gam$ 
  + *$underline("Inductive step")$*
    + By $IH$, one can apply both Barbara and down to derive $phi$
    + Barbara
      #figure(image("assets/image-1.png",width: 10em))
      The Barbara does preserve this propery as it does not which around any terms.
    + Down
      + #figure(image("assets/image.png",width: 12em))
      + Since Down preserves the property of $phi$, that is, it also does not switch around any terms (note that in the image x and y of course will not be a single letter as this would interfere with the property)
    + Because of this, deriving $phi$ from $Gam$ using these rules will find an element in $Gam$ which has this rule.
  ]

#pagebreak()
== 4
For logic $cal(A)$ since one can only use the Barbara rule, $phi$ can only be axioms since the two sets of sentences have no overlap
$ sent(1,1), sent(2,2), sent(3,3) $



== 5
#let fluff = $ent #h(-0.1em)#v(1.2em)^f$
#let nfluff = $ent.not #h(-0.1em)#v(1.2em)^f$
Let $Gam fluff phi$ be true if $(forall p in P): ip(p) != emptyset $

Now let $cal(N)$ be an arbitrary model.

If $cal(N) ent phi$ and $cal(N) nfluff phi$. This means that $exists p in P: ip(p) = emptyset$
Lets now add som elements to the domain such that.
$ (forall p in P): ip(p) != emptyset. $
Since no element have been removed, $cal(N) ent phi$ must still hold as must $cal(N) nfluff phi$

#QED




== 6

(a) The canonical model $model_Gam$ has the property $model_Gam ent phi bi Gam prov phi $ 

Since this logic is both sound and complete, the following is true

$ Gam ent phi imp Gam prov phi "and" Gam prov phi imp Gam ent phi $

Because of this both of the following are true

$ Gam ent phi &imp model_Gam ent phi \ model_Gam ent phi &imp Gam ent phi $

#pagebreak()
== 7
Lets prove this by induction
#pseudo[
  *Proof by induction* 
  - *Goal:* if $phi$ is a tautology, then $phi^R$ is also a tautology.
  + *$underline("Base case")$*
    + Case 1 ($phi = p$)
      + Let $phi$ be a single sentence letter $p$
      + $phi$ cannot be a tautology so this holds.
    + Case 2 ($phi = p imp q$)
      + Let $phi$ be a sentence on the form $p imp q$ where p, and q are single sentenced letters
      + $phi$ can only be a tautology if $p=q$.
      + Then $(p imp q)^R = q imp p$, this is still a tautology. So it holds
  + *$underline("Inductive hypothesis")$*
    + For any subterm $t$ where t can either be a single sentence letter or a sentence on the form $p imp q$. if $t$ is a tautology, then $t^R$ is also a tautology.
    + 
  + *$underline("Inductive step")$*
    + Let $phi$ be an arbitrary sentence consisting of subterms $t_1$ and $t_2$
    +  $ phi = t_1 imp t_2 $
    + If both $t_1$ and $t_2$ are tautologies, then both $t_1 imp t_2$ and $t_2 imp t_1$ are tautologies.
    + Because of this if $phi$ is a tautology, $phi^R$ must be as well.
  ]


== 8
Both $a,b$ must be less than $c,d$
$ min(a,b) < max(c,d) bi (a<c and a<d) or (b<c and b<d) $

