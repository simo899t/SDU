#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 6: ",
  course: "AI504 - Knowledge Represntation",
  date: "7/04/2026"
)
// content starts here


/*
Stephano is not here this week nor next week
We must go to Siddarth

Siddarth is leaving out full syllogistic logic of the corriculum

office hours:
Chech itsLearning
*/

= Relative clauses
The logic of $cal(A)(R C)$ of $"all _"+underline("relative") "clauses")$

Previously, a *signature* was simply a set of nouns, now our signature consists of a set of nouns, *plus* a set of tranitive verbs (a verb must take an object).

- *What are sentences? What are models? What are proofs?*

== Models
A Model (_relative to a particular signature_) consists of a set (_the domain_), and interpretations of both all nouns and all verbs

- $ip(#h(0em))_"nouns"$ is a subset of the domain
- $ip(#h(0em))_"verbs"$ is a binary relation on the domain
so..
$ ip(#h(0em))_"nouns":P -> pow(M), quad ip(#h(0em))_"verbs":R -> pow(M times M) $
Where $P$ is the set of nouns, and $R$ is the set of verbs

so..
$ model=(P,ip(#h(0em))_"nouns":P -> pow(M), quad ip(#h(0em))_"verbs":R -> pow(M times M)) $







== Terms
We want to give meaning to relative clauses so we ignore irrelevant words. $ #strike[those who] "love all "#strike[who]" fear all woodchucks" $
$ "love all (fear all woodchucks)" $

A term is either
- A single noun, or
- an expression ($r$ all $t$), where $r$ is a verb and $t$ is a term

#pseudo[
*Terms*
- i
   + if $t$ is a single noun, then
   - $ip(t) #[is simply] ip(t)_#[noun]$, which comes from the data in $model$
   + 
- ii
   + if $t$ is $term(r,t')$, then 
   + $ ip(t) = {m in M: forall n in ip(t') quad (m,n) in ip(r) } $
]

=== Example

#figure(
   image("assets/image.png",width: 30em),
   caption: [Example 2.4 from: Logic From Language by Lawrence S. Moss]
)
$ ip(#[see all hawks]) = {m in M: forall n in {1,2,3,4,5,6} quad (m,n) in {1}} = {2,3,4,5,7} $

$ ip(#[see all birds]) = {m in M: forall n in {1,2,3,4,5,6} quad (m,n) in {1,6}} = {3,4,5,7} $

$ ip(#[see all turtles]) = {m in M: forall n in {1,2,3,4,5,6} quad (m,n) in {3,5,7}} = {3,5} $

$ ip(#[see all (see all hawks)]) = {m in M: forall n in {2,3,4,5,7} quad (m,n) in {1}} = {3,5} $
== Sentences
A *sentence* is an expression $ sent("term", "term", mid: "are") $
#pagebreak()

== Fundemental Lemma

Work within a fixed model.

For any terms $s$ and $t$ and any ver $r$. if $ip(s) psubset ip(t)$
$ ip(term(r,t)) psubset ip(term(r,s)) $

=== Proof
Suppose that $m$ is an arbitrary element of $M$.

Assume that $m in ip(term(r,t))$ $qquad #[Goal: $m in ip(term(r,s))$]$

This means that $forall n in ip(t) quad (m,n) in ip(r)$ $qquad #[Goal: $forall n in ip(s) quad (m,n) in ip(s)$]$

Now let $n$ be an arbitrary element of $ip(s)$ $qquad #[Goal: $(m,n) in ip(s)$]$

Since $ip(s) psubset ip(t)$, $n in ip(t)$

Now because of $forall n in ip(s) quad (m,n) in ip(s)$ and $n in ip(t)$

We know that $(m,n) in ip(r)$
#QED


== Proof rule
For any model $model$, if $model ent sent(s,t), "then" model ent sent(term(r,t),term(r,s))$

This suggessts a proof rule (ANTI)
#figure(
  grid(
    columns: 2,
    align: horizon, 
    gutter: 1mm, 
    ptree(
      sent($lr((#term("r","t")))$, $lr((#term("r","s")))$),
      r($$, sent("s","t")),
    ),
    [ANTI]
  ),
   caption: [ANTI - proof ruleP]   
)

Also DOWN

This suggessts a proof rule (ANTI)
#figure(
  grid(
    columns: 2,
    align: horizon, 
    gutter: 1mm, 
    ptree(
      sent("x", $lr((#term("r","y")))$),
      r($$, sent("x", $lr((#term("r","z")))$)),
      r($$, sent("y","z")),
      reverse: true
    ),
    [DOWN]
  ),
   caption: [DOWN - proof ruleP]   
)

These two rules are interderivable. (shown in lecture)