#import "temp/temp.typ": *

#show: assignment.with(
  title: "Problem set 6",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm", "Johannes Rothe", "Shuagib Ibrahim", "Anne Sofie Høj", "Daniel Nissen"),
  date: "March, 2026",
  outline-depth: 1
)
#set par(
  justify: true,
  leading: 0.52em,
)

#let see = `see`
#let love = `love`
#let dogs = `dogs`
#let birds = `birds`


= Problem 1
We are going to describe a translation from the syntax of $cal(A(R C))$ to the syntax of propositional logic.
First of all, if $(P, R)$ is a signature of $cal(A(R C))$ then just map it to the propositional logic signature $P cup R$. (In other words, either a noun or a verb in my source signature can be used as a sentence letter in my target signature.)

Now map terms $t$ of $cal(A(R C))$ to formulas $t^*$ of propositional logic as follows. Each noun $p$ becomes the propositional formula $p$. Then “$term(r,(dots))$" becomes “$(dots) -> r$,” so that, e.g., the term see $term(love,dogs)$ becomes $(dogs → love) -> see$. In the latter formula $see$, $love$, $dogs$ are just boolean-valued sentence letters; we have forgotten the noun/verb distinction.

Finally, the translation of the sentence “$term(t,s)$” is $t^* -> s^*$, where $t^*$ and $s^*$ is the mapping on terms defined above. (Call this map $phi |-> phi^dag$, reusing the symbol $*$.) So, for example
$ (sent((term(see, term(love,dogs))), (term(love,birds))))^* $

+ Suppose that $Gam$ is a set of sentences and $phi$ is a sentence in $cal(A(R C))$,  and suppose that $Gam ent phi$. Prove that $Gam^* ent phi^*$, as a formulas of propositional logic.
+ Show that the converse is not true. In other words, come up with a concrete sentences $Gam, phi$ of $cal(A(R C))$ such that $Gam^* ent phi^*$ in propositional logic but $Gam ent.not phi$ in $cal(A(R C))$.

== Solution


= Problem 2
If $phi$ is a sentence in propositional logic, let $phid$ be obtained from $phi$ by negating all of the sentence letters. For example,

$ ((p or (q imp r)) bi not (r and p))^dag = (not p or (not q imp not r)) bi not (not r and not p) $

+ For any satisfiable propositional formula $phi$, must $phid$ also be satisfiable? Either prove or give a counterexample.

+ Can you find a formula $phi$ such that $phi$ is neither a tautology nor a contradiction and $phi$ and $phid$ are logically equivalent? Find one or prove that none exists.

== Solution


= Problem 3
Work in the signature that has sentence letters $p_(i,j)$ for $0 <= i, j <= 9$ (one hundred letters in all). Consider the formula $Phi$ which is the conjunction of all of the following clauses:

- $p_(0,9) and p_(9,0)$,
- $p_(i,j) imp p_(i-1,j)$ for each $1<=i<=9$ and $0<=j<=9$, and
- $p_(i,j) imp p_(i,j-1)$ for each $0<=i<=9$ and $1<=j<=9$.

How many models satisfy $Phi$? (_Hint_. If this seems too daunting, replace 9 with something smaller.)

== Solution

