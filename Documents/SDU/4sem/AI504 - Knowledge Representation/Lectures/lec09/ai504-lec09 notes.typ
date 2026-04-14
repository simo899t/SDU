#import "../../../../temp/temp.typ": *
#show: note.with(
  title: "Lecture 6: ",
  course: "AI504 - Knowledge Represntation",
  date: "7/04/2026"
)
// content starts here


/*
Stephano is still not here this week
We must go to Siddarth

Come by his office or email him
*/

= $cal(A(R C))$: logic of "$term(a,q)$"
Recal that this was enhanced by #u([relative clauses]) (terms and verbs)

Today we will do soundness and completeness


= Systems
We defined two proof systems

#definition(title: $A(R C)$, [
  AXIOMS $+$ BARBARA $+$ DOWN
  #figure(
  grid(
    columns: 2,
    align: horizon,
    gutter: 1mm,
    ptree(
      sent("x", $lr((#term("r","z")))$),
      sent("x", "y"),
      conclusion: sent("x", $lr((#term("r","y")))$),
      rule: [DOWN],
    ),
    [DOWN]
  ),
   caption: [Proof tree 1]
)<DOWNdef>
])
#definition(title: $A(R C)'$, [
  AXIOMS $+$ BARBARA $+$ ANTI
  #figure(
  grid(
    columns: 2,
    align: horizon,
    gutter: 1mm,
    ptree(
      sent("x", "y"),
      conclusion: sent($lr((#term("r","y")))$, $lr((#term("r","x")))$),
      rule: [ANTI],
    ),
    [ANTI]
  ),
   caption: [Proof tree 1]
)<ANTIdef>
])
Note that both DOWN and ANTI, preserve semantic validity

== Soundness
If $Gam prov phi$, them $Gam ent phi$



== Completeness
If $Gam ent phi$, then $Gam prov phi$