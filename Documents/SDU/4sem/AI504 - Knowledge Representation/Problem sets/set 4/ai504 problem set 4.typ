#import "../../../../temp/temp.typ": *

#assignment(
  title: "Problem set 4",
  course: "AI504 — Knowledge Representation",
  author: ("Simon Holm", "Johannes Rothe", "Shuagib Ibrahim", "Anne Sofie Høj"),
  date: "March, 2026",
  outline-depth: 1
)

#let see = `see`
#let hawks = `hawks`
#let turtles = `turtles`
#let birds = `birds`



#figure(
  ptree(
  $sent((term(see,(term(see,hawks)))), t)$,
  ptree($sent(t,(term(see,birds)))$, $term(hawks,birds)$, conclusion: $sent(t,(term(see,hawks)))$, rule: [DOWN]),
  conclusion: $sent((term(see,(term(see,hawks)))), (term(see,hawks)))$,
  rule: [BARBARA],
  
),  caption: [proof tree]
, gap: 2em
)