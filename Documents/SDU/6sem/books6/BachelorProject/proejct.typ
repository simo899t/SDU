#import "@preview/plotsy-3d:0.2.1": plot-3d-surface
// Document setup
#set page(
  paper: "us-letter",
  margin: (left: 3cm, right: 3cm, top: 2cm, bottom: 2cm),
)
#set text(
  font: "Times New Roman",
  size: 11pt,
  lang: "en",
)

#set heading(numbering: "1.")
#set enum(numbering: "(a)")
#set math.equation(numbering: none)
#set math.mat(delim: "[", gap: 0.3em)
#set math.vec(delim: "[", gap: 0.3em)


//----------------------
#let redmath(x) = text(fill: red, $#x$)
#let bluemath(x) = text(fill: blue, $#x$)
#let greenmath(x) = text(fill: green, $#x$)
#let small(x) = text(size: 8pt, $#x$)
#let big(x) = text(size: 16pt, $#x$)
#let large(x) = text(size: 20pt, $#x$)

#let evaluated(expr, size: 100%) = $lr(#expr|, size: #size)$

// Custom box functions for easy use
#let code(content) = block(
  fill: gradient.linear(
    rgb("#232528"), 
    rgb("#242127"), 
    angle: 45deg
  ),
  stroke: (
    left: 3pt + rgb("#151515"),
    rest: 0.5pt + rgb("#151515")
  ),
  inset: (left: 14pt, right: 14pt, top: 12pt, bottom: 12pt),
  radius: 6pt,
  [
    #text(
      fill: rgb("#e8eaed"), 
      font: "Monaco", 
      size: 9.5pt,
      weight: "medium"
    )[#content]
  ]
)

#let theorem(title: "Theorem", content) = block(
  fill: gradient.linear(
    rgb("#fafbfc"), 
    rgb("#f1f3f4"), 
    angle: 135deg
  ),
  stroke: (
    left: 3pt + rgb("#2c5aa0"),
    rest: 0.5pt + rgb("#e1e5e9")
  ),
  inset: (left: 18pt, right: 14pt, top: 14pt, bottom: 14pt),
  radius: 8pt,
  [
    #text(weight: "bold", fill: rgb("#1a365d"), size: 12.5pt)[#title]
    #v(0.5em)
    #text(fill: rgb("#2d3748"), size: 10.5pt)[#content]
  ]
)

#let definition(title: "Definition", content) = block(
  fill: gradient.linear(
    rgb("#fffef7"), 
    rgb("#fef9e7"), 
    angle: 135deg
  ),
  stroke: (
    left: 3pt + rgb("#d69e2e"),
    rest: 0.5pt + rgb("#f7d794")
  ),
  inset: (left: 18pt, right: 14pt, top: 14pt, bottom: 14pt),
  radius: 8pt,
  [
    #text(weight: "bold", fill: rgb("#744210"), size: 12.5pt)[#title]
    #v(0.5em)
    #text(fill: rgb("#553c0f"), size: 10.5pt)[#content]
  ]
)

// Title page
// Custom front page for Bachelor's Project Thesis
// Custom front page for Bachelor's Project Thesis
#pagebreak()
#align(center)[
  #text(size: 32pt, weight: "bold", fill: rgb("#1a365d"))[Title of Thesis]
  #v(1em)
  #text(size: 18pt, weight: "medium")[Bachelor's Project]
  #v(1em)
  #text(size: 14pt)[Department of Mathematics and Computer Science]
  #v(0.5em)
  #text(size: 13pt)[Faculty of Science]
  #v(2em)
  #text(size: 13pt)[Author: Simon Holm]
  #v(0.5em)
  #text(size: 13pt)[Supervisor: insert name]
  #v(0.5em)
  #text(size: 13pt)[Date: insert date]
  #v(2em)
  #text(size: 12pt, fill: rgb("#888"))[Bachelor of Science in Artificial Intelligence]
  #v(20em)
  #image("assets/IMADA_en.png", width: 6cm)
]
#pagebreak()

// Table of contents
#outline(indent: auto)

#pagebreak()

= problem 1


