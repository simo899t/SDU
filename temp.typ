// global Typst template
//   #import "../../temp.typ": *
// Then override variables like title, author, course, date as needed.
#import "@preview/lovelace:0.3.0": *

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

// just nice
#let evaluated(expr, size: 100%) = $lr(#expr|, size: #size)$


// shortcuts
#let redmath(x) = text(fill: red, $#x$)
#let bluemath(x) = text(fill: blue, $#x$)
#let greenmath(x) = text(fill: green, $#x$)
#let int(a,b,c) = $integral_(#a)^(#b) #c$ 
#let prod(a,b,c) = $product_(#a)^(#b) #c$ 
#let summ(a,b,c) = $sum_(#a)^(#b) #c$ 
#let limm(a) = $lim_(#a)$
#let nf(..args) = if args.pos().len() == 0 { $nabla f$ } else { $nabla f(#args.pos().at(0))$ }
#let nnf(..args) = if args.pos().len() == 0 { $nabla^2 f$ } else { $nabla^2 f(#args.pos().at(0))$ }
#let qquad = $quad quad$
#let qqquad = $quad quad quad$
#let qqqquad = $quad quad quad quad$

// pseudocode alias
#let pseudo = pseudocode-list

// symbols
#let phi = $phi.alt$
#let eps = $epsilon$

#let code(content) = block(
  fill: rgb("#282c34"),
  stroke: 1pt + rgb("#3e4452"),
  inset: (left: 16pt, right: 16pt, top: 12pt, bottom: 12pt),
  radius: 4pt,
  [
    #set par(leading: 0.75em)
    #text(
      fill: rgb("#b9c3d5"),
      font: "JetBrains Mono",
      size: 10pt,
      weight: "regular",
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

// --- Document metadata (override in your file) ---


#let default-title = "Untitled Document"
#let default-course= "SDU"
#let default-author = "Simon Holm"
#let default-date = "16/12/2002"


#let sdu-title(
  title: default-title,
  author: default-author,
  course: default-course,
  date: default-date
) = align(center,
  block(
    inset: (top: 6cm),
    [
      #text(size: 24pt, weight: "bold")[#title]
      #v(1.5em)
      #text(size: 18pt, weight: "medium", fill: gray)[#course]
      #v(1.5em)
      #text(size: 14pt, weight: "medium")[#author]
      #v(0.5em)
      #text(size: 12pt, fill: gray)[#date]
    ]
  )
)

#sdu-title(
  title: title,
  author: author,
  date: date
)


// --- Usage in a new Typst file ---
// #import "../../temp.typ": *
// #set title = "My Note Title"
// #set course = "Course name/code"
// #set author = "Name"
// #set date = "Date"
// ...
