// --- Macro to replace $phi$ with $phi.alt$ in math expressions ---

// SDU global Typst template
// Place this file at SDU/temp.typ and import it in any Typst file:
//   #import "../../temp.typ": *
// Then override variables like title, author, date as needed.

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

#let redmath(x) = text(fill: red, $#x$)
#let bluemath(x) = text(fill: blue, $#x$)
#let greenmath(x) = text(fill: green, $#x$)
#let evaluated(expr, size: 100%) = $lr(#expr|, size: #size)$
#let int(a,b,c) = $integral_(#a)^(#b) #c$ 
#let prod(a,b,c) = $product_(#a)^(#b) #c$ 
#let sum(a,b,c) = $sum_(#a)^(#b) #c$ 
#let lim(a) = $lim_(#a)$ 
#let phi = $phi.alt$


#let code(content) = block(
  fill: gradient.linear(
    rgb("#23272e"), 
    rgb("#2d3340"), 
    angle: 120deg
  ),
  stroke: (
    left: 4pt + rgb("#00b4d8"),
    rest: 1pt + rgb("#22223b")
  ),
  shadow: (x: 2pt, y: 2pt, blur: 6pt, color: rgb("#00000033")),
  inset: (left: 18pt, right: 18pt, top: 14pt, bottom: 14pt),
  radius: 10pt,
  [
    #text(
      fill: rgb("#caf0f8"), 
      font: "JetBrains Mono", 
      size: 10pt,
      weight: "medium",
      line-spacing: 120%
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
#let default-author = "Simon Holm"
#let default-date = "a"

#let default-title = "Untitled Document"
#let default-author = "Simon Holm"
#let default-date = "16/12/2002"

#let sdu-title(
  title: default-title,
  author: default-author,
  date: default-date
) = align(center,
  block(
    inset: (top: 6cm),
    [
      #text(size: 24pt, weight: "bold")[#title]
      #v(1.5em)
      #text(size: 14pt, weight: "medium")[#author]
      #v(0.5em)
      #text(size: 12pt, fill: gray)[#date]
    ]
  )
)


// Only show the title page if this file is the main document (not imported as a module)
// Typst does not have a built-in __main__ check, but you can document that users should not call sdu-title themselves unless they want to override.

// --- Usage in a new Typst file ---
// #import "../../temp.typ": *
// #set title = "My Note Title"
// #set author = "Simon Holm"
// #set date = "19. februar 2026"
// ...


#pagebreak()
