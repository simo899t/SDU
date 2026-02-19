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


//----------------------
#let redmath(x) = text(fill: red, $#x$)
#let bluemath(x) = text(fill: blue, $#x$)
#let greenmath(x) = text(fill: green, $#x$)

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
#align(center)[
  #text(size: 18pt, weight: "bold")[Exercises 4]
  
  #v(1em)
  
  Simon Holm \
    AI505: Optimization \
    Teacher: Marco Chiarandini
]

#pagebreak()

// Table of contents
#outline(indent: auto)

#pagebreak()

= Problems when using large inputs
Assume a standard mobile phone image with 8 Mega pixels
- Each pixel consists of 3 color channels, i.e., we end up with 24 million input "dimensions"
- Now we want to detect a face. We build a fully-connected layer where each node represents the presence of a certain feature, for instance:
- One for eyes
- One for hair
- One for lips

= Convolutional Networks
Neural networks that use convolution in place of general matrix multiplication in *at least one* of their layers

Convolution can be viewed as multiplication by a matrix
$ #image("/assets/image.png", width: 25em) $

$ #image("/assets/image-1.png", width: 30em) $

#pagebreak()

== 1-D Convolution
- Continuous (not really used for NN, this is more general)
$ (f times g)(t) = integral_(-oo)^(oo) f(tau)g(t-tau) dif t $
- Descrete
$ s(t) = (x times w)(t) sum_(a=-oo)^oo x(a)w(t-a)  $

== 2-D Convolution
- Descrete
$ S(i,j) = (I times K)(i,j) = sum_m sum_n I(m,n)K(i-m,j-n) $
and ofc
$ S(i,j) = (K times I)(i,j) = sum_m sum_n I(i-m,j-n)K(m,n) $

== 2-D Cross-Correlation
$ S(i,j) = (K times I)(i,j) = sum_m sum_n I(i+m,j+n)K(m,n) $
- Both referred to as convolution, whether kernel is flipped or not
- The learning algorithm will learn appropriate values of the kernel in the appropriate place

= Pooling

== Max Pooling
$ #image("/assets/image-2.png") $
This throws away possible crucial information, but it can optimize dimentionality quite a bit.


= Typical stages of CNN 
- Stage 1 (*Convolution*)
  - Perform several convolutions in parallel to produce a set of linear activations
- Stage 2 (Detector):
  - Each linear activation is run through a nonlinear activation
function (e.g. ReLU)
- Stage 3 (Pooling):
  - Use a pooling function to modify output of the layer further
$ #image("/assets/image-3.png") $







