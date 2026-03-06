#import "@preview/lovelace:0.3.0": *
#import "@preview/tdtr:0.5.2" : *
#import "@preview/h-graph:0.1.0": *
#let dirgraph(src) = h-graph(src, polar-render)

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
#let abc = enum.with(numbering: "(a)", spacing: 1.5em)
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
#let pred(a) = $accent(#a,\^)$
#let QED = [#h(1fr) $square$]
#let IH = [*_IH_*]
#let f = [#h(1fr)]
#let qquad = $quad quad$
#let qqquad = $quad quad quad$
#let qqqquad = $quad quad quad quad$
#let sign(a) = $"sign"(#a)$
#let psubset = $subset.eq$
#let rang = $chevron.r$
#let lang = $chevron.l$

// --- Calculus notation ---
#let dx = $dif x$
#let px = $partial x$
#let dx() = $dif #x$
#let px() = $partial #x$

// Ordinary derivatives
#let ddx = $dif/(dif x)$                                        // d/dx  (operator)
#let dd(x) = $dif/(dif #x)$                                    // d/d(var)  e.g. dd(t)
#let dv(f, x) = $(dif #f)/(dif #x)$                           // df/dx  e.g. dv(f,x)
#let dvn(f, x, n) = $(dif^#n #f)/(dif #x^#n)$                // dⁿf/dxⁿ  e.g. dvn(f,x,2)

// Partial derivatives
#let ppx = $partial/(partial x)$                               // ∂/∂x  (operator)
#let pp(x) = $partial/(partial #x)$                           // ∂/∂(var)  e.g. pp(y)
#let pv(f, x) = $(partial #f)/(partial #x)$                   // ∂f/∂x  e.g. pv(f,x)
#let pvn(f, x, n) = $(partial^#n #f)/(partial #x^#n)$        // ∂ⁿf/∂xⁿ  e.g. pvn(f,x,2)
#let pvm(f, x, y) = $(partial^2 #f)/(partial #x partial #y)$ // ∂²f/∂x∂y  mixed partial

// Hessian (2×2 and n×n pattern)
#let hess2(f) = $mat(
  (partial^2 #f)/(partial x^2), (partial^2 #f)/(partial x partial y);
  (partial^2 #f)/(partial y partial x), (partial^2 #f)/(partial y^2)
)$
#let hess(f) = $mat(
  (partial^2 #f)/(partial x_1^2), dots.c, (partial^2 #f)/(partial x_1 partial x_n);
  dots.v, dots.down, dots.v;
  (partial^2 #f)/(partial x_n partial x_1), dots.c, (partial^2 #f)/(partial x_n^2)
)$

// Gradient / Laplacian
#let nf(f) = $nabla #f$
#let nnf(f) = $nabla^2 #f$
#let nf = $nabla f$

// pseudocode alias
#let pseudo = pseudocode-list

// symbols
#let phi = $phi.alt$
#let eps = $epsilon$
#let del = $delta$
#let gam = $gamma$
#let cap = $inter$
#let cup = $union$
#let ent = symbol("⊨", ("not", "⊭"))
#let prov = symbol("⊢", ("not", "⊬"))

#let dag = $dagger$

#let tree(body, draw-node: tidy-tree-draws.circle-draw-node, ..args) = tidy-tree-graph(body, draw-node: draw-node, ..args)


#let group-by-pairs(elements) = {
  let lefts = elements
    .enumerate()
    .filter(((index, _)) => calc.rem(index, 2) == 0)
    .map(((_, element)) => element)
  let rights = elements
    .enumerate()
    .filter(((index, _)) => calc.rem(index, 2) == 1)
    .map(((_, element)) => element)
  lefts.zip(rights)
}

#let mycases(..cases, word: none) = {
  let cases = group-by-pairs(cases.pos())
    .map(((value, condition)) => {
      if word != none {
        $#value quad &#word #condition$
      } else {
        $#value quad & #condition$
      }
    })
  math.cases(..cases)
}

// Helper: accepts a string or array of strings, formats as "A · B · C"
#let _fmt-authors(author) = {
  if type(author) == str { author }
  else { author.join(" · ") }
}

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


#let note(
  title: default-title,
  author: default-author,
  course: default-course,
  date: default-date,
  outline: true,
  outline-depth: none,
  ..args,
) = {
  let body = args.pos().at(0, default: [])
  set math.mat(delim: "[", gap: 0.3em)
  set page(margin: (left: 3cm, right: 3cm, top: 3cm, bottom: 3cm))
  align(center,
    stack(
      spacing: 0pt,
      v(1.2cm),
      // Blue top bar + label
      line(length: 100%, stroke: 3pt + rgb("#2c5aa0")),
      v(1.2em),
      text(size: 9.5pt, fill: rgb("#2c5aa0"), tracking: 2.5pt, weight: "bold")[LECTURE NOTES],
      v(2.5cm),
      // Title
      text(size: 30pt, weight: "bold")[#title],
      v(1.3em),
      line(length: 28%, stroke: 0.5pt + rgb("#bbbbbb")),
      v(0.7em),
      text(size: 14pt, fill: rgb("#444444"))[#course],
      // Push to bottom
      v(1fr),
      text(size: 12pt)[#_fmt-authors(author)],
      v(0.3em),
      text(size: 11pt, fill: rgb("#888888"))[#date],
      v(1.8em),
      image("/assets/image-8.png", width: 15em),
      v(1cm),
    )
  )
  pagebreak()
  if outline { std.outline(depth: outline-depth); pagebreak() }
  body
}

#let exercise(
  title: default-title,
  author: default-author,
  course: default-course,
  date: default-date,
  outline: true,
  outline-depth: none,
  ..args,
) = {
  let body = args.pos().at(0, default: [])
  set math.mat(delim: "[", gap: 0.3em)
  set page(margin: (left: 3cm, right: 3cm, top: 3cm, bottom: 3cm))
  align(center,
    stack(
      spacing: 0pt,
      v(1.2cm),
      // Amber top bar + label
      line(length: 100%, stroke: 3pt + rgb("#b7410e")),
      v(1.2em),
      text(size: 9.5pt, fill: rgb("#b7410e"), tracking: 2.5pt, weight: "bold")[EXERCISES],
      v(2.5cm),
      // Title
      text(size: 30pt, weight: "bold")[#title],
      v(1.3em),
      line(length: 28%, stroke: 0.5pt + rgb("#bbbbbb")),
      v(0.7em),
      text(size: 14pt, fill: rgb("#444444"))[#course],
      // Push to bottom
      v(1fr),
      text(size: 12pt)[#_fmt-authors(author)],
      v(0.3em),
      text(size: 11pt, fill: rgb("#888888"))[#date],
      v(1.8em),
      image("/assets/image-8.png", width: 15em),
      v(1cm),
    )
  )
  pagebreak()
  if outline { std.outline(depth: outline-depth); pagebreak() }
  body
}

#let assignment(
  title: default-title,
  author: default-author,
  course: default-course,
  date: default-date,
  outline: true,
  outline-depth: none,
  ..args,
) = {
  let body = args.pos().at(0, default: [])
  set math.mat(delim: "[", gap: 0.3em)
  set page(margin: (left: 3cm, right: 3cm, top: 3cm, bottom: 3cm))
  align(center,
    stack(
      spacing: 0pt,
      v(1.2cm),
      // Amber top bar + label
      line(length: 100%, stroke: 3pt + rgb("#b7410e")),
      v(1.2em),
      text(size: 9.5pt, fill: rgb("#621e00"), tracking: 2.5pt, weight: "bold")[ASSIGNMENTS],
      v(2.5cm),
      // Title
      text(size: 30pt, weight: "bold")[#title],
      v(1.3em),
      line(length: 28%, stroke: 0.5pt + rgb("#bbbbbb")),
      v(0.7em),
      text(size: 14pt, fill: rgb("#444444"))[#course],
      // Push to bottom
      v(1fr),
      text(size: 12pt)[#_fmt-authors(author)],
      v(1.8em),
      text(size: 11pt, fill: rgb("#888888"))[#date],
      v(1.8em),
      image("/assets/image-8.png", width: 15em),
      v(1cm),
    )
  )
  pagebreak()
  if outline { std.outline(depth: outline-depth); pagebreak() }
  body
}

#let project(
  title: default-title,
  subtitle: none,
  author: default-author,
  course: default-course,
  date: default-date,
  group: none,
  supervisor: none,
  university: "University of Southern Denmark",
  outline: true,
  outline-depth: none,
  ..args,
) = {
  let body = args.pos().at(0, default: [])
  set math.mat(delim: "[", gap: 0.3em)
  set page(margin: (left: 3cm, right: 3cm, top: 3cm, bottom: 3cm))
  align(center,
    stack(
      spacing: 0pt,
      // Top: university name
      v(1.5cm),
      text(size: 13pt, fill: rgb("#555555"))[#university],
      v(0.6em),
      line(length: 60%, stroke: 0.5pt + rgb("#aaaaaa")),
      v(3cm),

      // Title block
      text(size: 28pt, weight: "bold")[#title],
      if subtitle != none {
        stack(
          v(1.5em),
          text(size: 15pt, fill: rgb("#444444"), style: "italic")[#subtitle],
        )
      },
      v(1em),
      line(length: 40%, stroke: 0.5pt + rgb("#aaaaaa")),
      v(1.3em),
      text(size: 14pt, fill: rgb("#333333"))[#course],

      // Fill remaining space
      v(1fr),

      // Authors — 3/2 centered grid layout
      {
        let author-arr = if type(author) == str {
          ((name: author),)
        } else if type(author) == array and author.len() > 0 and type(author.at(0)) == str {
          author.map(n => (name: n))
        } else if type(author) == array {
          author
        } else { ((name: str(author)),) }

        let render-author(a) = align(center, stack(
          spacing: 0.25em,
          text(weight: "bold", size: 11pt)[#a.at("name", default: "")],
          if a.at("email", default: "") != "" {
            text(size: 8.5pt, fill: rgb("#4a90d9"))[#a.at("email", default: "")]
          },
        ))

        let per-row = 3
        let row-starts = range(0, author-arr.len(), step: per-row)
        stack(spacing: 1.5em,
          ..row-starts.map(i => {
            let row = author-arr.slice(i, calc.min(i + per-row, author-arr.len()))
            align(center,
              box(width: (100% * row.len() / per-row),
                grid(
                  columns: (1fr,) * row.len(),
                  column-gutter: 2em,
                  ..row.map(render-author),
                )
              )
            )
          })
        )
      },
      v(1.5em),

      // Metadata box (group / supervisor / date)
      block(
        width: 60%,
        stroke: (top: 0.5pt + rgb("#aaaaaa"), bottom: 0.5pt + rgb("#aaaaaa")),
        inset: (top: 1em, bottom: 1em),
        align(left,
          stack(
            spacing: 0.5em,
            if group != none {
              grid(
                columns: (4cm, 1fr),
                text(fill: rgb("#777777"))[*Group:*],
                text()[#group],
              )
            },
            if supervisor != none {
              grid(
                columns: (4cm, 1fr),
                text(fill: rgb("#777777"))[*Supervisor:*],
                text()[#supervisor],
              )
            },
            grid(
              columns: (4cm, 1fr),
              text(fill: rgb("#777777"))[*Date:*],
              text()[#date],
            ),
          )
        )
      ),
      v(1.8em),
      image("/assets/image-8.png", width: 15em),
      v(1cm),
    )
  )
  pagebreak()
  if outline { std.outline(depth: outline-depth); pagebreak() }
  body
}

#let exam(
  title: default-title,
  subtitle: none,
  author: default-author,
  course: default-course,
  date: default-date,
  student-id: none,
  username: none,
  student-number: none,
  duration: none,
  allowed-aids: none,
  university: "University of Southern Denmark",
  outline: true,
  outline-depth: none,
  ..args,
) = {
  let body = args.pos().at(0, default: [])
  let author-name = if type(author) == str { author }
    else if type(author) == array and author.len() > 0 {
      if type(author.at(0)) == str { author.at(0) }
      else { author.at(0).at("name", default: "") }
    } else { "" }
  set math.mat(delim: "[", gap: 0.3em)
  set page(
    margin: (left: 3cm, right: 3cm, top: 3cm, bottom: 3cm),
    header: if username != none or student-number != none {
      set text(size: 9pt, fill: rgb("#555555"))
      grid(
        columns: (1fr, 1fr, 1fr),
        align(left)[#author-name],
        align(center)[#if username != none { username }],
        align(right)[#if student-number != none { student-number }],
      )
    },
  )
  align(center,
    stack(
      spacing: 0pt,
      // Top: university name
      v(1.5cm),
      text(size: 13pt, fill: rgb("#555555"))[#university],
      v(0.6em),
      line(length: 60%, stroke: 0.5pt + rgb("#aaaaaa")),
      v(0.5cm),

      // Green label
      text(size: 9.5pt, fill: rgb("#1a6b3c"), tracking: 2.5pt, weight: "bold")[EXAM],
      v(4.5cm),

      // Title block
      text(size: 28pt, weight: "bold")[#title],
      if subtitle != none {
        stack(
          v(1.5em),
          text(size: 15pt, fill: rgb("#444444"), style: "italic")[#subtitle],
        )
      },
      v(1em),
      line(length: 40%, stroke: 0.5pt + rgb("#aaaaaa")),
      v(1.3em),
      text(size: 14pt, fill: rgb("#333333"))[#course],

      // Fill remaining space
      v(1fr),

      // Authors
      {
        let author-arr = if type(author) == str {
          ((name: author),)
        } else if type(author) == array and author.len() > 0 and type(author.at(0)) == str {
          author.map(n => (name: n))
        } else if type(author) == array {
          author
        } else { ((name: str(author)),) }

        let render-author(a) = align(center, stack(
          spacing: 0.25em,
          text(weight: "bold", size: 11pt)[#a.at("name", default: "")],
          if a.at("id", default: "") != "" {
            text(size: 9pt, fill: rgb("#555555"))[#a.at("id", default: "")]
          },
        ))

        let per-row = 3
        let row-starts = range(0, author-arr.len(), step: per-row)
        stack(spacing: 1.5em,
          ..row-starts.map(i => {
            let row = author-arr.slice(i, calc.min(i + per-row, author-arr.len()))
            align(center,
              box(width: (100% * row.len() / per-row),
                grid(
                  columns: (1fr,) * row.len(),
                  column-gutter: 2em,
                  ..row.map(render-author),
                )
              )
            )
          })
        )
      },
      v(1.5em),

      // Metadata box
      block(
        width: 60%,
        stroke: (top: 0.5pt + rgb("#aaaaaa"), bottom: 0.5pt + rgb("#aaaaaa")),
        inset: (top: 1em, bottom: 1em),
        align(left,
          stack(
            spacing: 0.5em,
            if duration != none {
              grid(
                columns: (4cm, 1fr),
                text(fill: rgb("#777777"))[*Duration:*],
                text()[#duration],
              )
            },
            if allowed-aids != none {
              grid(
                columns: (4cm, 1fr),
                text(fill: rgb("#777777"))[*Allowed aids:*],
                text()[#allowed-aids],
              )
            },
            grid(
              columns: (4cm, 1fr),
              text(fill: rgb("#777777"))[*Date:*],
              text()[#date],
            ),
          )
        )
      ),
      v(1.8em),
      image("/assets/image-8.png", width: 15em),
      v(1cm),
    )
  )
  pagebreak()
  if outline { std.outline(depth: outline-depth); pagebreak() }
  body
}



#let chi(
  title: default-title,
  authors: (),       // string or array of dicts: (name:, institution:, city:, country:, email:)
  abstract: [],
  keywords: (),
  ccs: none,
  date: default-date,
  outline: false,
  ..args,
) = {
  let body = args.pos().at(0, default: [])
  set math.mat(delim: "[", gap: 0.3em)
  set page(paper: "us-letter", margin: (x: 1.9cm, y: 2.3cm))
  set text(size: 9.5pt)

  // Title
  v(0.5cm)
  align(center, text(size: 18pt, weight: "bold")[#title])
  v(1.5em)

  // Authors — accepts either a string or an array of dicts
  let authors-arr = if type(authors) == str {
    ((name: authors),)
  } else if authors.len() > 0 and type(authors.at(0)) == str {
    authors.map(n => (name: n))
  } else {
    authors
  }
  if authors-arr.len() > 0 {
    let render-author(a) = align(center, stack(
      spacing: 0.3em,
      text(weight: "bold", size: 10.5pt)[#a.at("name", default: "")],
      if a.at("institution", default: "") != "" { text(size: 9pt)[#a.at("institution", default: "")] },
      if a.at("city", default: "") != "" or "country" in a {
        text(size: 9pt)[#a.at("city", default: "")#if "country" in a [, #a.country]]
      },
      if a.at("email", default: "") != "" {
        text(size: 9pt, fill: rgb("#0055aa"))[#a.at("email", default: "")]
      },
    ))

    // CHI layout: up to 3 per row, partial last row centered
    let n = authors-arr.len()
    let row-starts = range(0, n, step: 3)
    for i in row-starts {
      let row = authors-arr.slice(i, calc.min(i + 3, n))
      align(center,
        box(width: (100% * row.len() / 3),
          grid(columns: (1fr,) * row.len(), column-gutter: 2em,
            ..row.map(render-author))
        )
      )
      if i + 3 < n { v(1.5em) }
    }

    v(1.8em)
    align(center, text(size: 9pt, fill: rgb("#888888"))[#date])
    v(1em)
  }

  // Abstract + CCS + Keywords — only rendered if any are provided
  let has-meta = abstract != [] or keywords != () or ccs != none
  if has-meta {
    line(length: 100%, stroke: 0.5pt + rgb("#888888"))
    v(1em)
    columns(2, gutter: 1.5em, [
      #if abstract != [] {
        text(weight: "bold", size: 8.5pt, tracking: 0.8pt)[ABSTRACT]
        v(0.4em)
        abstract
      }
      #if ccs != none {
        v(0.8em)
        text(weight: "bold", size: 8.5pt, tracking: 0.8pt)[CCS CONCEPTS]
        v(0.4em)
        ccs
      }
      #if keywords != () {
        v(0.8em)
        text(weight: "bold", size: 8.5pt, tracking: 0.8pt)[KEYWORDS]
        v(0.4em)
        keywords.join("; ")
      }
    ])
  }

  line(length: 100%, stroke: 0.5pt + rgb("#888888"))
  v(1em)
  if outline { pagebreak(); std.outline(); pagebreak() }
  body
}

/*
=============================================================
TEMPLATE CHEATSHEET — copy the block you need into a new file
=============================================================

── NOTE ──────────────────────────────────────────────────────
#import "../../temp.typ": *
#show: note.with(
  title:         "Lecture Notes",
  course:        "DM000 — Course Name",
  author:        "Simon Holm",
  date:          "February 2026",
  outline:       true,          // set false to skip TOC
  outline-depth: 2,             // none = unlimited depth
)

= First Section
Content goes here.

── EXERCISE ──────────────────────────────────────────────────
#import "../../temp.typ": *
#show: exercise.with(
  title:         "Exercises 1",
  course:        "DM000 — Course Name",
  author:        "Simon Holm",
  date:          "February 2026",
  outline:       true,
  outline-depth: 2,
)

= Exercise 1
// Content goes here.

── ASSIGNMENT ────────────────────────────────────────────────
#import "../../temp.typ": *
#show: assignment.with(
  title:         "Assignment 1",
  course:        "DM000 — Course Name",
  author:        "Simon Holm",
  date:          "February 2026",
  outline:       true,
  outline-depth: 2,
)

= Problem 1
// Content goes here.

── EXAM ──────────────────────────────────────────────────────
#import "../../temp.typ": *
#show: exam.with(
  title:         "Written Exam",
  subtitle:      "Re-exam",                    // optional
  course:        "DM000 — Course Name",
  author:        "Simon Holm",
  date:          "June 2026",
  student-id:    "sihol24",                    // optional
  username:      "sihol24",                    // optional — shown in page header
  student-number: "215751682",                 // optional — shown in page header
  duration:      "4 hours",                    // optional
  allowed-aids:  "All written materials",      // optional
  university:    "University of Southern Denmark",
  outline:       false,
)

= Problem 1
// Content goes here.

── EXAM (group / multiple students) ─────────────────────────
#import "../../temp.typ": *
#show: exam.with(
  title:        "Written Exam",
  course:       "DM000 — Course Name",
  author: (
    (name: "Simon Holm", id: "sihol24"),
    (name: "Firstname Lastname",   id: "jado42"),
  ),
  date:         "June 2026",
  duration:     "4 hours",
  allowed-aids: "None",
)

= Problem 1
// Content goes here.

── PROJECT ───────────────────────────────────────────────────
#import "../../temp.typ": *
#show: project.with(
  title:         "Project Title",
  subtitle:      "Optional subtitle",          // optional
  course:        "DM000 — Course Name",
  author:        "Simon Holm",                 // or array of dicts below
  date:          "February 2026",
  group:         "Group 4",                    // optional
  supervisor:    "Prof. Firstname Lastname",             // optional
  university:    "University of Southern Denmark",
  outline:       true,
  outline-depth: 2,
)

= Introduction
// Content goes here.

── PROJECT (multiple authors with email) ─────────────────────
#import "../../temp.typ": *
#show: project.with(
  title:  "Project Title",
  course: "DM000 — Course Name",
  author: (
    (name: "Simon Holm", email: "sihol24@student.sdu.dk"),
    (name: "Firstname Lastname",   email: "jado@student.sdu.dk"),
  ),
  date:       "February 2026",
  group:      "Group 4",
  supervisor: "Prof. Jane Doe",
)

= Introduction
// Content goes here.

── CHI PAPER ─────────────────────────────────────────────────
#import "../../temp.typ": *
#show: chi.with(
  title: "Paper Title",
  authors: (
    (name: "Simon Holm", institution: "University of Southern Denmark", city: "Odense", country: "Denmark", email: "sihol24@student.sdu.dk"),
    (name: "Author Two", institution: "University of Southern Denmark", city: "Odense", country: "Denmark", email: "two@student.sdu.dk"),
  ),
  abstract: [Your abstract text here.],
  keywords: ("keyword one", "keyword two", "keyword three"),
  ccs:      [\u{2192} Human-centered computing \u{2192} HCI theory, concepts and models], // optional
  date:     "March 2026",
  outline:  false,
)
#set page(columns: 2)

= Introduction
// Content goes here.

── MAPPING DIAGRAM (inline, no import needed) ────────────────
#mapdiag(
  title:        $f: A -> B$,           // optional label above diagram
  a:            $A$,                   // left set label  (default $A$)
  b:            $B$,                   // right set label (default $B$)
  a-elems:      ($1$, $2$, $3$),
  b-elems:      ($a$, $b$, $c$),
  arrow-color:  black,                 // default arrow colour
  arrows: (
    (0, 0),                            // plain arrow
    (1, 2, red),                       // coloured arrow
    (2, 1, blue, $g$),                 // coloured + label
  ),
)
*/
